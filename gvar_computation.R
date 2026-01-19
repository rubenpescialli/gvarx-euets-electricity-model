#### Libraries ####

library(readxl)
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(scales)



#### Data import and foreign variables calculation ####

file_path <- "data/processed/final_data_transformed.xlsx"

elec_prices   <- read_excel(file_path, sheet = "Electricity prices")
wind_gen      <- read_excel(file_path, sheet = "Wind generation")
solar_gen     <- read_excel(file_path, sheet = "Solar generation")
temp_index    <- read_excel(file_path, sheet = "Temperature index")
precip_index  <- read_excel(file_path, sheet = "Precipitation index")
common_inputs <- read_excel(file_path, sheet = "Common inputs")
trans_matrix  <- read_excel(file_path, sheet = "TCs - columns sum to 1")

country_codes <- toupper(trans_matrix[[1]])
W <- as.matrix(trans_matrix[, -1])
colnames(W) <- toupper(colnames(trans_matrix)[-1])
rownames(W) <- country_codes



#### Outliers management ####

lower_q <- 0.05
upper_q <- 0.95

#elec_prices
for(country in country_codes) {
  col_name <- paste0("d_l_elec_", country)
  x <- elec_prices[[col_name]]
  lo <- quantile(x, probs = lower_q, na.rm = TRUE)
  hi <- quantile(x, probs = upper_q, na.rm = TRUE)
  elec_prices[[col_name]] <- pmin(pmax(x, lo), hi)    # So outliers have already been removed from elec_prices.
}

# EUA
eua_lo <- quantile(common_inputs$d_l_EUA, probs = lower_q, na.rm = TRUE)
eua_hi <- quantile(common_inputs$d_l_EUA, probs = upper_q, na.rm = TRUE)

common_inputs$d_l_EUA <- pmin(
  pmax(common_inputs$d_l_EUA, eua_lo),
  eua_hi
)

d_l_elec_x <- data.frame(matrix(NA, nrow = nrow(elec_prices), ncol = length(country_codes)))
colnames(d_l_elec_x) <- paste0("d_l_elec_", country_codes, "_x")

# for-loop to compute foreign variables.
for (country in country_codes) {
  i <- which(country_codes == country)
  w_vec <- W[, i]
  w_vec[i] <- 0                                     # Weight of that same country = 0.
  cols_all <- paste0("d_l_elec_", country_codes)    # Extracts columns d_l_elec for all countries (in the same order as of country_codes).
  data_all <- elec_prices[, cols_all]
  weighted_mean <- as.matrix(data_all) %*% w_vec                        # Calculates the weighted average for each row.
  d_l_elec_x[[paste0("d_l_elec_", country, "_x")]] <- weighted_mean     # I save the result in the dataframe.
}

# for-loop to calculate data_XX (XX = country code) for each country.
# data_XX contains the target, the foreign, and all the exogenous (country-specific and global) for country XX.
for (country in country_codes) {
  data_name <- paste0("data_", country)
  suffix <- paste0("_", country)
  temp_data <- data.frame(
    elec_prices[[paste0("d_l_elec_", country)]],
    d_l_elec_x[[paste0("d_l_elec_", country, "_x")]],
    wind_gen[[paste0("d_l_wind_", country)]],
    solar_gen[[paste0("d_l_solar_", country)]],
    temp_index[[paste0("d_", country, "_t2m")]],
    precip_index[[paste0("d_", country, "_tp")]]
  )
  colnames(temp_data) <- c(
    paste0("d_l_elec", suffix),
    paste0("d_l_elec_x", suffix),
    paste0("d_l_wind", suffix),
    paste0("d_l_solar", suffix),
    paste0("d_temp", suffix),
    paste0("d_precip", suffix)
  )
  temp_data <- cbind(temp_data, common_inputs)
  assign(data_name, temp_data)
}

rm(data_all, d_l_elec_x, temp_data, trans_matrix, weighted_mean,
   cols_all, country, data_name, i, suffix, w_vec, lower_q, upper_q,
   lo, hi, x, eua_lo, eua_hi)



#### Individual ARX* estimation ####

# Separation of the various data_XX in Y_XX (target of country XX) and X_XX (exogenous (foreign + non-foreign) of country XX).
for (country in country_codes) {
  data_name <- paste0("data_", country)
  data <- get(data_name)
  y_name <- paste0("y_", country)
  var_name <- paste0("d_l_elec_", country)
  assign(y_name, data[[var_name]])
  n <- length(data[[var_name]])
  X_name <- paste0("X_", country)
  X_vars <- setdiff(colnames(data), var_name)
  X_df <- data %>% dplyr::select(all_of(X_vars))
  assign(X_name, X_df)
}

rm(X_df, y_name, country, data_name, var_name, X_name, X_vars)

# Function that generates the necessary lags for the estimation of the individual ARX*.
makeXlags <- function(X_orig, m) {
  X_mat <- as.matrix(X_orig)
  k     <- ncol(X_mat)
  if (m == 0) {
    colnames(X_mat) <- paste0(colnames(X_mat), "_lag0")
    return(X_mat)
  }
  lagged_and_cont <- embed(X_mat, m + 1)
  X_lags_and_cont <- lagged_and_cont[, 1:(k * (m + 1)), drop = FALSE]
  colnames(X_lags_and_cont) <- unlist(
    lapply(0:m, function(l) paste0(colnames(X_mat), "_lag", l))
  )
  return(X_lags_and_cont)
}

# Creation of the dummy variables.
# ! important !:  Since we have log differences (i.e., 1-1-2018 which is a Monday is no longer present as a data point,
# these dummies start from Tuesday: 1st entry of the dataset = 1-2-2018).
dow <- ((seq_len(n) - 1 + 1) %% 7) + 1
dow_factor <- factor(dow)
dow_dummies <- model.matrix(~ dow_factor)[, -1]

rm(dow, dow_factor)

check_univariate_stability <- function(arx_model, S = 7) {
  # This is needed to evaluate the stationarity of the estimated models.
  
  library(polynom)
  
  cf_dom <- coef(arx_model)
  # Extracts non-seasonal phi.
  ar_names <- grep("^ar[0-9]+$", names(cf_dom), value = TRUE)
  p_i      <- if (length(ar_names)) max(as.integer(sub("ar","",ar_names))) else 0
  phi_ns   <- numeric(p_i + 1)
  phi_ns[1] <- 1
  for (l in seq_len(p_i)) phi_ns[l+1] <- - (cf_dom[paste0("ar", l)] %||% 0)
  
  # Extracts sar1
  Phi_S <- if ("sar1" %in% names(cf_dom)) cf_dom["sar1"] else 0
  
  # Builds the two polynomials.
  poly_ns <- polynomial(phi_ns)
  poly_s  <- polynomial(c(1, rep(0, S-1), -Phi_S))
  
  # Overall polynomial = (1 − Σ φℓ Lℓ)(1 − Φ_S L^S)
  poly_full <- poly_ns * poly_s
  
  # Roots computation.
  roots_full <- polyroot(coef(poly_full))
  min_root   <- min(Mod(roots_full))
  
  return(min_root)
}

elec_prices_not_transformed <- read_excel("data/raw/country_specific/electricity_prices_not_transformed.xlsx")
    # This is needed to plot the IRFs in the levels (in case one would want that).
    # The order of columns in elec_prices_not_transformed does not correspond
    # to the alphabetical order in country codes but this is not a problem since 
    # the function plot_country_irf already accounts for this (as it extracts the column
    # of interest on the basis of the column's name).

plot_country_irf <- function(country_code, shock_var, shock_size, h, elec_prices_df) {
  
  seasonal_period <- 7
  price_col       <- paste0("elec_", country_code)
  P0              <- tail(elec_prices_df[[price_col]], 1)
  
  # Extracts the coefficients from the ARX model.
  coefs        <- coef(get(paste0("ARX_", country_code)))
  phi          <- coefs[grep("^ar[0-9]+$", names(coefs))]
  phi_sar      <- coefs[grep("^sar[0-9]+$", names(coefs))]
  beta_shock   <- coefs[grep(paste0("^", shock_var, "_lag"), names(coefs))]
  
  # Prepares the shocks matrix.
  lags  <- as.integer(sub(".*_lag", "", names(beta_shock)))
  x_irf <- matrix(0, nrow = h+1, ncol = length(beta_shock),
                  dimnames = list(0:h, names(beta_shock)))
  idx   <- cbind(lags + 1, seq_along(lags))
  x_irf[idx] <- shock_size
  
  # Simulates the IRF in delta ln price.
  y_irf <- numeric(h+1)
  for (t in seq_len(h+1)) {
    if (t > length(phi)) {
      ar_part <- sum(phi * rev(y_irf[(t-length(phi)):(t-1)]))
    } else if (t > 1) {
      ar_part <- sum(phi[1:(t-1)] * rev(y_irf[1:(t-1)]))
    } else {
      ar_part <- 0
    }
    if (length(phi_sar) > 0 && t > seasonal_period) {
      sar_part <- phi_sar * y_irf[t - seasonal_period]
    } else {
      sar_part <- 0
    }
    
    x_part   <- sum(beta_shock * x_irf[t, ])
    y_irf[t] <- ar_part + sar_part + x_part
  }
  
  # Back-transform to the levels.
  cumlog     <- cumsum(y_irf)
  pct_change <- exp(cumlog) - 1
  level_imp  <- P0 * pct_change
  
  # Creates the dataframe.
  df <- data.frame(
    t            = 0:h,
    delta_ln     = y_irf,
    level_imp    = level_imp
  )
  
  # Builds the plot.
  p <- ggplot(df, aes(x = t, y = level_imp)) +
    geom_line(size = 1.2, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste0("IRF on ", country_code, " – ", shock_var, " shock"),
      x     = "Horizon (t)",
      y     = "Δ Price: electricity price in levels"
    ) +
    theme_minimal(base_size = 14)
  
  return(p)
}



##### ARX AT #####

# This code structure will be repeated for each country. This could be refactored.
# However, when building the individual ARX, I needed to check each model and adjust
# it on the basis of its behaviour (residuals, summary output, etc.) which was
# different for each country.

# Given that each country required a significant degree of personalisation 
# I decided to repeat this code snippet for all the selected countries.

# In addition, some countries (e.g., NO and SI) required a slightly different structure
# in order to properly prepare all the coefficients needed for the GVAR computation.

p_AT = 3
m_AT = 1

X_lags      <- makeXlags(X_AT, m_AT)
y           <- y_AT[(m_AT + 1):length(y_AT)]

dow_aligned <- dow_dummies[(m_AT + 1):length(y_AT), ]

X           <- cbind(dow_aligned, X_lags)

ARX_AT <- Arima(y,
                order = c(p_AT, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_AT)

coefs <- ARX_AT$coef
sterrs   <- sqrt(diag(ARX_AT$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_AT)
# residuals <- residuals(ARX_AT)         # To see other than the ACF, also the
# ggtsdisplay(residuals, lag.max = 30)   # PACF of residuals.

check_univariate_stability(ARX_AT, S = 7)

plot_country_irf(country_code = "AT", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX BE #####

p_BE = 4
m_BE = 0

X_lags      <- makeXlags(X_BE, m_BE)
y           <- y_BE[(m_BE + 1):length(y_BE)]

dow_aligned <- dow_dummies[(m_BE + 1):length(y_BE), ]

X           <- cbind(dow_aligned, X_lags)

ARX_BE <- Arima(y,
                order = c(p_BE, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_BE)

coefs <- ARX_BE$coef
sterrs   <- sqrt(diag(ARX_BE$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_BE)

check_univariate_stability(ARX_BE, S = 7)

plot_country_irf(country_code = "BE", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX BG #####

p_BG = 7
m_BG = 0

X_lags      <- makeXlags(X_BG, m_BG)
y           <- y_BG[(m_BG + 1):length(y_BG)]

dow_aligned <- dow_dummies[(m_BG + 1):length(y_BG), ]

X           <- cbind(dow_aligned, X_lags)

ARX_BG <- Arima(y,
                order = c(p_BG, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_BG)

coefs <- ARX_BG$coef
sterrs   <- sqrt(diag(ARX_BG$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_BG)

check_univariate_stability(ARX_BG, S = 7)

plot_country_irf(country_code = "BG", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX CZ #####

p_CZ = 4
m_CZ = 0

X_lags      <- makeXlags(X_CZ, m_CZ)
y           <- y_CZ[(m_CZ + 1):length(y_CZ)]

dow_aligned <- dow_dummies[(m_CZ + 1):length(y_CZ), ]

X           <- cbind(dow_aligned, X_lags)

ARX_CZ <- Arima(y,
                order = c(p_CZ, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_CZ)

coefs <- ARX_CZ$coef
sterrs   <- sqrt(diag(ARX_CZ$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_CZ)

check_univariate_stability(ARX_CZ, S = 7)

plot_country_irf(country_code = "CZ", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX DE #####

p_DE = 3
m_DE = 0

X_lags      <- makeXlags(X_DE, m_DE)
y           <- y_DE[(m_DE + 1):length(y_DE)]

dow_aligned <- dow_dummies[(m_DE + 1):length(y_DE), ]

X           <- cbind(dow_aligned, X_lags)

ARX_DE <- Arima(y,
                order = c(p_DE, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_DE)

coefs <- ARX_DE$coef
sterrs   <- sqrt(diag(ARX_DE$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_DE)

check_univariate_stability(ARX_DE, S = 7)

plot_country_irf(country_code = "DE", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX DK #####

p_DK = 7
m_DK = 0

X_lags      <- makeXlags(X_DK, m_DK)
y           <- y_DK[(m_DK + 1):length(y_DK)]

dow_aligned <- dow_dummies[(m_DK + 1):length(y_DK), ]

X           <- cbind(dow_aligned, X_lags)

ARX_DK <- Arima(y,
                order = c(p_DK, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_DK)

coefs <- ARX_DK$coef
sterrs   <- sqrt(diag(ARX_DK$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_DK)

check_univariate_stability(ARX_DK, S = 7)

plot_country_irf(country_code = "DK", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX EE #####

p_EE = 5
m_EE = 0

X_lags      <- makeXlags(X_EE, m_EE)
y           <- y_EE[(m_EE + 1):length(y_EE)]

dow_aligned <- dow_dummies[(m_EE + 1):length(y_EE), ]

X           <- cbind(dow_aligned, X_lags)

ARX_EE <- Arima(y,
                order = c(p_EE, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_EE)

coefs <- ARX_EE$coef
sterrs   <- sqrt(diag(ARX_EE$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_EE)

check_univariate_stability(ARX_EE, S = 7)

plot_country_irf(country_code = "EE", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX ES #####

p_ES = 5
m_ES = 0

X_lags      <- makeXlags(X_ES, m_ES)
y           <- y_ES[(m_ES + 1):length(y_ES)]

dow_aligned <- dow_dummies[(m_ES + 1):length(y_ES), ]

X           <- cbind(dow_aligned, X_lags)

ARX_ES <- Arima(y,
                order = c(p_ES, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_ES)

coefs <- ARX_ES$coef
sterrs   <- sqrt(diag(ARX_ES$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_ES)

check_univariate_stability(ARX_ES, S = 7)

plot_country_irf(country_code = "ES", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX FI #####

p_FI = 4
m_FI = 0

X_lags      <- makeXlags(X_FI, m_FI)
y           <- y_FI[(m_FI + 1):length(y_FI)]

dow_aligned <- dow_dummies[(m_FI + 1):length(y_FI), ]

X           <- cbind(dow_aligned, X_lags)

ARX_FI <- Arima(y,
                order = c(p_FI, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_FI)

coefs <- ARX_FI$coef
sterrs   <- sqrt(diag(ARX_FI$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_FI)

check_univariate_stability(ARX_FI, S = 7)

plot_country_irf(country_code = "FI", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX FR #####

p_FR = 7
m_FR = 0

X_lags      <- makeXlags(X_FR, m_FR)
y           <- y_FR[(m_FR + 1):length(y_FR)]

dow_aligned <- dow_dummies[(m_FR + 1):length(y_FR), ]

X           <- cbind(dow_aligned, X_lags)

ARX_FR <- Arima(y,
                order = c(p_FR, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_FR)

coefs <- ARX_FR$coef
sterrs   <- sqrt(diag(ARX_FR$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_FR)

check_univariate_stability(ARX_FR, S = 7)

plot_country_irf(country_code = "FR", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX GR #####

p_GR = 5
m_GR = 1

X_lags      <- makeXlags(X_GR, m_GR)
y           <- y_GR[(m_GR + 1):length(y_GR)]

dow_aligned <- dow_dummies[(m_GR + 1):length(y_GR), ]

X           <- cbind(dow_aligned, X_lags)

ARX_GR <- Arima(y,
                order = c(p_GR, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_GR)

coefs <- ARX_GR$coef
sterrs   <- sqrt(diag(ARX_GR$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_GR)

check_univariate_stability(ARX_GR, S = 7)

plot_country_irf(country_code = "GR", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX HU #####

p_HU = 4
m_HU = 1

X_lags      <- makeXlags(X_HU, m_HU)
y           <- y_HU[(m_HU + 1):length(y_HU)]

dow_aligned <- dow_dummies[(m_HU + 1):length(y_HU), ]

X           <- cbind(dow_aligned, X_lags)

ARX_HU <- Arima(y,
                order = c(p_HU, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_HU)

coefs <- ARX_HU$coef
sterrs   <- sqrt(diag(ARX_HU$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_HU)

check_univariate_stability(ARX_HU, S = 7)

plot_country_irf(country_code = "HU", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX IT #####

p_IT = 8
m_IT = 1

X_lags      <- makeXlags(X_IT, m_IT)
y           <- y_IT[(m_IT + 1):length(y_IT)]

dow_aligned <- dow_dummies[(m_IT + 1):length(y_IT), ]

X           <- cbind(dow_aligned, X_lags)

ARX_IT <- Arima(y,
                order = c(p_IT, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_IT)

coefs <- ARX_IT$coef
sterrs   <- sqrt(diag(ARX_IT$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_IT)

check_univariate_stability(ARX_IT, S = 7)

plot_country_irf(country_code = "IT", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX LT #####

p_LT = 6
m_LT = 1

X_lags      <- makeXlags(X_LT, m_LT)
y           <- y_LT[(m_LT + 1):length(y_LT)]

dow_aligned <- dow_dummies[(m_LT + 1):length(y_LT), ]

X           <- cbind(dow_aligned, X_lags)

ARX_LT <- Arima(y,
                order = c(p_LT, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_LT)

coefs <- ARX_LT$coef
sterrs   <- sqrt(diag(ARX_LT$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_LT)

check_univariate_stability(ARX_LT, S = 7)

plot_country_irf(country_code = "LT", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX LV #####

p_LV = 8
m_LV = 1

X_lags      <- makeXlags(X_LV, m_LV)
y           <- y_LV[(m_LV + 1):length(y_LV)]

dow_aligned <- dow_dummies[(m_LV + 1):length(y_LV), ]

X           <- cbind(dow_aligned, X_lags)

ARX_LV <- Arima(y,
                order = c(p_LV, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_LV)

coefs <- ARX_LV$coef
sterrs   <- sqrt(diag(ARX_LV$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_LV)

check_univariate_stability(ARX_LV, S = 7)

plot_country_irf(country_code = "LV", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX NL #####

p_NL = 7
m_NL = 0

X_lags      <- makeXlags(X_NL, m_NL)
y           <- y_NL[(m_NL + 1):length(y_NL)]

dow_aligned <- dow_dummies[(m_NL + 1):length(y_NL), ]

X           <- cbind(dow_aligned, X_lags)

ARX_NL <- Arima(y,
                order = c(p_NL, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_NL)

coefs <- ARX_NL$coef
sterrs   <- sqrt(diag(ARX_NL$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_NL)

check_univariate_stability(ARX_NL, S = 7)

plot_country_irf(country_code = "NL", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX NO #####

p_NO = 2
m_NO = 0

# Remove solar from NO.
X_NO[ ,"d_l_solar_NO"] <- NULL

X_lags      <- makeXlags(X_NO, m_NO)
y           <- y_NO[(m_NO + 1):length(y_NO)]

dow_aligned <- dow_dummies[(m_NO + 1):length(y_NO), ]

X           <- cbind(dow_aligned, X_lags)

ARX_NO <- Arima(y,
                order = c(p_NO, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

# I add the coefficient associated with solar setting it = 0.
# This is needed otherwise the computation of the GVAR won't work if 
# a coefficient is missing.

# Coefficients
co <- ARX_NO$coef
new_name  <- "d_l_solar_NO_lag0"
new_value <- 0
k <- 13
co_left  <- co[seq_len(k-1)]
co_right <- co[seq(k, length(co))]
co_new   <- c(co_left,
              setNames(new_value, new_name),
              co_right)
ARX_NO$coef <- co_new

# Var-cov matrix
vc <- ARX_NO$var.coef 
p  <- nrow(vc)
vc_new <- matrix(0, nrow = p+1, ncol = p+1)
vc_new[1:(k-1), 1:(k-1)]         <- vc[1:(k-1), 1:(k-1)]
vc_new[1:(k-1), (k+1):(p+1)]     <- vc[1:(k-1), k:p]
vc_new[(k+1):(p+1), 1:(k-1)]     <- vc[k:p, 1:(k-1)]
vc_new[(k+1):(p+1), (k+1):(p+1)] <- vc[k:p, k:p]
dimnames(vc_new) <- list(names(co_new), names(co_new))
ARX_NO$var.coef <- vc_new

rm(co, new_name,new_value, k,co_left, co_right, co_new, vc, p, vc_new)

summary(ARX_NO)

coefs <- ARX_NO$coef
sterrs   <- sqrt(diag(ARX_NO$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_NO)

check_univariate_stability(ARX_NO, S = 7)

plot_country_irf(country_code = "NO", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX PL #####

p_PL = 6
m_PL = 1

X_lags      <- makeXlags(X_PL, m_PL)
y           <- y_PL[(m_PL + 1):length(y_PL)]

dow_aligned <- dow_dummies[(m_PL + 1):length(y_PL), ]

X           <- cbind(dow_aligned, X_lags)

ARX_PL <- Arima(y,
                order = c(p_PL, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_PL)

coefs <- ARX_PL$coef
sterrs   <- sqrt(diag(ARX_PL$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_PL)

check_univariate_stability(ARX_PL, S = 7)

plot_country_irf(country_code = "PL", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX PT #####

p_PT = 8
m_PT = 0

X_lags      <- makeXlags(X_PT, m_PT)
y           <- y_PT[(m_PT + 1):length(y_PT)]

dow_aligned <- dow_dummies[(m_PT + 1):length(y_PT), ]

X           <- cbind(dow_aligned, X_lags)

ARX_PT <- Arima(y,
                order = c(p_PT, 0, 0),
                seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_PT)

coefs <- ARX_PT$coef
sterrs   <- sqrt(diag(ARX_PT$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_PT)

check_univariate_stability(ARX_PT, S = 7)

plot_country_irf(country_code = "PT", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX RO #####

p_RO = 7
m_RO = 0

X_lags      <- makeXlags(X_RO, m_RO)
y           <- y_RO[(m_RO + 1):length(y_RO)]

dow_aligned <- dow_dummies[(m_RO + 1):length(y_RO), ]

X           <- cbind(dow_aligned, X_lags)

ARX_RO <- Arima(y,
                order = c(p_RO, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_RO)

coefs <- ARX_RO$coef
sterrs   <- sqrt(diag(ARX_RO$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_RO)

check_univariate_stability(ARX_RO, S = 7)

plot_country_irf(country_code = "RO", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX SE #####

p_SE = 4
m_SE = 0

X_lags      <- makeXlags(X_SE, m_SE)
y           <- y_SE[(m_SE + 1):length(y_SE)]

dow_aligned <- dow_dummies[(m_SE + 1):length(y_SE), ]

X           <- cbind(dow_aligned, X_lags)

ARX_SE <- Arima(y,
                order = c(p_SE, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

summary(ARX_SE)

coefs <- ARX_SE$coef
sterrs   <- sqrt(diag(ARX_SE$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_SE)

check_univariate_stability(ARX_SE, S = 7)

plot_country_irf(country_code = "SE", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX SI #####

p_SI = 4
m_SI = 0

# Remove wind from SI.
X_SI[ ,"d_l_wind_SI"] <- NULL

X_lags      <- makeXlags(X_SI, m_SI)
y           <- y_SI[(m_SI + 1):length(y_SI)]

dow_aligned <- dow_dummies[(m_SI + 1):length(y_SI), ]

X           <- cbind(dow_aligned, X_lags)

ARX_SI <- Arima(y,
                order = c(p_SI, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

# I add the coefficient associated with wind setting it = 0.
# Again, this is needed otherwise the GVAR computation won't work
# if a coefficient is missing.

# Coefficients
co <- ARX_SI$coef
new_name  <- "d_l_wind_SI_lag0"
new_value <- 0
k <- 13
co_left  <- co[seq_len(k-1)]
co_right <- co[seq(k, length(co))]
co_new   <- c(co_left,
              setNames(new_value, new_name),
              co_right)
ARX_SI$coef <- co_new

# Var-cov matrix
vc <- ARX_SI$var.coef 
p  <- nrow(vc)
vc_new <- matrix(0, nrow = p+1, ncol = p+1)
vc_new[1:(k-1), 1:(k-1)]         <- vc[1:(k-1), 1:(k-1)]
vc_new[1:(k-1), (k+1):(p+1)]     <- vc[1:(k-1), k:p]
vc_new[(k+1):(p+1), 1:(k-1)]     <- vc[k:p, 1:(k-1)]
vc_new[(k+1):(p+1), (k+1):(p+1)] <- vc[k:p, k:p]
dimnames(vc_new) <- list(names(co_new), names(co_new))
ARX_SI$var.coef <- vc_new

rm(co, new_name,new_value, k,co_left, co_right, co_new, vc, p, vc_new)


summary(ARX_SI)

coefs <- ARX_SI$coef
sterrs   <- sqrt(diag(ARX_SI$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_SI)

check_univariate_stability(ARX_SI, S = 7)

plot_country_irf(country_code = "SI", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### ARX SK #####

p_SK = 6
m_SK = 0

# Remove wind from SI.
X_SK[ ,"d_l_wind_SK"] <- NULL

X_lags      <- makeXlags(X_SK, m_SK)
y           <- y_SK[(m_SK + 1):length(y_SK)]

dow_aligned <- dow_dummies[(m_SK + 1):length(y_SK), ]

X           <- cbind(dow_aligned, X_lags)

ARX_SK <- Arima(y,
                order = c(p_SK, 0, 0),
                #seasonal = list(order = c(1, 0, 0), period = 7),
                xreg = X,
                method = "CSS")

# I add the coefficient associated with wind setting it = 0 here as well.

# Coefficients.
co <- ARX_SK$coef
new_name  <- "d_l_wind_SK_lag0"
new_value <- 0
k <- 15
co_left  <- co[seq_len(k-1)]
co_right <- co[seq(k, length(co))]
co_new   <- c(co_left,
              setNames(new_value, new_name),
              co_right)
ARX_SK$coef <- co_new

# Var-cov matrix
vc <- ARX_SK$var.coef 
p  <- nrow(vc)
vc_new <- matrix(0, nrow = p+1, ncol = p+1)
vc_new[1:(k-1), 1:(k-1)]         <- vc[1:(k-1), 1:(k-1)]
vc_new[1:(k-1), (k+1):(p+1)]     <- vc[1:(k-1), k:p]
vc_new[(k+1):(p+1), 1:(k-1)]     <- vc[k:p, 1:(k-1)]
vc_new[(k+1):(p+1), (k+1):(p+1)] <- vc[k:p, k:p]
dimnames(vc_new) <- list(names(co_new), names(co_new))
ARX_SK$var.coef <- vc_new

rm(co, new_name,new_value, k,co_left, co_right, co_new, vc, p, vc_new)


summary(ARX_SK)

coefs <- ARX_SK$coef
sterrs   <- sqrt(diag(ARX_SK$var.coef))
tvals    <- coefs / sterrs
pvalues  <- 2 * (1 - pnorm(abs(tvals)))
pvalues

checkresiduals(ARX_SK)

check_univariate_stability(ARX_SK, S = 7)

plot_country_irf(country_code = "SK", shock_var = "d_l_EUA", shock_size = 0.1, h = 30, elec_prices_df = elec_prices_not_transformed)



##### I create a list with all the estimated ARX* models #####
ARX_list <- list(
  AT   = ARX_AT,
  BE   = ARX_BE,
  BG   = ARX_BG,
  CZ   = ARX_CZ,
  DE   = ARX_DE,
  DK   = ARX_DK,
  EE   = ARX_EE,
  ES   = ARX_ES,
  FI   = ARX_FI,
  FR   = ARX_FR,
  GR   = ARX_GR,
  HU   = ARX_HU,
  IT   = ARX_IT,
  LT   = ARX_LT,
  LV   = ARX_LV,
  NL   = ARX_NL,
  NO   = ARX_NO,
  PL   = ARX_PL,
  PT   = ARX_PT,
  RO   = ARX_RO,
  SE   = ARX_SE,
  SI   = ARX_SI,
  SK   = ARX_SK
)



#### GVAR computation starting from the individual ARX* ####

extract_coefs <- function(arx_model) {
  # This function extracts from an Arima object, the coefficients vectors.
  # - domestic: specifically "arl" (l=1...p_i) + seasonal AR ("sar1").
  # - foreign:   "d_l_elec_x_lagl" (l=0...m_i).
  #
  # It returns two lists of vectors: domestic[[i]] and foreign[[i]]. The output for AT is for instance:
  # 
  # $domestic
  # ar1          ar2          sar1 
  # -0.29938209  -0.17980737  0.03337623 
  # 
  # $foreign
  # d_l_elec_x_AT_lag0 
  # 0.6925724 
  cf <- arx_model$coef
  dom_idx <- grep("^ar", names(cf))                                                # Regular AR coefficients.
  seas_idx <- grep("sar1", names(cf), ignore.case=TRUE)                            # Seasonal AR coefficient.
  for_idx <- grep("^d_l_elec_x_[A-Z]{2}_lag[0-9]+$", names(cf), ignore.case=TRUE)  # Foreign coefficient.
  list(
    domestic = cf[c(dom_idx, seas_idx)],
    foreign  = cf[for_idx]
  )
}

# Lists of coefficients of the estimated models; it is needed to build the Ai matrices.
coefs_list <- list(
  AT   = extract_coefs(ARX_AT),
  BE   = extract_coefs(ARX_BE),
  BG   = extract_coefs(ARX_BG),
  CZ   = extract_coefs(ARX_CZ),
  DE   = extract_coefs(ARX_DE),
  DK   = extract_coefs(ARX_DK),
  EE   = extract_coefs(ARX_EE),
  ES   = extract_coefs(ARX_ES),
  FI   = extract_coefs(ARX_FI),
  FR   = extract_coefs(ARX_FR),
  GR   = extract_coefs(ARX_GR),
  HU   = extract_coefs(ARX_HU),
  IT   = extract_coefs(ARX_IT),
  LT   = extract_coefs(ARX_LT),
  LV   = extract_coefs(ARX_LV),
  NL   = extract_coefs(ARX_NL),
  NO   = extract_coefs(ARX_NO),
  PL   = extract_coefs(ARX_PL),
  PT   = extract_coefs(ARX_PT),
  RO   = extract_coefs(ARX_RO),
  SE   = extract_coefs(ARX_SE),
  SI   = extract_coefs(ARX_SI),
  SK   = extract_coefs(ARX_SK)
)

rm(data)

build_Ai_blocks <- function(coefs_list, P, S = 7) { 

# P = max lag of the GVAR = max(max_i(p_i), max_i(k_i)), hence it does 
# not count the seasonal AR polynomials as this lag extension is 
# already computed within this function.

# S: seasonal polynomial in L^7.
  
# coefs_list = list of all estimated ARX coefficients through 
# the function extract_coefs().

# coefs_list  <- list(AT = extract_coefs(ARX_AT),
#                     BE = extract_coefs(ARX_BE),
#                     BG = extract_coefs(ARX_BG),
#                     ...)
  
# Will return a list of elements Ai_blocks[[i]] where each element
# is associated to country i and contains all the matrices Ai0, Ai1, ...

  # List of lists filled with N (= number of countries) NULL values.
  Ai_blocks <- vector("list", length(coefs_list))

  # I rename every sub-element of the major list with country codes.
  names(Ai_blocks) <- names(coefs_list) 

  globalMaxLag <- P + S
  
  for (i in seq_along(coefs_list)) {
    cf_dom <- coefs_list[[i]]$domestic     # Domestic coefficients.
    cf_for <- coefs_list[[i]]$foreign      # Foreign coefficients.
    
    # 1) Extracts p_i e p_S.
    ar_names <- grep("^ar[0-9]+$", names(cf_dom), value = TRUE)
    p_i   <- if (length(ar_names)) max(as.integer(sub("ar","",ar_names))) else 0
    p_S <- if ("sar1" %in% names(cf_dom)) cf_dom["sar1"] else 0
    
    # 2) Builds rho(L) = ()
    rho <- numeric(p_i + 1)
    rho[1] <- 1
    
    for (l in seq_len(p_i)) { 
      rho[l+1] <- - (cf_dom[paste0("ar",l)] %||% 0)  
      # rho will be (1 - ar1 - ar2 - ar3) if p_i = 3 for instance. 
    }
    
    # 3) I consider the seasonal polynomial if present.
    maxLag <- globalMaxLag
    psi    <- numeric(maxLag + 1)
    
    for (l in 0:maxLag) {
      term1 <- if (l <= p_i) rho[l+1] else 0
      term2 <- if (l >= S) - p_S * (if ((l - S) <= p_i) rho[l-S+1] else 0) else 0
      psi[l+1] <- term1 + term2 
      # if p_i = 3 -> rho = 1 - ar1 - ar2 - ar3 - sar1
    } #                       + sar1*ar1 + sar1*ar2 + sar1*ar3.
    
    # 4) Obtains the coefficients λ_i.
    lag_names <- grep("_lag[0-9]+$", names(cf_for), value = TRUE)   
    # lag_names is the name of the foreign coefficient as in output 
    # from the summary.
    
    if (length(lag_names)) {
      k_i <- max(as.integer(sub(".*_lag","", lag_names)))
    } else {
      k_i <- 0
    }
    lambda <- numeric(maxLag + 1)
    
    for (l in 0:k_i) {
      nm <- paste0("_lag", l)
      lambda[l+1] <- cf_for[grep(nm, names(cf_for), fixed = TRUE)]
    }
    
    # 5) Builds a list of Ai blocks.
    Ai_i <- vector("list", maxLag + 1)
    names(Ai_i) <- paste0("lag", 0:maxLag)
    for (l in 0:maxLag) {
      dom_coef <- if (l == 0) psi[1] else - psi[l+1]
      for_coef <- if (l == 0) - lambda[1] else lambda[l+1]
      Ai_i[[l+1]] <- c(domestic = dom_coef, foreign = for_coef)
    }
    Ai_blocks[[i]] <- Ai_i
  }
  
  return(Ai_blocks)
}

make_Wi_list <- function(country_codes, W_global) {
  
  # country_codes: vector of length N with the country names.
  # W_global: NxN matrix with weights w_ji (w_ii = 0).
  
  # Will return the link matrices Wi.

  N <- length(country_codes)          
  Wi_list <- vector("list", N)
  names(Wi_list) <- country_codes
  
  for (i in seq_len(N)) { 
    Wi <- matrix(0, nrow = 2, ncol = N,
                 dimnames = list(c("y_i","x*_i"), country_codes))
    
    Wi[1, i] <- 1

    Wi[2, ] <- W_global[, i]
    Wi_list[[i]] <- Wi
  }
  
  return(Wi_list)
}

extract_intercepts <- function(ARX_list) {
  # This is needed to extract the vector of intercepts mu_0.
  
  intercepts <- sapply(ARX_list, function(mod) {
    cf <- coef(mod)
    if ("intercept" %in% names(cf)) {
      cf["intercept"]
    } else {
      NA                 # For the case in which the intercept is not found.
    }
  })
  return(intercepts)
}

extract_delta <- function(ARX_list) {
  # This extracts the matrix delta which is an Nx6 (given that I have 6 dummies).
  
country_codes <- names(ARX_list)
  dummy_names   <- paste0("dow_factor", 2:7)             # I want the columns: dow_factor2 ... dow_factor7.

  mat <- t(sapply(ARX_list, function(mod) {              # For each model, I extract in a vector the 6 coefficients.
    cf <- coef(mod)

    vals <- cf[dummy_names]                              # Extracts cf["dow_factor2"], ..., cf["dow_factor7"].
    vals[!dummy_names %in% names(cf)] <- NA              # If some name does not exist, I put it as NA.
    return(vals)
  }))
  
  delta <- matrix(mat,                                   # Transforms the matrix.
                  nrow   = length(ARX_list),
                  ncol   = length(dummy_names),
                  dimnames = list(country_codes, dummy_names))
  return(delta)
}

build_Ex_blocks <- function(ARX_list,
                            exog_local  = c("d_l_wind","d_l_solar",
                                            "d_temp","d_precip"),
                            exog_global = c("d_l_EUA","d_l_natural_gas",
                                            "d_l_coal","d_l_brent",
                                            "d_l_stoxx_600")) {
  
  # Builds the exogenous blocks B0^c, ..., Bk^c, B0^g, ..., Bk^g.
  # The output is a list of this kind:
  # list(
  #   Exc = list(B0^c, B1^c, ...),  # country-specific.
  #   Exg = list(B0^g, B1^g, ...)   # global.
  # )
  
  country_codes <- names(ARX_list)
  N             <- length(country_codes)
  
  max_lags <- sapply(ARX_list, function(mod) {
    lags <- grep("_lag[0-9]+$", names(coef(mod)), value = TRUE)
    if (length(lags)) max(as.integer(sub(".*_lag","", lags))) else 0
  })
  K <- max(max_lags)
  
  # I prepare an empty structure.
  Exc_blocks <- vector("list", K+1)
  Exg_blocks <- vector("list", K+1)
  names(Exc_blocks) <- paste0("Bc", 0:K)
  names(Exg_blocks) <- paste0("Bg", 0:K)
  
  # Loop on the K lags.
  for (l in 0:K) {
    
    # 1) country‑specific: N × (Nx4).
    Bc <- matrix(0, nrow = N, ncol = N * length(exog_local),
                  dimnames = list(country_codes,
                                  paste0(rep(country_codes, 
                                             each=length(exog_local)),
                                         "_", exog_local)))
    # 2) global: N × 5.
    Bg <- matrix(0, nrow = N, ncol = length(exog_global),
                  dimnames = list(country_codes, exog_global))
    
    for (i in seq_len(N)) {
      country <- country_codes[i]
      cf      <- coef(ARX_list[[i]])
      
      # Country-specific: each country has its four "names".
      for (k in seq_along(exog_local)) {
        varb <- exog_local[k]
        nm   <- paste0(varb, "_", country, "_lag", l)
        if (nm %in% names(cf)) {
          colname <- paste0(country, "_", varb)
          Bc[i, colname] <- cf[nm]
        }
      }
      
      # Global: same 5 columns for each country.
      for (k in seq_along(exog_global)) {
        varb <- exog_global[k]
        nm   <- paste0(varb, "_lag", l)
        if (nm %in% names(cf)) {
          Bg[i, varb] <- cf[nm]
        }
      }
    }
    
    Exc_blocks[[l+1]] <- Bc
    Exg_blocks[[l+1]] <- Bg
  }
  
  list(Exc = Exc_blocks, Exg = Exg_blocks)
}

build_G_blocks <- function(Ai_blocks, Wi_list) {
  
  # Calculates the matrices G0, G1, ...
  
  country_codes <- names(Ai_blocks)
  N <- length(Ai_blocks)
  P <- length(Ai_blocks[[1]]) - 1   
  # P = number of lags = length(Ai_blocks[[1]]) - 1
  
  G_blocks <- vector("list", P+1)
  names(G_blocks) <- paste0("G", 0:P)
  
  for (j in 0:P) {
    Gmat <- matrix(0, nrow = N, ncol = N,
                   dimnames = list(country_codes, country_codes))
    
    for (i in seq_len(N)) {
      Ai_ell <- Ai_blocks[[i]][[j+1]]
      Gmat[i, ] <- as.vector( Ai_ell %*% Wi_list[[i]] )
    }
    
    G_blocks[[j+1]] <- Gmat
  }
  
  return(G_blocks)
}

compute_GVAR <- function(G_blocks, mu0, delta, Exc_blocks, Exg_blocks) {
  
  # G_blocks    : list with G0, G1, ..., Gp.
  # mu0         : intercepts.
  # delta       : N×6 matrix of seasonal dummies.
  # Exc_blocks  : list with B0^c, ..., BK^c. (Each Bk^c is an N × (Nx4) matrix).
  # Exg_blocks  : list with B0^g, ..., BK^g. (Each Bk^g is an N x 5 matrix).
  
  # In output:
                # list(
                #   G0inv      = G0inv,
                #   a0         = G0inv*mu0,
                #   D          = G0inv*delta,
                #   F_matrices = list with F1, F2, ..., Fp,
                #   Hc         = list with H0^c, ..., HK^c,
                #   Hg         = list with H0^g, ..., HK^g
                # )
  
  # Extracts G0 and calculate its inverse.
  G0    <- G_blocks$G0
  G0inv <- solve(G0)
  
  # 1) a0 = G0inv*mu0.
  a0 <- as.vector(G0inv %*% mu0)
  names(a0) <- names(mu0)
  
  # 2) D = G0inv*delta.
  D <- G0inv %*% delta
  
  # 3) F_j = G0inv*G_j for j = 1, ..., P.
  P <- length(G_blocks) - 1
  F_list <- vector("list", P)
  names(F_list) <- paste0("F", 1:P)
  for (j in 1:P) {
    Gj        <- G_blocks[[j+1]]
    F_list[[j]] <- G0inv %*% Gj
  }
  
  # 4) Hv^c = G0inv*Bv^c and Hv^g = G0inv*Bc^g.
  K <- length(Exc_blocks) - 1
  Hc_list <- vector("list", K+1)
  Hg_list <- vector("list", K+1)
  names(Hc_list) <- paste0("Hc", 0:K)
  names(Hg_list) <- paste0("Hg", 0:K)
  for (v in 0:K) {
    Bc <- Exc_blocks[[v+1]]
    Bg <- Exg_blocks[[v+1]]
    Hc_list[[v+1]] <- G0inv %*% Bc
    Hg_list[[v+1]] <- G0inv %*% Bg
  }
  
  # 5) Returns everything.
  list(
    G0inv      = G0inv,
    a0         = a0,
    D          = D,
    F_matrices = F_list,
    Hc         = Hc_list,
    Hg         = Hg_list
  )
}



#### Computation of the IRFs from the exogenous (without CI's) ####

exog_local = c("d_l_wind","d_l_solar","d_temp","d_precip")
exog_global = c("d_l_EUA","d_l_natural_gas",
                "d_l_coal","d_l_brent",
                "d_l_stoxx_600")

compute_GVAR_IRF_exog <- function(GVAR_res, exog_global, shock_var, shock_size, H_horizon) {
  # This does not calculate the C.I.'s but it is needed for the function that later on will calculate them.
  
  # GVAR_res    : output from compute_GVAR()
  # exog_global : vectors with the names of the global exogenous (c("d_l_EUA",...))
  # shock_var   : exogenous on which the shock happens (needs to be in exog_global)
  # shock_size  : shock amplitude, > 0
  # H_horizon   : horizon of the IRF
  
  N <- nrow(GVAR_res$G0inv)
  P <- length(GVAR_res$F_matrices)         # maximum lag order (also with the seasonal AR) of the GVAR (= 12 in my case).
  M <- length(GVAR_res$Hg) - 1             # max exogenous lag (= 1 in my case).
  
  # 1) Matrix F (companion form)
  F_upper <- do.call(cbind, GVAR_res$F_matrices)            # blocks F1 | F2 | ... | Fp
  F_lower <- cbind(diag(N*(P-1)), matrix(0, N*(P-1), N))
  A <- rbind(F_upper, F_lower)                              # I call it "A" since "F" is a symbol in R.
  
  # 2) H_list from the global exogenous: Hg0...HgM -> I obtain the matrices Hg0, Hg1.
  K <- ncol(GVAR_res$Hg[[1]])                           # K = number of columns of Hg0.
  H_list <- lapply(GVAR_res$Hg, function(Hgv) {         # H_list contains Hg0 and Hg1.
    rbind(Hgv, matrix(0, nrow = N*(P-1), ncol = K))
  })
  
  # 3) Vector of shocks e_k of length K
  k_star <- match(shock_var, exog_global)
  if (is.na(k_star)) stop("shock_var is not in exog_global")
  e_k <- rep(0, K); e_k[k_star] <- shock_size
  
  # 4) I prepare a matrix Z_mat companion: (H+1) × (N*P)
  Z_mat <- matrix(0, nrow = H_horizon+1, ncol = N*P)       # Z_mat contains on rowa every companion vector PSI_h.
                                                           # Each row represents a lag of the horizon of the IRF (H_horizon + 1 rows).
                                                           # Each column is structured like this: (y1t y2t ... yNt y1t-1 ... yNt-1 ...).
  
  # 5)
  # h=0: only Hg0 * e_k
  Z_mat[1, ] <- as.vector(H_list[[1]] %*% e_k)
  # h=1: A * PSI_0 + Hg1 * e_k
  Z_mat[2, ] <- A %*% Z_mat[1, ] + H_list[[2]] %*% e_k
  # h>=2: only the A dynamic
  for (h in 2:H_horizon) {
    Z_mat[h+1, ] <- A %*% Z_mat[h, ]
  }
  
  # 6) I extract the first N columns (the y)
  IRF_mat <- Z_mat[, 1:N, drop = FALSE]
  colnames(IRF_mat) <- rownames(GVAR_res$G0inv)
  IRF_mat
}

extract_GVAR_residuals <- function(ARX_list, G0inv) {
  # G0inv: matrix G0^{-1}, dimensions N×N
  
  # 1) Extracts the residuals of each ARX*: list of vectors of length 2635 (if k_i = 0) or 2634 (if k_i = 1).
  resid_list <- lapply(ARX_list, residuals)
  
  # 2) Discovers the maximum length among the series.
  lengths_vec <- sapply(resid_list, length)
  max_len     <- max(lengths_vec)                 # max_len = 2635.
  
  # 3) For each series, aligns at the last timestamp:
  #    -> adds NA at the top if it is too short, so that I have all the series of residuals of length = 'max_len'.
  resid_aligned <- lapply(resid_list, function(r) {
    pad <- max_len - length(r)
    if (pad > 0) {
      c(rep(NA, pad), r)
    } else {
      r
    }
  })
  
  # 4) Combines in a matrix and removes all the rows where there's at least one NA.
  eps_mat <- do.call(cbind, resid_aligned)
  colnames(eps_mat) <- names(ARX_list)
  
  # 5) Valid temporal interval: eps_mat now will have length 2634: I remove the first row.
  eps_mat <- eps_mat[complete.cases(eps_mat), , drop = FALSE]
  
  # 6) I apply G0inv: U_t = G0inv %*% eps_t: if eps_mat is 2634xN then G0in %*% t(eps_mat) 
  #    is Nx2634 and the transpose of this latter matrix is still 2634xN.
  U <- t(G0inv %*% t(eps_mat))
  colnames(U) <- names(ARX_list)
  
  return(U)    # This will have dimensions 2634xN. Each column of country i will have a number of residuals set = 0 equal to
  # (number of ar coefficients) + (7 * number of sar coefficients) +
  # ( 0) (if k_i = 1),
  # (-1) (if k_i = 0).
}



##### Plot IRFs without C.I.'s #####

Ai_blocks <- build_Ai_blocks(coefs_list, 8, 7)    # 8 = P (= max lag of the GVAR without considering the seasonal component), 7 = order of the seasonal AR pol.
W_shrunk <- 0.7*W                                 # If necessary, reduce the influence of W since it can cause instability problems in the GVAR.
Wi_list <- make_Wi_list(country_codes, W_shrunk)

mu0 = extract_intercepts(ARX_list)
delta <- extract_delta(ARX_list)
Ex_blocks <- build_Ex_blocks(
  ARX_list    = ARX_list,
  exog_local  = exog_local,
  exog_global = exog_global
)
G_blocks <- build_G_blocks(Ai_blocks, Wi_list)
Exc_blocks <- Ex_blocks$Exc   # List Bc0, Bc1, ... (N×4N each).
Exg_blocks <- Ex_blocks$Exg   # List Bg0, Bg1, ... (N×5 each).
res <- compute_GVAR(G_blocks, mu0, delta, Exc_blocks, Exg_blocks)
res_resids <- extract_GVAR_residuals(ARX_list, res$G0inv)



###### Instability check of the GVAR ######
N <- nrow(res$G0inv)
P <- length(res$F_matrices)         # Maximum order (counting also the seasonal AR) of GVAR.
M <- length(res$Hg) - 1             # Maximum exogenous lag.
F_upper <- do.call(cbind, res$F_matrices)
F_lower <- cbind(diag(N*(P-1)), matrix(0, N*(P-1), N))
A <- rbind(F_upper, F_lower)        # This is the F matrix (companion form).
ev <- eigen(A)
max_mod <- max(Mod(ev$values))


max_mod                             # Needs to be < 1.

mods <- Mod(ev$values)              # If it is far from one it is better -> "greater stability".

rm(max_mod, mods, N, P, M, F_upper, F_lower, A, ev)



IRF_EUA <- compute_GVAR_IRF_exog(GVAR_res   = res,
                                 exog_global = exog_global,
                                 shock_var  = "d_l_EUA",
                                 shock_size = 0.10,          # 10% shock.
                                 H_horizon  = 30)

df_irf <- as.data.frame(IRF_EUA) %>%
  mutate(h = 0:30) %>%
  pivot_longer(-h, names_to = "Country", values_to = "irf_logdiff")



country_to_plot <- "FI"    # - - - To modify with the country code of interest. - - - #

price_col <- paste0("elec_", country_to_plot)                      # IRF for elec_h (= P_h) - elec_T (T = last known observation).
P0 <- tail(elec_prices_not_transformed[[price_col]], 1)            # Takes the last observed level (P0).
                                                                   # Not a problem that the columns of elec_prices_not_transformed are not ordered
                                                                   # as in in country_codes since I extract P0 considering the name of the column, not its position.

df_plot <- df_irf %>%                                              # Extracts only the chosen country and transforms from ΔlnP to levels P.
  filter(Country == country_to_plot) %>%
  arrange(h) %>%
  mutate(
    cumlog     = cumsum(irf_logdiff),           # Cumulated sum of the delta log.
    pct_change = exp(cumlog) - 1,               # Cumulated % change.
    level_imp  = P0 * pct_change                # Absolute impact on the levels.
  )

ggplot(df_plot, aes(x = h, y = cumlog*100)) +   # Plot 1: impact on the % change.
  geom_line(size = 1) +
  labs(
    title = paste0("IRF to a 10% shock of EUA: ( ln(elec_h) - ln(elec_T) ) * 100 ", country_to_plot),
    x = "Horizon (days)",
    y = "( ln(elec_h) - ln(elec_T) ) * 100"
  ) +
  theme_minimal()

                                               # Plot 2 (if needed): % response. 
df_sel <- df_irf %>%                           # This represents the % change with respect to the previous price: ln(elec_h) - ln(elec_h-1).
  filter(Country %in% country_to_plot)

ggplot(df_sel, aes(x = h, y = irf_logdiff*100)) +
  geom_line(size = 1) +
  labs(
    title = paste0("IRF to a 10% shock EUA: ( ln(elec_h) - ln(elec_h-1) ) * 100", country_to_plot),
    x = "Horizon (days)",
    y = "( ln(elec_h) - ln(elec_h-1) ) * 100"
  ) +
  theme_minimal()

rm(IRF_EUA, df_irf, country_to_plot, price_col, P0, df_plot, df_sel)



#### Plot ACF and distributions of the GVAR residuals ####

##### ACF GVAR residuals #####

max_lag <- 30
T_obs   <- nrow(res_resids)
acf_list <- lapply(colnames(res_resids), function(country) {
  x <- res_resids[, country]
  acf0 <- Acf(x, plot = FALSE, lag.max = max_lag)
  data.frame(
    country = country,
    lag     = as.integer(acf0$lag[-1]),
    acf     = as.numeric(acf0$acf[-1])
  )
})
df_acf <- bind_rows(acf_list)
ci <- qnorm(0.975) / sqrt(T_obs)        # 95% confidence bands.

countries    <- unique(df_acf$country)
n_countries  <- length(countries)
chunks       <- ceiling(n_countries / 3)
country_list <- split(countries, 
                      ceiling(seq_along(countries) / chunks))

for (i in seq_along(country_list)) {
  df_sub <- df_acf %>% 
    filter(country %in% country_list[[i]])
  p <- ggplot(df_sub, aes(x = lag, y = acf)) +
    geom_col(width = 0.2, fill = "black") +
    geom_hline(yintercept = 0, color = "black", size = 0.2) +
    geom_hline(yintercept =  ci, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = -ci, linetype = "dashed", color = "blue") +
    facet_wrap(~ country, ncol = 2) +
    scale_x_continuous(breaks = seq(0, max_lag, by = 5)) +
    labs(x = "Lag", y = "ACF",
         title = paste0("Correlograms GVAR residuals - Batch ", i, "/", length(country_list))) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"),
          panel.grid.major.x = element_blank())
  print(p)
}



##### GVAR residuals distributions #####

df_den <- as.data.frame(res_resids) %>%
  mutate(time = seq_len(nrow(.))) %>%
  pivot_longer(-time, names_to = "country", values_to = "residual")

stats <- df_den %>%
  group_by(country) %>%
  summarize(mu = mean(residual),
            sigma = sd(residual),
            min_r = min(residual),
            max_r = max(residual),
            .groups = "drop")

df_norm <- stats %>%
  rowwise() %>%
  do({
    x_seq <- seq(.$min_r, .$max_r, length.out = 200)
    data.frame(country = .$country,
               x       = x_seq,
               y       = dnorm(x_seq, mean = .$mu, sd = .$sigma))
  })

countries    <- unique(df_den$country)
n_countries  <- length(countries)
chunks       <- ceiling(n_countries / 4)
country_list <- split(countries,
                      ceiling(seq_along(countries) / chunks))

for (i in seq_along(country_list)) {
  df_den_sub  <- df_den  %>% filter(country %in% country_list[[i]])
  df_norm_sub <- df_norm %>% filter(country %in% country_list[[i]])
  
  p <- ggplot(df_den_sub, aes(x = residual)) +
    geom_histogram(aes(y = ..density..),
                   bins  = 50,
                   fill  = "grey80",
                   color = "black") +
    geom_line(data = df_norm_sub,
              aes(x = x, y = y),
              color = "brown",
              size  = 0.8) +
    facet_wrap(~ country, ncol = 2, scales = "free") +
    labs(
      title = paste0("Histogram + density + theoretical normal GVAR residuals - Batch ", 
                     i, "/4"),
      x     = "Residuals",
      y     = "Density"
    ) +
    theme_minimal() +
    theme(
      strip.text        = element_text(face = "bold"),
      panel.grid.major  = element_line(color = "grey90")
    )
  print(p)
}

rm(acf_list, df_acf, df_den, df_norm, stats, ci, max_lag, T_obs,
   countries, n_countries, chunks, country_list)



#### Plot IRFs with C.I.'s: bootstrap procedure ####

center_residuals <- function(U) {
  # Function to center the GVAR residuals.
  
  # U: T×N matrix of GVAR residuals (not centered).
  col_means <- colMeans(U, na.rm = TRUE)
  sweep(U, 2, col_means, FUN = "-")                 # Returns a TXN matrix with the centered residuals.
}

build_overlapped_blocks <- function(Uc, L) {
  # Function to build all the overlapped blocks of length L
  # on which sampling with replacement.
  
  # Uc: T×N matrix of centered residuals (output of center_residuals).
  # L = blocks amplitude.
  T_obs  <- nrow(Uc)
  blocks <- lapply(1:(T_obs - L + 1), function(start) {
    Uc[start:(start + L - 1), , drop = FALSE]
  })
  return(blocks)                   # Returns a list of (T-L+1) blocks, each L×N.
                                   # blocks is a list where each element is a block that contains (for each column, i.e., for each country):
                                   # the residuals from 1 to L. The next block contains the residuals from 2 to L+1, etc.
}

sample_bootstrap_ut <- function(blocks, T_obs_residuals, L) {
  # Function to assemble (through sampling with replacement) a single series bootstrap of residuals
  # where T_obs_residuals is the number of observations of residuals.
  # Obviously ub will have T_obs_residuals observations (= 2634).
  
  B <- ceiling(T_obs_residuals / L)                        # If I have 100/50 -> 2 blocks, if I have 100/51 -> 2 blocks, if I have 100/100 -> 1 block.
                                                           # If L = 49 -> 100/49 -> take 3 blocks, put them together and take the first T_obs_residuals obs.

  idx <- sample(seq_along(blocks), size = B, replace = TRUE)           # Samples B blocks with replacement.
  ub <- do.call(rbind, blocks[idx])

  ub[1:T_obs_residuals, , drop = FALSE]           # Takes the first T_obs_residuals rows.
                                                  # Returns a matrix (T_obs_residuals x N) of bootstrapped residuals.
}

##### Building the matrices d_exog_local, global, d_seasonal and y_original #####

data_all <- bind_cols(
  elec_prices,
  wind_gen,
  solar_gen,
  temp_index,   
  precip_index, 
  common_inputs
)
d_exog_local <- do.call(cbind, lapply(country_codes, function(country) {       # Each row is the vector gammac_t for all t = 1, ..., T.
  wind   <- data_all[[paste0("d_l_wind_", country)]]
  solar  <- data_all[[paste0("d_l_solar_",country)]]
  temp   <- data_all[[paste0("d_",       country, "_t2m")]]
  precip <- data_all[[paste0("d_",       country, "_tp")]]
  cbind(wind, solar, temp, precip)
}))
colnames(d_exog_local) <- paste0(                      # Renames the columns in the same order as of Bc0, Bc1, ...
  rep(country_codes, each = 4),
  "_",
  c("d_l_wind","d_l_solar","d_temp","d_precip")
)

d_exog_global <- as.matrix(data_all[ , exog_global])

d_seasonal <- as.matrix(dow_dummies)

y_original <- as.matrix(elec_prices)

rm(data_all)

simulate_xt_boot_transforms <- function(res,           # Output of compute_GVAR().
                                        d_seasonal,    # T×6 matrix of the seasonal dummies (dow 2–7).
                                        d_exog_local,  # Matrix Tx(Nx4) (with the values of the local exogenous variables for the various countries).
                                                       # Each row is the vector gammac_t for all t = 1, ..., T.
                                        d_exog_global, # Matrix Tx5 (with the values of the global exogenous variables.
                                        U_b,           # T×N bootstrap residuals.
                                        y_original) {  # Matrix TxN containing the real observations of the target.
  
  # Function to generate the series y_t^(b). -> Extracts a series from 1 to T of yb_t.
  
  # extracts the components
  a0     <- res$a0                  # N×1
  D      <- res$D                   # N×6
  F_list <- res$F_matrices          # List of F1...Fp, each element is a matrix N×N.
  Hc     <- res$Hc
  Hg     <- res$Hg
  
  N      <- length(a0)
  p      <- length(F_list)          # Is the maximum lag of the whole GVAR (considering also the lag obtained with the seasonal AR).      
  
  M            <- length(Hg) - 1          # Is the maximum M.
  T_obs_total  <- nrow(y_original)
  
  # Prepares yb = a bootstrap.
  # Xb is a matrix of dimensions T x N.

  Xb <- matrix(NA, nrow = T_obs_total, ncol = N, dimnames = list(NULL, colnames(U_b)))
  Xb[1:p, ] <- y_original[1:p, ]      # I set the p initial observations = to the original ones.
  
  for (t in (p+1):T_obs_total) {
    
    # Adds the intercept.
    xt <- a0
    
    # Adds the seasonal component.
    xt <- xt + D %*% d_seasonal[t, ]        # d_seasonal[t, ] is already a 6x1 vector, R reads it already as a vector.
    
    # Adds the local + global exogenous.
    for (v in 0:M) {
      tau <- t - v
      if (tau >= 1) {
        # 1) Country‑specific.
        xt <- xt + Hc[[v+1]] %*% d_exog_local[tau, ]
        # 2) Global.
        xt <- xt + Hg[[v+1]] %*% d_exog_global[tau, ]
      }
    }
    
    # Adds the VAR dynamic.
    for (j in 1:p) {
      xt <- xt + F_list[[j]] %*% y_original[t-j, ] 
    }
    
    # Adds the shock.
    xt <- xt + U_b[t-1, ]                      # I start from t-1 because t in the for-loop starts from p+1 but for U_b I 
                                               # need to start from p (since U_b has 2634 observations (due to the fact that
                                               # it is "shifted down" -> the 2634th coincides with the last of Xb).
    
    Xb[t, ] <- xt
  }
  
  return(Xb)
}

# ! Important ! : here one needs to modify the various ARX* specifying them with the final parameters
#                 used to specify the individual ARX*.
estimate_ARX_on_Xb <- function(Xb) {
  # Function to re-estimate the various ARX* once I have "jittered" the series yt through ybt.
  
  # I recalculate the foreign variable for each country i.
  # I use Xb and W: for each country i, I take the column i of W (weight of country i).
  foreign_list <- lapply(seq_along(country_codes), function(i) {
    xi <- Xb %*% W[, i]      # T×N %*% N×1 → T×1
    colnames(xi) <- paste0("d_l_elec_x_", country_codes[i])
    xi
  })
  names(foreign_list) <- country_codes
  
  #### ARX ATb ####
  
  # Again, this  code structure will be repeated for each country. It could be refactored,
  # but for the same reasons as in the initial ARX* estimations, I leave it like this.
  
  p_AT = 3
  m_AT = 1
  
  y_ATb         <- Xb[, "AT"]
  y_ATb         <- y_ATb[(m_AT + 1):length(y_ATb)]
  
  Xreg_AT       <- X_AT
  Xreg_AT[, 1]  <- foreign_list[["AT"]]
  Xreg_AT_lags  <- makeXlags(Xreg_AT, m_AT)

  dow_aligned   <- dow_dummies[(m_AT + 1):length(y_AT), ]
  Xreg_AT       <- cbind(dow_aligned, Xreg_AT_lags)
  
  ARX_ATb <- Arima(y_ATb,
                  order = c(p_AT, 0, 0),
                  #seasonal = list(order = c(1, 0, 0), period = 7),
                  xreg = Xreg_AT,
                  method = "CSS")
  
  
  
  #### ARX BEb ####

  p_BE = 4
  m_BE = 0
  
  y_BEb         <- Xb[, "BE"]
  y_BEb         <- y_BEb[(m_BE + 1):length(y_BEb)]
  
  Xreg_BE       <- X_BE
  Xreg_BE[, 1]  <- foreign_list[["BE"]]
  Xreg_BE_lags  <- makeXlags(Xreg_BE, m_BE)
  
  dow_aligned   <- dow_dummies[(m_BE + 1):length(y_BE), ]
  Xreg_BE       <- cbind(dow_aligned, Xreg_BE_lags)
  
  ARX_BEb <- Arima(y_BEb,
                   order = c(p_BE, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_BE,
                   method = "CSS")
  
  
  
  #### ARX BGb ####

  p_BG = 7
  m_BG = 0
  
  y_BGb         <- Xb[, "BG"]
  y_BGb         <- y_BGb[(m_BG + 1):length(y_BGb)]
  
  Xreg_BG       <- X_BG
  Xreg_BG[, 1]  <- foreign_list[["BG"]]
  Xreg_BG_lags  <- makeXlags(Xreg_BG, m_BG)
  
  dow_aligned   <- dow_dummies[(m_BG + 1):length(y_BG), ]
  Xreg_BG       <- cbind(dow_aligned, Xreg_BG_lags)
  
  ARX_BGb <- Arima(y_BGb,
                   order = c(p_BG, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_BG,
                   method = "CSS")
  
  
  
  #### ARX CZb ####

  p_CZ = 4
  m_CZ = 0
  
  y_CZb         <- Xb[, "CZ"]
  y_CZb         <- y_CZb[(m_CZ + 1):length(y_CZb)]
  
  Xreg_CZ       <- X_CZ
  Xreg_CZ[, 1]  <- foreign_list[["CZ"]]
  Xreg_CZ_lags  <- makeXlags(Xreg_CZ, m_CZ)
  
  dow_aligned   <- dow_dummies[(m_CZ + 1):length(y_CZ), ]
  Xreg_CZ       <- cbind(dow_aligned, Xreg_CZ_lags)
  
  ARX_CZb <- Arima(y_CZb,
                   order = c(p_CZ, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_CZ,
                   method = "CSS")
  
  
  
  #### ARX DEb ####

  p_DE = 3
  m_DE = 0
  
  y_DEb         <- Xb[, "DE"]
  y_DEb         <- y_DEb[(m_DE + 1):length(y_DEb)]
  
  Xreg_DE       <- X_DE
  Xreg_DE[, 1]  <- foreign_list[["DE"]]
  Xreg_DE_lags  <- makeXlags(Xreg_DE, m_DE)
  
  dow_aligned   <- dow_dummies[(m_DE + 1):length(y_DE), ]
  Xreg_DE       <- cbind(dow_aligned, Xreg_DE_lags)
  
  ARX_DEb <- Arima(y_DEb,
                   order = c(p_DE, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_DE,
                   method = "CSS")
  
  
  
  #### ARX DKb ####

  p_DK = 7
  m_DK = 0
  
  y_DKb         <- Xb[, "DK"]
  y_DKb         <- y_DKb[(m_DK + 1):length(y_DKb)]
  
  Xreg_DK       <- X_DK
  Xreg_DK[, 1]  <- foreign_list[["DK"]]
  Xreg_DK_lags  <- makeXlags(Xreg_DK, m_DK)
  
  dow_aligned   <- dow_dummies[(m_DK + 1):length(y_DK), ]
  Xreg_DK       <- cbind(dow_aligned, Xreg_DK_lags)
  
  ARX_DKb <- Arima(y_DKb,
                   order = c(p_DK, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_DK,
                   method = "CSS")
  
  
  
  #### ARX EEb ####

  p_EE = 5
  m_EE = 0
  
  y_EEb         <- Xb[, "EE"]
  y_EEb         <- y_EEb[(m_EE + 1):length(y_EEb)]
  
  Xreg_EE       <- X_EE
  Xreg_EE[, 1]  <- foreign_list[["EE"]]
  Xreg_EE_lags  <- makeXlags(Xreg_EE, m_EE)
  
  dow_aligned   <- dow_dummies[(m_EE + 1):length(y_EE), ]
  Xreg_EE       <- cbind(dow_aligned, Xreg_EE_lags)
  
  ARX_EEb <- Arima(y_EEb,
                   order = c(p_EE, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_EE,
                   method = "CSS")
  
  
  
  #### ARX ESb ####

  p_ES = 5
  m_ES = 0
  
  y_ESb         <- Xb[, "ES"]
  y_ESb         <- y_ESb[(m_ES + 1):length(y_ESb)]
  
  Xreg_ES       <- X_ES
  Xreg_ES[, 1]  <- foreign_list[["ES"]]
  Xreg_ES_lags  <- makeXlags(Xreg_ES, m_ES)
  
  dow_aligned   <- dow_dummies[(m_ES + 1):length(y_ES), ]
  Xreg_ES       <- cbind(dow_aligned, Xreg_ES_lags)
  
  ARX_ESb <- Arima(y_ESb,
                   order = c(p_ES, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_ES,
                   method = "CSS")
  
  
  
  #### ARX FIb ####

  p_FI = 4
  m_FI = 0
  
  y_FIb         <- Xb[, "FI"]
  y_FIb         <- y_FIb[(m_FI + 1):length(y_FIb)]
  
  Xreg_FI       <- X_FI
  Xreg_FI[, 1]  <- foreign_list[["FI"]]
  Xreg_FI_lags  <- makeXlags(Xreg_FI, m_FI)
  
  dow_aligned   <- dow_dummies[(m_FI + 1):length(y_FI), ]
  Xreg_FI       <- cbind(dow_aligned, Xreg_FI_lags)
  
  ARX_FIb <- Arima(y_FIb,
                   order = c(p_FI, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_FI,
                   method = "CSS")
  
  
  
  #### ARX FRb ####

  p_FR = 7
  m_FR = 0
  
  y_FRb         <- Xb[, "FR"]
  y_FRb         <- y_FRb[(m_FR + 1):length(y_FRb)]
  
  Xreg_FR       <- X_FR
  Xreg_FR[, 1]  <- foreign_list[["FR"]]
  Xreg_FR_lags  <- makeXlags(Xreg_FR, m_FR)
  
  dow_aligned   <- dow_dummies[(m_FR + 1):length(y_FR), ]
  Xreg_FR       <- cbind(dow_aligned, Xreg_FR_lags)
  
  ARX_FRb <- Arima(y_FRb,
                   order = c(p_FR, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_FR,
                   method = "CSS")
  
  
  
  #### ARX GRb ####

  p_GR = 5
  m_GR = 1
  
  y_GRb         <- Xb[, "GR"]
  y_GRb         <- y_GRb[(m_GR + 1):length(y_GRb)]
  
  Xreg_GR       <- X_GR
  Xreg_GR[, 1]  <- foreign_list[["GR"]]
  Xreg_GR_lags  <- makeXlags(Xreg_GR, m_GR)
  
  dow_aligned   <- dow_dummies[(m_GR + 1):length(y_GR), ]
  Xreg_GR       <- cbind(dow_aligned, Xreg_GR_lags)
  
  ARX_GRb <- Arima(y_GRb,
                   order = c(p_GR, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_GR,
                   method = "CSS")
  
  
  
  #### ARX HUb ####

  p_HU = 4
  m_HU = 1
  
  y_HUb         <- Xb[, "HU"]
  y_HUb         <- y_HUb[(m_HU + 1):length(y_HUb)]
  
  Xreg_HU       <- X_HU
  Xreg_HU[, 1]  <- foreign_list[["HU"]]
  Xreg_HU_lags  <- makeXlags(Xreg_HU, m_HU)
  
  dow_aligned   <- dow_dummies[(m_HU + 1):length(y_HU), ]
  Xreg_HU       <- cbind(dow_aligned, Xreg_HU_lags)
  
  ARX_HUb <- Arima(y_HUb,
                   order = c(p_HU, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_HU,
                   method = "CSS")
  
  
  
  #### ARX ITb ####

  p_IT = 8
  m_IT = 1
  
  y_ITb         <- Xb[, "IT"]
  y_ITb         <- y_ITb[(m_IT + 1):length(y_ITb)]
  
  Xreg_IT       <- X_IT
  Xreg_IT[, 1]  <- foreign_list[["IT"]]
  Xreg_IT_lags  <- makeXlags(Xreg_IT, m_IT)
  
  dow_aligned   <- dow_dummies[(m_IT + 1):length(y_IT), ]
  Xreg_IT       <- cbind(dow_aligned, Xreg_IT_lags)
  
  ARX_ITb <- Arima(y_ITb,
                   order = c(p_IT, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_IT,
                   method = "CSS")
  
  
  
  #### ARX LTb ####

  p_LT = 6
  m_LT = 1
  
  y_LTb         <- Xb[, "LT"]
  y_LTb         <- y_LTb[(m_LT + 1):length(y_LTb)]
  
  Xreg_LT       <- X_LT
  Xreg_LT[, 1]  <- foreign_list[["LT"]]
  Xreg_LT_lags  <- makeXlags(Xreg_LT, m_LT)
  
  dow_aligned   <- dow_dummies[(m_LT + 1):length(y_LT), ]
  Xreg_LT       <- cbind(dow_aligned, Xreg_LT_lags)
  
  ARX_LTb <- Arima(y_LTb,
                   order = c(p_LT, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_LT,
                   method = "CSS")
  
  
  
  #### ARX LVb ####

  p_LV = 8
  m_LV = 1
  
  y_LVb         <- Xb[, "LV"]
  y_LVb         <- y_LVb[(m_LV + 1):length(y_LVb)]
  
  Xreg_LV       <- X_LV
  Xreg_LV[, 1]  <- foreign_list[["LV"]]
  Xreg_LV_lags  <- makeXlags(Xreg_LV, m_LV)
  
  dow_aligned   <- dow_dummies[(m_LV + 1):length(y_LV), ]
  Xreg_LV       <- cbind(dow_aligned, Xreg_LV_lags)
  
  ARX_LVb <- Arima(y_LVb,
                   order = c(p_LV, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_LV,
                   method = "CSS")
  
  
  
  #### ARX NLb ####

  p_NL = 7
  m_NL = 0
  
  y_NLb         <- Xb[, "NL"]
  y_NLb         <- y_NLb[(m_NL + 1):length(y_NLb)]
  
  Xreg_NL       <- X_NL
  Xreg_NL[, 1]  <- foreign_list[["NL"]]
  Xreg_NL_lags  <- makeXlags(Xreg_NL, m_NL)
  
  dow_aligned   <- dow_dummies[(m_NL + 1):length(y_NL), ]
  Xreg_NL       <- cbind(dow_aligned, Xreg_NL_lags)
  
  ARX_NLb <- Arima(y_NLb,
                   order = c(p_NL, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_NL,
                   method = "CSS")
  
  
  
  #### ARX NOb ####
  
  p_NO = 2
  m_NO = 0
  
  y_NOb         <- Xb[, "NO"]
  y_NOb         <- y_NOb[(m_NO + 1):length(y_NOb)]
  
  # Remove solar from NO.
  X_NO[ ,"d_l_solar_NO"] <- NULL
  
  Xreg_NO       <- X_NO
  Xreg_NO[, 1]  <- foreign_list[["NO"]]
  Xreg_NO_lags  <- makeXlags(Xreg_NO, m_NO)
  
  dow_aligned   <- dow_dummies[(m_NO + 1):length(y_NO), ]
  Xreg_NO       <- cbind(dow_aligned, Xreg_NO_lags)
  
  ARX_NOb <- Arima(y_NOb,
                   order = c(p_NO, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_NO,
                   method = "CSS")
  
  # I add the solar coefficient setting it = 0.
  # Needed otherwise the GVAR computation won't work if a coefficient is missing.
  
  # Coefficients.
  co <- ARX_NOb$coef
  new_name  <- "d_l_solar_NO_lag0"
  new_value <- 0
  k <- 13
  co_left  <- co[seq_len(k-1)]
  co_right <- co[seq(k, length(co))]
  co_new   <- c(co_left,
                setNames(new_value, new_name),
                co_right)
  ARX_NOb$coef <- co_new
  
  # Var-cov matrix
  vc <- ARX_NOb$var.coef 
  p  <- nrow(vc)
  vc_new <- matrix(0, nrow = p+1, ncol = p+1)
  vc_new[1:(k-1), 1:(k-1)]         <- vc[1:(k-1), 1:(k-1)]
  vc_new[1:(k-1), (k+1):(p+1)]     <- vc[1:(k-1), k:p]
  vc_new[(k+1):(p+1), 1:(k-1)]     <- vc[k:p, 1:(k-1)]
  vc_new[(k+1):(p+1), (k+1):(p+1)] <- vc[k:p, k:p]
  dimnames(vc_new) <- list(names(co_new), names(co_new))
  ARX_NOb$var.coef <- vc_new
  
  rm(co, new_name,new_value, k,co_left, co_right, co_new, vc, p, vc_new)
  
  
  
  #### ARX PLb ####

  p_PL = 6
  m_PL = 1
  
  y_PLb         <- Xb[, "PL"]
  y_PLb         <- y_PLb[(m_PL + 1):length(y_PLb)]
  
  Xreg_PL       <- X_PL
  Xreg_PL[, 1]  <- foreign_list[["PL"]]
  Xreg_PL_lags  <- makeXlags(Xreg_PL, m_PL)
  
  dow_aligned   <- dow_dummies[(m_PL + 1):length(y_PL), ]
  Xreg_PL       <- cbind(dow_aligned, Xreg_PL_lags)
  
  ARX_PLb <- Arima(y_PLb,
                   order = c(p_PL, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_PL,
                   method = "CSS")
  
  
  
  #### ARX PTb ####

  p_PT = 8
  m_PT = 0
  
  y_PTb         <- Xb[, "PT"]
  y_PTb         <- y_PTb[(m_PT + 1):length(y_PTb)]
  
  Xreg_PT       <- X_PT
  Xreg_PT[, 1]  <- foreign_list[["PT"]]
  Xreg_PT_lags  <- makeXlags(Xreg_PT, m_PT)
  
  dow_aligned   <- dow_dummies[(m_PT + 1):length(y_PT), ]
  Xreg_PT       <- cbind(dow_aligned, Xreg_PT_lags)
  
  ARX_PTb <- Arima(y_PTb,
                   order = c(p_PT, 0, 0),
                   seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_PT,
                   method = "CSS")
  
  
  
  #### ARX ROb ####

  p_RO = 7
  m_RO = 0
  
  y_ROb         <- Xb[, "RO"]
  y_ROb         <- y_ROb[(m_RO + 1):length(y_ROb)]
  
  Xreg_RO       <- X_RO
  Xreg_RO[, 1]  <- foreign_list[["RO"]]
  Xreg_RO_lags  <- makeXlags(Xreg_RO, m_RO)
  
  dow_aligned   <- dow_dummies[(m_RO + 1):length(y_RO), ]
  Xreg_RO       <- cbind(dow_aligned, Xreg_RO_lags)
  
  ARX_ROb <- Arima(y_ROb,
                   order = c(p_RO, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_RO,
                   method = "CSS")
  
  
  
  #### ARX SEb ####
 
  p_SE = 4
  m_SE = 0
  
  y_SEb         <- Xb[, "SE"]
  y_SEb         <- y_SEb[(m_SE + 1):length(y_SEb)]
  
  Xreg_SE       <- X_SE
  Xreg_SE[, 1]  <- foreign_list[["SE"]]
  Xreg_SE_lags  <- makeXlags(Xreg_SE, m_SE)
  
  dow_aligned   <- dow_dummies[(m_SE + 1):length(y_SE), ]
  Xreg_SE       <- cbind(dow_aligned, Xreg_SE_lags)
  
  ARX_SEb <- Arima(y_SEb,
                   order = c(p_SE, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_SE,
                   method = "CSS")
  
  
  
  #### ARX SIb ####
  
  
  p_SI = 4
  m_SI = 0
  
  y_SIb         <- Xb[, "SI"]
  y_SIb         <- y_SIb[(m_SI + 1):length(y_SIb)]
  
  # Remove wind from SI.
  X_SI[ ,"d_l_wind_SI"] <- NULL
  
  Xreg_SI       <- X_SI
  Xreg_SI[, 1]  <- foreign_list[["SI"]]
  Xreg_SI_lags  <- makeXlags(Xreg_SI, m_SI)
  
  dow_aligned   <- dow_dummies[(m_SI + 1):length(y_SI), ]
  Xreg_SI       <- cbind(dow_aligned, Xreg_SI_lags)
  
  ARX_SIb <- Arima(y_SIb,
                   order = c(p_SI, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_SI,
                   method = "CSS")
  
  # I add the wind coefficient setting it = 0.
  # Needed otherwise the GVAR computation won't work if a coefficient is missing.
  
  # Coefficients.
  co <- ARX_SIb$coef
  new_name  <- "d_l_wind_SI_lag0"
  new_value <- 0
  k <- 13
  co_left  <- co[seq_len(k-1)]
  co_right <- co[seq(k, length(co))]
  co_new   <- c(co_left,
                setNames(new_value, new_name),
                co_right)
  ARX_SIb$coef <- co_new
  
  # Var-cov matrix
  vc <- ARX_SIb$var.coef 
  p  <- nrow(vc)
  vc_new <- matrix(0, nrow = p+1, ncol = p+1)
  vc_new[1:(k-1), 1:(k-1)]         <- vc[1:(k-1), 1:(k-1)]
  vc_new[1:(k-1), (k+1):(p+1)]     <- vc[1:(k-1), k:p]
  vc_new[(k+1):(p+1), 1:(k-1)]     <- vc[k:p, 1:(k-1)]
  vc_new[(k+1):(p+1), (k+1):(p+1)] <- vc[k:p, k:p]
  dimnames(vc_new) <- list(names(co_new), names(co_new))
  ARX_SIb$var.coef <- vc_new
  
  rm(co, new_name,new_value, k,co_left, co_right, co_new, vc, p, vc_new)
  
  
  
  #### ARX SKb ####
  
  p_SK = 6
  m_SK = 0
  
  y_SKb         <- Xb[, "SK"]
  y_SKb         <- y_SKb[(m_SK + 1):length(y_SKb)]
  
  # Remove wind from SK.
  X_SK[ ,"d_l_wind_SK"] <- NULL
  
  Xreg_SK       <- X_SK
  Xreg_SK[, 1]  <- foreign_list[["SK"]]
  Xreg_SK_lags  <- makeXlags(Xreg_SK, m_SK)
  
  dow_aligned   <- dow_dummies[(m_SK + 1):length(y_SK), ]
  Xreg_SK       <- cbind(dow_aligned, Xreg_SK_lags)
  
  ARX_SKb <- Arima(y_SKb,
                   order = c(p_SK, 0, 0),
                   #seasonal = list(order = c(1, 0, 0), period = 7),
                   xreg = Xreg_SK,
                   method = "CSS")
  
  # I add the wind coefficient setting it = 0.
  # Needed otherwise the GVAR computation won't work if a coefficient is missing.
  
  # Coefficients.
  co <- ARX_SKb$coef
  new_name  <- "d_l_wind_SK_lag0"
  new_value <- 0
  k <- 15
  co_left  <- co[seq_len(k-1)]
  co_right <- co[seq(k, length(co))]
  co_new   <- c(co_left,
                setNames(new_value, new_name),
                co_right)
  ARX_SKb$coef <- co_new
  
  # Var-cov matrix
  vc <- ARX_SKb$var.coef 
  p  <- nrow(vc)
  vc_new <- matrix(0, nrow = p+1, ncol = p+1)
  vc_new[1:(k-1), 1:(k-1)]         <- vc[1:(k-1), 1:(k-1)]
  vc_new[1:(k-1), (k+1):(p+1)]     <- vc[1:(k-1), k:p]
  vc_new[(k+1):(p+1), 1:(k-1)]     <- vc[k:p, 1:(k-1)]
  vc_new[(k+1):(p+1), (k+1):(p+1)] <- vc[k:p, k:p]
  dimnames(vc_new) <- list(names(co_new), names(co_new))
  ARX_SKb$var.coef <- vc_new
  
  rm(co, new_name,new_value, k,co_left, co_right, co_new, vc, p, vc_new)


    
  #### List with all the estimated models "b" ####
  ARXb_list <- list(
    AT   = ARX_ATb,
    BE   = ARX_BEb,
    BG   = ARX_BGb,
    CZ   = ARX_CZb,
    DE   = ARX_DEb,
    DK   = ARX_DKb,
    EE   = ARX_EEb,
    ES   = ARX_ESb,
    FI   = ARX_FIb,
    FR   = ARX_FRb,
    GR   = ARX_GRb,
    HU   = ARX_HUb,
    IT   = ARX_ITb,
    LT   = ARX_LTb,
    LV   = ARX_LVb,
    NL   = ARX_NLb,
    NO   = ARX_NOb,
    PL   = ARX_PLb,
    PT   = ARX_PTb,
    RO   = ARX_ROb,
    SE   = ARX_SEb,
    SI   = ARX_SIb,
    SK   = ARX_SKb
  )
  
  return(ARXb_list)
}

bootstrap_irf_CI <- function(res,
                             shock_var,
                             shock_size,
                             H_horizon,
                             GVAR_resids,
                             d_seasonal,
                             d_exog_local,
                             d_exog_global,
                             y_original,
                             B_iter,           # Number of bootstrap iterations.
                             alpha,            # Confidence level.
                             L_block) {        # Width of the L blocks.
   
  # Function that bootstraps to obtain the C.I.'s. 
  
  N                <- ncol(GVAR_resids)
  T_obs_residuals  <- nrow(GVAR_resids)
  country_names    <- colnames(GVAR_resids)
  
  # 1) Center the residuals.
  Uc <- center_residuals(GVAR_resids)       # Uc are the centered residuals.
  
  # 2) Build the overlapped blocks.
  blocks <- build_overlapped_blocks(Uc, L = L_block)    # List of (T_obs_residuals-L+1) blocks, each L×N -> blocks is a list 
                                                        # where each element is a block that contains (for each column, i.e., for each country):
                                                        # the residuals from 1 to L. The next block contains the residuals from 2 to L+1, etc.
  
  # 3) Storage for all bootstrap IRFs.
  IRF_array <- array(NA, dim = c(H_horizon + 1, N, B_iter),          # 3D array where each matrix is an H_horizon x N matrix.
                     dimnames = list(NULL, country_names, NULL))     # I will have B_iter matrices where each column is an IRF for each country.
  # Therefore, I will have B_iter IRFs for each country.
  
  message("Bootstrap procedure started...")
  
  # 4) Bootstrap loop.
  for (b in 1:B_iter) {                                              # b iterates on 1, 2, ... B_iter.
    
    # I sample the bootstrap residuals.
    U_b <- sample_bootstrap_ut(blocks, T_obs_residuals, L_block)     # U_b is a TxN matrix of bootstrapped residuals.
    
    # I generates bootstrap sample.
    Xb <- simulate_xt_boot_transforms(res            = res,          # Xb is a matrix of dimensions T x N = bootstrap sample for iteration b.
                                      d_seasonal     = d_seasonal,
                                      d_exog_local   = d_exog_local,
                                      d_exog_global  = d_exog_global,
                                      U_b            = U_b,
                                      y_original     = y_original)
    
    # I re-estimates the ARX* for each country on Xb.
    ARXb_list <- estimate_ARX_on_Xb(Xb)
    
    # I rebuild the GVAR from the new ARX* estimates.
    coefs_list_b   <- lapply(ARXb_list, extract_coefs)
    Ai_blocks_b    <- build_Ai_blocks(coefs_list_b, P = length(res$F_matrices) - 7, S = 7)   # P = length(res$F_matrices) - 7 because the function
                                                                                             # build_Ai_blocks already counts automatically the seasonal AR.     
    G_blocks_b     <- build_G_blocks(Ai_blocks_b, Wi_list)
    mu0_b          <- extract_intercepts(ARXb_list)
    delta_b        <- extract_delta(ARXb_list)
    EX_blocks_b    <- build_Ex_blocks(ARXb_list, exog_local, exog_global)
    Exc_blocks_b   <- EX_blocks_b$Exc                                         # List Bc0, Bc1, … (N×4N each).
    Exg_blocks_b   <- EX_blocks_b$Exg                                         # List Bg0, Bg1, … (N×5 each).
    
    res_b <- compute_GVAR(G_blocks_b, mu0_b, delta_b, Exc_blocks_b, Exg_blocks_b)
    
    # I compute the IRF
    IRF_b <- compute_GVAR_IRF_exog(GVAR_res     = res_b,
                                   exog_global  = exog_global,
                                   shock_var    = shock_var,
                                   shock_size   = shock_size,
                                   H_horizon    = H_horizon)
    
    IRF_array[ , , b] <- IRF_b
    
    if (b %% 5 == 0) {
      message(sprintf("Bootstrap iteration %d / %d complete", b, B_iter))
    }
  }
  
  message("Bootstrap completed.")
  
  # 5) I compute the quantiles.
  lower_q <- alpha / 2
  upper_q <- 1 - (alpha / 2)
  
  IRF_lower <- apply(IRF_array, c(1, 2), quantile, probs = lower_q, na.rm = TRUE)
  IRF_upper <- apply(IRF_array, c(1, 2), quantile, probs = upper_q, na.rm = TRUE)
  
  return(list(
    IRF_array = IRF_array,
    lower     = IRF_lower,
    upper     = IRF_upper
  ))
}

# Here I execute the bootstrap.
boot_res <- bootstrap_irf_CI(res            = res,
                             shock_var      = "d_l_EUA",
                             shock_size     = 0.10,
                             H_horizon      = 20,
                             GVAR_resids    = res_resids,
                             d_seasonal     = d_seasonal,
                             d_exog_local   = d_exog_local,
                             d_exog_global  = d_exog_global,
                             y_original     = y_original,
                             B_iter         = 1000,
                             alpha          = 0.05,
                             L_block        = 2600)

dims <- dim(boot_res$IRF_array)
H_plus_1_irf        <- dims[1]
N_countries_irf     <- dims[2]
B_iterations_irf    <- dims[3]

# I calculate the cumulated of the IRF for each iteration b = 1, ..., B. 
cum_IRF_array <- array(NA, dim = c(H_plus_1_irf, N_countries_irf, B_iterations_irf), dimnames = dimnames(boot_res$IRF_array))
for (b in seq_len(B_iterations_irf)) {
  cum_IRF_array[,,b] <- apply(boot_res$IRF_array[,,b], 2, cumsum)   # Here I calculate the cumulated.
}

# I compute the mean and CI on the cumulated.
IRF_cum_mean  <- apply(cum_IRF_array, c(1,2), mean)
IRF_cum_lower <- apply(cum_IRF_array, c(1,2),
                       quantile, probs = 0.025)
IRF_cum_upper <- apply(cum_IRF_array, c(1,2),
                       quantile, probs = 0.975)

countries <- colnames(IRF_cum_mean)
for (cty in countries) {
  df_plot <- tibble(
    Horizon = 0:(H_plus_1_irf - 1),
    Mean     = IRF_cum_mean[, cty],
    Lower    = IRF_cum_lower[, cty],
    Upper    = IRF_cum_upper[, cty]
  )
  
  p <- ggplot(df_plot, aes(x = Horizon, y = Mean * 100)) +
    geom_ribbon(aes(ymin = Lower * 100, ymax = Upper * 100),
                fill = "cyan3", alpha = 0.4) +
    geom_line(color = "blue", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste0("Cumulated IRF of ", cty,
                     " to an EUA shock (bootstrap CI)"),
      x = "Horizon",
      y = "% difference (ln(h) - ln(T))"
    ) +
    theme_minimal()
  
  # filename <- paste0(cty, ".jpeg")
  # ggsave(
  #   filename = filename,
  #   plot     = p,
  # )

  print(p)
}

# Down here there's the code to obtain the plot for the single countries if one would want those.

# dims <- dim(boot_res$IRF_array)
# H_plus_1_irf        <- dims[1]
# N_countries_irf     <- dims[2]
# B_iterations_irf    <- dims[3]
# 
# # I calculate the cumulated IRF for each iteration b = 1, ..., B. 
# cum_IRF_array <- array(NA, dim = c(H_plus_1_irf, N_countries_irf, B_iterations_irf), dimnames = dimnames(boot_res$IRF_array))
# for (b in seq_len(B_iterations_irf)) {
#   cum_IRF_array[,,b] <- apply(boot_res$IRF_array[,,b], 2, cumsum)   # Here I calculate the cumulated.
# }
# 
# # I calculate the mean and C.I.s on the cumulated.
# IRF_cum_mean  <- apply(cum_IRF_array, c(1,2), mean)
# IRF_cum_lower <- apply(cum_IRF_array, c(1,2),
#                        quantile, probs = 0.025)
# IRF_cum_upper <- apply(cum_IRF_array, c(1,2),
#                        quantile, probs = 0.975)



# country_to_plot <- "AT"                             # - - - To modify with the country code of interest. - - - #
# 
# df_plot <- tibble(
#   Horizon = 0:(H_plus_1_irf - 1),
#   Mean     = IRF_cum_mean[, country_to_plot],
#   Lower    = IRF_cum_lower[, country_to_plot],
#   Upper    = IRF_cum_upper[, country_to_plot]
# )
# 
# ggplot(df_plot, aes(x = Horizon, y = Mean*100)) +
#   
#   geom_ribbon(aes(ymin = Lower*100, ymax = Upper*100), fill = "#8EA5B6", alpha = 0.4) +
#   geom_line(color = "blue", size = 1.2) +
#   geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
#   
#   labs(
#     title = paste0(country_to_plot),
#     x = "Horizon (days)",
#     y = "Percentage increase in electricity prices"
#   ) +
#   
#   scale_x_continuous(expand = c(0, 0), breaks = pretty_breaks(n = 10)) +
#   scale_y_continuous(expand = c(0, 0), breaks = pretty_breaks(n = 10), limits = c(-4, 4)) +    # To modify with the limits of y for the plot.
#   
#   theme_minimal() +
#   theme(
#     plot.title           = element_text(hjust = 0.5, size = 16),
#     axis.title           = element_text(size = 14),
#     axis.text            = element_text(size = 12),
#     
#     panel.border         = element_blank(),
#     panel.grid.major     = element_line(color = "grey90"),
#     panel.grid.minor     = element_blank(),
#     
#     axis.line.x.bottom   = element_line(color = "black"),
#     axis.line.y.left     = element_line(color = "black"),
#     axis.ticks           = element_line(color = "black", linewidth = 1),
#     
#     axis.text.x          = element_text(margin = margin(t = 6)),
#     axis.text.y          = element_text(margin = margin(r = 6)),
#     
#     plot.margin          = margin(t = 8, r = 10, b = 5, l = 5, unit = "pt")
#   )

# # ggsave(filename = paste0(country_to_plot, ".jpeg"))