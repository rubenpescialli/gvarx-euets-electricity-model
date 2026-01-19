#### Libraries ####
library(readxl)
library(dplyr)
library(writexl)



file_path <- "data/processed/final_data_levels.xlsx"



#### Log differencing electricity prices ####
sheet_elec <- "Electricity prices"
df_elec <- read_excel(path = file_path, sheet = sheet_elec)

df_logdiff_elec <- df_elec %>%
  transmute(
    across(
      .cols = everything(),
      .fns = ~ {
        x <- .
        lagx <- dplyr::lag(x)

        res <- rep(NA_real_, length(x))

        idx <- which(x > 0 & lagx > 0)
        res[idx] <- log(x[idx]) - log(lagx[idx])

        res[is.na(res)] <- 0
        return(res)
      },
      .names = "d_l_{.col}"
    )
  )

# write_xlsx(df_logdiff_elec, "d_l_electricity.xlsx")



#### Log differencing common inputs ####
sheet_ci <- "Common inputs"
df_ci <- read_excel(path = file_path, sheet = sheet_ci)

df_logdiff_ci <- df_ci %>%
  transmute(
    across(
      .cols = everything(),
      .fns = ~ {
        x <- .
        lagx <- dplyr::lag(x)
        
        res <- rep(NA_real_, length(x))
        
        idx <- which(x > 0 & lagx > 0)
        res[idx] <- log(x[idx]) - log(lagx[idx])
        
        res[is.na(res)] <- 0
        return(res)
      },
      .names = "d_l_{.col}"
    )
  )

# write_xlsx(df_logdiff_ci, "d_l_ci.xlsx")



#### Log differencing renewable generation ####
sheet_w <- "Wind generation"
df_w <- read_excel(path = file_path, sheet = sheet_w)

df_logdiff_w <- df_w %>%
  transmute(
    across(
      .cols = everything(),
      .fns = ~ {
        x <- .
        lagx <- dplyr::lag(x)
        
        res <- rep(NA_real_, length(x))
        
        idx <- which(x > 0 & lagx > 0)
        res[idx] <- log(x[idx]) - log(lagx[idx])
        
        res[is.na(res)] <- 0
        return(res)
      },
      .names = "d_l_{.col}"
    )
  )

# write_xlsx(df_logdiff_w, "d_l_w.xlsx")

sheet_s <- "Solar generation"
df_s <- read_excel(path = file_path, sheet = sheet_s)

df_logdiff_s <- df_s %>%
  transmute(
    across(
      .cols = everything(),
      .fns = ~ {
        x <- .
        lagx <- dplyr::lag(x)
        
        res <- rep(NA_real_, length(x))
        
        idx <- which(x > 0 & lagx > 0)
        res[idx] <- log(x[idx]) - log(lagx[idx])
        
        res[is.na(res)] <- 0
        return(res)
      },
      .names = "d_l_{.col}"
    )
  )

# write_xlsx(df_logdiff_s, "d_l_s.xlsx")



#### Differencing climatic variables ####
sheet_t <- "Temperature index"
df_t <- read_excel(path = file_path, sheet = sheet_t)

df_diff_t <- df_t %>%
  transmute(
    across(
      .cols = everything(),
      .fns = ~ {
        x <- .
        lagx <- dplyr::lag(x)
        res <- x - lagx
        return(res)
      },
      .names = "d_{.col}"
    )
  )

# write_xlsx(df_diff_t, "d_t.xlsx")

sheet_p <- "Precipitation index"
df_p <- read_excel(path = file_path, sheet = sheet_p)

df_diff_p <- df_p %>%
  transmute(
    across(
      .cols = everything(),
      .fns = ~ {
        x <- .
        lagx <- dplyr::lag(x)
        res <- x - lagx
        return(res)
      },
      .names = "d_{.col}"
    )
  )

# write_xlsx(df_diff_p, "d_p.xlsx")