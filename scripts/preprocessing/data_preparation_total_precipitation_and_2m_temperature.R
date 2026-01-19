#### Libraries ####

# If needed -> install.packages(c("terra","sf","dplyr","lubridate","exactextractr","tidyr","rnaturalearth","rnaturalearthdata"))

library(terra)           # To read gridded data.
library(sf)              # This is needed for shapefiles.
library(dplyr)           
library(lubridate)       
library(exactextractr)   # This is to extract "polygons".
library(raster)
library(tidyr)           
library(rnaturalearth)   
library(ggplot2)
library(writexl)
library(readxl)



#### Data loading ####

ERA5_grib_file <- "data/raw/country_specific/total_precipitation_and_2m_temperature/ERA5.grib"

# terra should find out atuomatically the variables and time steps within the grib file.
# rast builds a SpatRaster object with many layers where each layer is a combination of: variable + timestamp.
r <- rast(ERA5_grib_file)

# I can see the layers.
print(r)

# I extract the times.
times <- time(r) 

# I create a vector of variables to identify the temperature and precipitation layers.
vars <- ifelse(grepl("2 metre temperature", names(r), ignore.case = TRUE),
               "t2m", 
               "tp")

# I create unique names.
unique_names <- paste0(vars, "_", format(times, "%Y%m%dT%H%M"))

# I assign then to the raster
names(r) <- unique_names



#### Preparation of UE countries' polygons ####

countries <- st_read("data/raw/country_specific/total_precipitation_and_2m_temperature/ne_10m_admin_0_countries.shp")

# I filter only for UE countries and I consider only nations, not regional areas.
names(countries)

# I define only the EU countries of interest's codes.
EU_ISO3 <- c(
  "AUT","BEL","BGR","HRV","CZE","DNK","EST","FIN","FRA","DEU","GRC",
  "HUN","IRL","ITA","LVA","LTU","LUX","NLD","NOR","POL","PRT","ROU","SVK",
  "SVN","ESP","SWE"
)

# I filter on the ADM0_A3 field
eu_countries <- countries %>%
  dplyr::filter(ADM0_A3 %in% EU_ISO3) %>%
  dplyr::select(ISO_A3 = ADM0_A3, NAME_EN = ADMIN)

eu_countries <- st_transform(eu_countries, crs(r))

# Let's check
ggplot(eu_countries) +
  geom_sf(fill = NA, color = "steelblue") +
  ggtitle("Extracted polygons") +
  theme_minimal()

# I notice that French Guiana is considered as part of France. We don't want this so I exclude it.
# I define a bounding box per l'EU.
europe_bb <- st_as_sfc(st_bbox(c(
  xmin = -15,  
  xmax =  40,  
  ymin =  30, 
  ymax =  73   
), crs = st_crs(4326)))

europe_bb <- st_transform(europe_bb, st_crs(eu_countries))

# I keep only each country's part that falls within the box.
eu_countries <- st_crop(eu_countries, europe_bb)

# Let's check
ggplot(eu_countries) +
  geom_sf(fill = NA, color = "steelblue") +
  ggtitle("Extracted polygons") +
  theme_minimal()
# Ok.

eu_countries <- st_transform(eu_countries, crs(r))



#### Mean values over the identified polygons for each timestamp ####

# This takes too much time for my PC to compute...

# # I build layer_info by combining name, variables and time.
# layer_info <- tibble(
#   layer    = names(r),
#   variable = vars,
#   datetime = times
# )
# 
# # For each layer I extract the average over each polygon.
# res_list <- lapply(layer_info$layer, function(lyr) {
#   
#   rast_lyr <- r[[lyr]]
# 
#   # For each layer (which is variable + time) I use exact_extract() 
#   # which calculates the average of pixels within each polygon.
#   
#   vals <- exact_extract(rast_lyr, eu_countries, 'mean')
# 
#   data.frame(
#     ISO_A3   = eu_countries$ISO_A3,
#     NAME_EN  = eu_countries$NAME_EN,
#     datetime = layer_info$datetime[layer_info$layer == lyr],
#     variable = layer_info$variable[layer_info$layer == lyr],
#     value    = vals
#   )
# })

# Other possibility

# # I transform eu_countries (sf) in SpatVector
# vec <- vect(eu_countries)
# 
# # Let's first estimate how much time this is going to take.
# single <- r[[1]]
# 
# single_mem <- toMemory(single)
# 
# system.time({
#   out1 <- terra::extract(single_mem, vec, fun=mean, na.rm=TRUE)
# })
# 
# # 0.58 -> It would take about 8.5h to solve. I don't have this much time :/ even if I could
# # run it while sleeping.
# 
# # Let's see what happens if we first crop for the EU bb.
# single <- r[[1]]
# 
# single_eu   <- crop(single, ext(eu_countries))
# 
# single_eu_mem <- toMemory(single_eu)
# 
# system.time({
#   out1 <- terra::extract(single_eu_mem, vec, fun=mean, na.rm=TRUE)
# })
# 
# # 0.38 -> 5.5h (it's better).
# # Let's try.

# It was still too slow so I try to loop for each month.

terra::terraOptions(progress=1)

ext_eu <- terra::ext(eu_countries)
vec    <- terra::vect(eu_countries)
eu_countries_sf <- eu_countries

# layer_info with index, name of the layer, datetime and variable.
layer_info <- tibble(
  idx      = seq_len(nlyr(r)),
  datetime = times,
  variable = vars,
  year     = year(times),
  ym       = format(times, "%Y-%m")
)

layer_info <- layer_info %>%
  mutate(
    year = year(datetime),
    ym   = format(datetime, "%Y-%m")   # se ti serve ancora
  )

years <- sort(unique(layer_info$year))

# This is where I load the dataset containing the information on the population: I need this
# to compute a population weighted average.
pop <- rast("data/raw/country_specific/total_precipitation_and_2m_temperature/gpw.tif") %>%
  project(crs(r)) %>%
  crop(ext_eu)

template <- r[[1]] %>% crop(ext_eu)

pop_aligned <- pop %>%
  terra::resample(template, method="near") %>%
  toMemory()

pop_wmean <- function(clim_rast, pop_rast, vec) {
  prod_rast <- clim_rast * pop_rast
  sum_prod  <- terra::extract(prod_rast, vec, fun=sum, na.rm=TRUE)[,2]
  sum_pop   <- terra::extract(pop_rast, vec, fun=sum, na.rm=TRUE)[,2]
  sum_prod / sum_pop
}

print(years)
str(years)



for (yr in years) {
  cat(">>> Year:", yr, "\n")
  
  months_in_year <- unique(layer_info$ym[layer_info$year == yr])
  
  if (yr == 2017) {
    months_in_year <- months_in_year[months_in_year == "2017-12"]
  }
  
  out_list <- vector("list", length(months_in_year))
  names(out_list) <- months_in_year
  
  for (m in months_in_year) {
    cat("  - Processing month", m, "\n")
    
    idx_month <- layer_info %>% filter(ym == m) %>% pull(idx)
    
    r_sub_mem <- r[[idx_month]] %>%
      crop(ext_eu) %>%
      toMemory()
    
    res_month <- lapply(seq_along(idx_month), function(j) {
      li    <- idx_month[j]
      var_j <- layer_info$variable[layer_info$idx == li]
      dt    <- layer_info$datetime[layer_info$idx == li]
      
      clim <- r_sub_mem[[j]]
      if (var_j == "t2m") {
        clim <- abs(clim - 291.15)
      }
      
      wmean <- pop_wmean(clim, pop_aligned, vec)
      
      tibble(
        iso_a3   = eu_countries$ISO_A3,
        NAME_EN  = eu_countries$NAME_EN,
        datetime = dt,
        variable = var_j,
        value    = wmean
      )
    })
    
    out_list[[m]] <- bind_rows(res_month)
    rm(r_sub_mem, res_month); gc()
  }
  
  df_year <- bind_rows(out_list) %>%
    pivot_wider(
      id_cols    = c(iso_a3, NAME_EN, datetime),
      names_from = variable,
      values_from= value
    )
  
  # write_xlsx(df_year, path = paste0("era5_weighted_", yr, ".xlsx"))
  
  rm(out_list, df_year); gc()
}



#### Calculating daily values ####

library(purrr)

files <- list.files(
  path    = ".", 
  pattern = "^era5_weighted_\\d{4}\\.xlsx$", 
  full.names = TRUE
)

df_all <- files %>%
  set_names() %>%
  map_dfr(read_excel, .id = "source") %>%
  mutate(
    date = as_date(datetime)
  )

daily <- df_all %>%
  group_by(iso_a3, NAME_EN, date) %>%
  summarise(

    t2m   = mean(t2m, na.rm = TRUE),
    tp  = mean(tp, na.rm = TRUE) * 24 * 1000,
    .groups = "drop"
  )

daily_wide <- daily %>%
  pivot_longer(
    cols      = c(t2m, tp),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  mutate(var_name = paste0(iso_a3, "_", variable)) %>%
  dplyr::select(date, var_name, value) %>%
  pivot_wider(names_from = var_name, values_from = value)

# write_xlsx(daily_wide, "era5_weighted_daily_2017-2025.xlsx")