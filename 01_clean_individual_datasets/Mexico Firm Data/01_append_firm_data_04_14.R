# Append Canada Firm Data

# Load Data --------------------------------------------------------------------
#### 04 - 14 in same format
firmdata_df <- file.path(data_file_path, "Mexico Industry Data", "RawData") %>%
  list.files(pattern = "*.csv",
             full.names = T
  ) %>%
  lapply(read_csv) %>%
  bind_rows

firmdata_df <- firmdata_df %>%
  dplyr::rename(employment = empl) %>%
  dplyr::select(year, employment, naics2, lon, lat) # naicsname 

# Spatially Define -------------------------------------------------------------
coordinates(firmdata_df) <- ~lon+lat
crs(firmdata_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract DMSPOLS --------------------------------------------------------------
firmdata_df$dmspols <- NA
dmspols_04 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",2004,".tif")))
dmspols_09 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",2009,".tif")))
dmspols_13 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",2013,".tif")))

firmdata_df$dmspols[firmdata_df$year %in% 2004] <- extract(dmspols_04, firmdata_df[firmdata_df$year %in% 2004,]) %>% as.numeric()
firmdata_df$dmspols[firmdata_df$year %in% 2009] <- extract(dmspols_09, firmdata_df[firmdata_df$year %in% 2009,]) %>% as.numeric()
firmdata_df$dmspols[firmdata_df$year %in% 2014] <- extract(dmspols_13, firmdata_df[firmdata_df$year %in% 2014,]) %>% as.numeric()

# Extract VIIRS ----------------------------------------------------------------
firmdata_df$viirs <- NA
viirs_13 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mex_viirs_mean_",2013,".tif")))

firmdata_df$viirs[firmdata_df$year %in% 2013] <- extract(viirs_13, firmdata_df[firmdata_df$year %in% 2013,]) %>% as.numeric()

# Export -----------------------------------------------------------------------
saveRDS(firmdata_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_04_14.Rds"))

