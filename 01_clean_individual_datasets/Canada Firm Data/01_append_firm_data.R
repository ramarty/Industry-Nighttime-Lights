# Append Canada Firm Data

# Load, Append and Merge Data --------------------------------------------------
# Load and Append Firm Data
firmdata_df <- list.files(file.path(data_file_path, "Canada Industry Data", "RawData", "Firm Data"), 
                          pattern = "*.csv",
                          full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows

# Spatially Define -------------------------------------------------------------
coordinates(firmdata_df) <- ~lon+lat
crs(firmdata_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract DMSPOLS --------------------------------------------------------------
firmdata_df$dmspols <- NA
for(year in c(2001, 2003, 2005, 2007, 2009, 2011, 2013)){
  print(year)
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("canada_dmspols_",year,".tif")))
  firmdata_df$dmspols[firmdata_df$year %in% year] <- extract(dmspols, firmdata_df[firmdata_df$year %in% year,]) %>% as.numeric()
}

# Extract VIIRS ----------------------------------------------------------------
firmdata_df$viirs <- NA
firmdata_df$viirs_lead <- NA

viirs_12 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("can_chull_viirs_median_",2012,".tif")))
viirs_13 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("can_chull_viirs_median_",2013,".tif")))
viirs_14 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("can_chull_viirs_median_",2014,".tif")))

## VIIRS
firmdata_df$viirs[firmdata_df$year %in% 2013] <- extract(viirs_13, firmdata_df[firmdata_df$year %in% 2013,]) %>% as.numeric()

## VIIRS Lag
firmdata_df$viirs_lead[firmdata_df$year %in% 2011] <- extract(viirs_12, firmdata_df[firmdata_df$year %in% 2011,]) %>% as.numeric()
firmdata_df$viirs_lead[firmdata_df$year %in% 2013] <- extract(viirs_14, firmdata_df[firmdata_df$year %in% 2013,]) %>% as.numeric()

# Restrict Variables to Only Needed Ones ---------------------------------------
firmdata_df@data <- firmdata_df@data %>%
  dplyr::select(year, employment, dmspols, naics2, naicsname, dmspols, viirs, viirs_lead)

# Remove Outliers --------------------------------------------------------------
firmdata_df$employment[(firmdata_df$naicsname %in% "Educational Services") &
                         firmdata_df$employment %in% 1000000] <- NA

# Fix naics --------------------------------------------------------------------
# https://en.wikipedia.org/wiki/North_American_Industry_Classification_System
firmdata_df$naics2[firmdata_df$naics2 %in% "41"] <- "42" # Wholesale Trade (41 in Canada,[3] 42 in the United States[2]);
firmdata_df$naicsname[firmdata_df$naics2 %in% "42"] <- "Wholesale Trade"

firmdata_df$naics2[firmdata_df$naics2 %in% "91"] <- "92" # (91 in the United States, 92 in Canada[4]);
firmdata_df$naicsname[firmdata_df$naics2 %in% "92"] <- "Public Administration"

# Export -----------------------------------------------------------------------
#firmdata_df <- as.data.frame(firmdata_df)
saveRDS(firmdata_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

