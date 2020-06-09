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

# Extract NTL ------------------------------------------------------------------
for(year in seq(from=2001, to=2013, by=2)){
  print(year)
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("canada_dmspols_", year, ".tif")))
  firmdata_df[[paste0("dmspols_", year)]] <- extract(dmspols, firmdata_df) %>% as.numeric
  
  #velox(dmspols)$extract(sp=firmdata_df, fun = function(x) mean(x, na.rm=T))
}

firmdata_df$dmspols <- NA
firmdata_df$dmspols[firmdata_df$year %in% 2001] <- firmdata_df$dmspols_2001[firmdata_df$year %in% 2001]
firmdata_df$dmspols[firmdata_df$year %in% 2003] <- firmdata_df$dmspols_2003[firmdata_df$year %in% 2003]
firmdata_df$dmspols[firmdata_df$year %in% 2005] <- firmdata_df$dmspols_2005[firmdata_df$year %in% 2005]
firmdata_df$dmspols[firmdata_df$year %in% 2007] <- firmdata_df$dmspols_2007[firmdata_df$year %in% 2007]
firmdata_df$dmspols[firmdata_df$year %in% 2009] <- firmdata_df$dmspols_2009[firmdata_df$year %in% 2009]
firmdata_df$dmspols[firmdata_df$year %in% 2011] <- firmdata_df$dmspols_2011[firmdata_df$year %in% 2011]
firmdata_df$dmspols[firmdata_df$year %in% 2013] <- firmdata_df$dmspols_2013[firmdata_df$year %in% 2013]

# Export -----------------------------------------------------------------------
#firmdata_df <- as.data.frame(firmdata_df)
saveRDS(firmdata_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms_clean.Rds"))

