# Append Canada Firm Data

# Load Data --------------------------------------------------------------------
firmdata_df <- list.files(file.path(data_file_path, "Mexico Industry Data", "RawData"), 
                          pattern = "*.csv",
                          full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows

# Spatially Define -------------------------------------------------------------
coordinates(firmdata_df) <- ~lon+lat
crs(firmdata_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract NTL ------------------------------------------------------------------

for(year in c(2004, 2009, 2013)){
  print(year)
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_", year, ".tif")))
  firmdata_df[[paste0("dmspols_", year)]] <- extract(dmspols, firmdata_df) %>% as.numeric
  
  #velox(dmspols)$extract(sp=firmdata_df, fun = function(x) mean(x, na.rm=T))
}

firmdata_df$dmspols <- NA
firmdata_df$dmspols[firmdata_df$year %in% 2004] <- firmdata_df$dmspols_2004[firmdata_df$year %in% 2004]
firmdata_df$dmspols[firmdata_df$year %in% 2009] <- firmdata_df$dmspols_2009[firmdata_df$year %in% 2009]
firmdata_df$dmspols[firmdata_df$year %in% 2014] <- firmdata_df$dmspols_2013[firmdata_df$year %in% 2014]

# Export -----------------------------------------------------------------------
#firmdata_df <- as.data.frame(firmdata_df)
saveRDS(firmdata_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms.Rds"))

