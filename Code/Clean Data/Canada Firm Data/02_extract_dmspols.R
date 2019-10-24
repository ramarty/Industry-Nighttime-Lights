# Extract DMSP-OLS

# TODO
# 1. Extract all years OR t, t-1, t-2, t+1, t+2...

# Load Data --------------------------------------------------------------------
coords_df <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "coordinates_appended.Rds"))
coordinates(coords_df) <- ~lon+lat
crs(coords_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract DMSPOLS --------------------------------------------------------------
#year <- 2000

for(year in 1992:2013){
  dmspols <- raster(file.path(raw_data_file_path, "Nighttime Lights", "DMSPOLS", paste0("canada_dmspols_", year, ".tif")))
  coords_df[[paste0("dmspols_", year)]] <- extract(dmspols, coords_df) %>% as.numeric
}

# Export -----------------------------------------------------------------------
saveRDS(coords_df, file.path(final_data_file_path, "Canada Industry Data", "coords_dmspols.Rds"))


