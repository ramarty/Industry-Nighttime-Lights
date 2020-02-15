# Summarize data in hexagons

# Take average nighttime lights and firm level data within hexagons

# Load Data --------------------------------------------------------------------
hexagons <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "hexagon_data", "hexagons.Rds"))

firmdata_df <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "points_data", "firms_points_data.Rds"))

firmdata_df <- firmdata_df %>%
  dplyr::select(year, scottsid, employment, lon, lat) %>%
  mutate(N = 1)

coordinates(firmdata_df) <- ~lon+lat
crs(firmdata_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Firm Stats in Hexagons -------------------------------------------------------
hex_firm_stat_df <- lapply(c(2001, 2003, 2005, 2007, 2009, 2011, 2013), function(year){
  print(paste(year, "------------------------------------------------------"))
  
  firmdata_df_i <- firmdata_df[firmdata_df$year %in% year,]
  
  hexagons_OVER_firm_mean <- over_chunks(hexagons, firmdata_df_i, "mean", 2000) 
  names(hexagons_OVER_firm_mean) <- paste0(names(hexagons_OVER_firm_mean), "_mean")
  
  hexagons_OVER_firm_sum <- over_chunks(hexagons, firmdata_df_i, "sum", 2000) 
  names(hexagons_OVER_firm_sum) <- paste0(names(hexagons_OVER_firm_sum), "_sum")
  
  hexagons_OVER_firm <- bind_cols(hexagons_OVER_firm_mean, 
                                  hexagons_OVER_firm_sum)
  hexagons_OVER_firm$id <- hexagons$id
  hexagons_OVER_firm$year <- year
  
  return(hexagons_OVER_firm)
}) %>% bind_rows()

# DMSP-OLS Stats in Hexagons ---------------------------------------------------
hexagons_dmspols_df <- lapply(c(2001, 2003, 2005, 2007, 2009, 2011, 2013), function(year){
  print(paste(year, "------------------------------------------------------"))
  
  hexagons$year <- year
  dmspols <- raster(file.path(raw_data_file_path, "Nighttime Lights", "DMSPOLS", paste0("canada_dmspols_",year,".tif")))
  hexagons$dmspols_mean <- velox(dmspols)$extract(sp=hexagons, fun = function(x) mean(x, na.rm=T))
  hexagons$dmspols_median <- velox(dmspols)$extract(sp=hexagons, fun = function(x) median(x, na.rm=T))
  
  return(hexagons@data)
}) %>% bind_rows()

# Merge Data -------------------------------------------------------------------
hexagon_data <- merge(hex_firm_stat_df, hexagons_dmspols_df, by=c("id", "year"))

# Export -----------------------------------------------------------------------
saveRDS(hexagon_data, file.path(final_data_file_path, "Canada Industry Data", "hexagon_data", "hexagons_data.Rds"))





head(hexagon_data)

