# Clean Firm Data

# Load Data --------------------------------------------------------------------
firms_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

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

# Restrict Variables to Only Needed Ones ---------------------------------------
# Large dataset, so helps with efficiency

firms_df@data <- firms_df@data %>%
  dplyr::select(year, employment, dmspols, naics2, naicsname)

# Remove Outliers --------------------------------------------------------------
firms_df$employment[(firms_df$naicsname %in% "Educational Services") &
                      firms_df$employment %in% 1000000] <- NA

# Fix naics --------------------------------------------------------------------
# https://en.wikipedia.org/wiki/North_American_Industry_Classification_System
firms_df$naics2[firms_df$naics2 %in% "41"] <- "42" # Wholesale Trade (41 in Canada,[3] 42 in the United States[2]);
firms_df$naicsname[firms_df$naics2 %in% "42"] <- "Wholesale Trade"

firms_df$naics2[firms_df$naics2 %in% "91"] <- "92" # (91 in the United States, 92 in Canada[4]);
firms_df$naicsname[firms_df$naics2 %in% "92"] <- "Public Administration"

# Export Data ------------------------------------------------------------------
saveRDS(firms_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms_clean.Rds"))
