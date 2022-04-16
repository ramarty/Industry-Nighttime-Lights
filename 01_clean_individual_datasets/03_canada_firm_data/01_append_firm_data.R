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

# Restrict Variables to Only Needed Ones ---------------------------------------
firmdata_df@data <- firmdata_df@data %>%
  dplyr::select(year, employment, naics2, naicsname)

# Remove Outliers --------------------------------------------------------------
firmdata_df$employment[(firmdata_df$naicsname %in% "Educational Services") &
                         firmdata_df$employment %in% 1000000] <- NA

# Fix naics --------------------------------------------------------------------
# https://en.wikipedia.org/wiki/North_American_Industry_Classification_System
firmdata_df$naics2[firmdata_df$naics2 %in% 41] <- 42 # Wholesale Trade (41 in Canada,[3] 42 in the United States[2]);
firmdata_df$naicsname[firmdata_df$naics2 %in% 42] <- "Wholesale Trade"

firmdata_df$naics2[firmdata_df$naics2 %in% 91] <- 92 # (91 in the United States, 92 in Canada[4]);
firmdata_df$naicsname[firmdata_df$naics2 %in% 92] <- "Public Administration"

#### Simplify
firmdata_df$naics2[firmdata_df$naics2 %in% 21:22] <- 21
firmdata_df$naics2[firmdata_df$naics2 %in% 31:33] <- 31
firmdata_df$naics2[firmdata_df$naics2 %in% 41:43] <- 42
firmdata_df$naics2[firmdata_df$naics2 %in% 44:46] <- 44
firmdata_df$naics2[firmdata_df$naics2 %in% 48:49] <- 48
firmdata_df$naics2[firmdata_df$naics2 %in% 51:56] <- 51
firmdata_df$naics2[firmdata_df$naics2 %in% 61:62] <- 61
firmdata_df$naics2[firmdata_df$naics2 %in% 71:72] <- 71
firmdata_df$naics2[firmdata_df$naics2 %in% 81:92] <- 81

# Add hexagon ids --------------------------------------------------------------
firmdata_df <- firmdata_df %>% as.data.frame()
firmdata_sf <- st_as_sf(firmdata_df, coords = c("lon", "lat"), crs = 4326)

for(i in 1:8){
  print(i)
  firmdata_sf[[paste0("hexgrid", i)]] <- point_to_h3(firmdata_sf, res = i)
}

firmdata_df <- firmdata_sf
firmdata_df$geometry <- NULL

# Export -----------------------------------------------------------------------
saveRDS(firmdata_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

