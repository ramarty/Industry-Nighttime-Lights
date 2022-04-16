# Append Mexico Firm Data

# Load Data --------------------------------------------------------------------
firmdata_df <- read_csv(file.path(data_file_path, "Mexico Industry Data", "RawData", "mexicofirm_04_14.csv"))

firmdata_df <- firmdata_df %>%
  dplyr::rename(employment = empl) %>%
  dplyr::select(year, employment, naics2, lon, lat) # naicsname 

# Employment Category ----------------------------------------------------------
firmdata_df$empl_med <- NA
firmdata_df$empl_med[firmdata_df$employment %in% 0:5]     <- 2.5
firmdata_df$empl_med[firmdata_df$employment %in% 6:10]    <- 8
firmdata_df$empl_med[firmdata_df$employment %in% 11:30]   <- 20.5
firmdata_df$empl_med[firmdata_df$employment %in% 31:50]   <- 40.5
firmdata_df$empl_med[firmdata_df$employment %in% 51:100]  <- 75.5
firmdata_df$empl_med[firmdata_df$employment %in% 101:250] <- 175.5
firmdata_df$empl_med[firmdata_df$employment >= 251]       <- 300

## Factor Variable
firmdata_df$empl_med_fact <- firmdata_df$empl_med %>% as.factor() %>% as.numeric()

# Simplify naics ---------------------------------------------------------------
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
firmdata_df <- firmdata_df 
firmdata_sf <- st_as_sf(firmdata_df, coords = c("lon", "lat"), crs = 4326)

for(i in 1:8){
  print(i)
  firmdata_sf[[paste0("hexgrid", i)]] <- point_to_h3_chunks(firmdata_sf, res = i, 1000000)
}

firmdata_df <- firmdata_sf
firmdata_df$geometry <- NULL

# Export -----------------------------------------------------------------------
saveRDS(firmdata_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_04_14.Rds"))


