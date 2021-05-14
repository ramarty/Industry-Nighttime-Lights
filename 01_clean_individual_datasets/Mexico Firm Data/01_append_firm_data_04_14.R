# Append Canada Firm Data

# firmdata_df <- read_csv(file.path(data_file_path, "Mexico Industry Data", "RawData", "mexicofirm_04_14.csv"))
# 
# a <- firmdata_df %>%
#   dplyr::group_by(code, naics6, year, naicsname) %>%
#   dplyr::summarise(empl_min = min(empl),
#                    empl_max = max(empl),
#                    empl_sd  = sd(empl),
#                    N = n())
# 
# firmdata_df$cat <- paste(firmdata_df$code, firmdata_df$naics6)
# 
# a <- firmdata_df[firmdata_df$cat %in% "30011 114119",]
# 
# code_df <- firmdata_df[firmdata_df$code %in% 30011,]
# n_df <- firmdata_df[firmdata_df$naics6 %in% 114119,]

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

# Spatially Define -------------------------------------------------------------
coordinates(firmdata_df) <- ~lon+lat
crs(firmdata_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract DMSPOLS --------------------------------------------------------------
firmdata_df$dmspols <- NA
dmspols_04 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",2004,".tif")))
dmspols_09 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",2009,".tif")))
dmspols_13 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",2013,".tif")))

firmdata_df$dmspols[firmdata_df$year %in% 2004] <- raster::extract(dmspols_04, firmdata_df[firmdata_df$year %in% 2004,]) %>% as.numeric()
firmdata_df$dmspols[firmdata_df$year %in% 2009] <- raster::extract(dmspols_09, firmdata_df[firmdata_df$year %in% 2009,]) %>% as.numeric()
firmdata_df$dmspols[firmdata_df$year %in% 2014] <- raster::extract(dmspols_13, firmdata_df[firmdata_df$year %in% 2014,]) %>% as.numeric()

# Extract VIIRS ----------------------------------------------------------------
firmdata_df$viirs <- NA
viirs_14 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_mean_",2014,".tif")))
viirs_14c <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_corrected_mean_",2014,".tif")))

firmdata_df$viirs[firmdata_df$year %in% 2014] <- raster::extract(viirs_14, firmdata_df[firmdata_df$year %in% 2014,]) %>% as.numeric()
firmdata_df$viirs_corrected[firmdata_df$year %in% 2014] <- raster::extract(viirs_14c, firmdata_df[firmdata_df$year %in% 2014,]) %>% as.numeric()

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

# Export -----------------------------------------------------------------------
saveRDS(firmdata_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_04_14.Rds"))


