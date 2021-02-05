# Append Canada Firm Data

# Load Data --------------------------------------------------------------------
firmdata_17 <- file.path(data_file_path, "Mexico Industry Data", "RawData", "DENUE 2017") %>%
  list.files(pattern = "*.csv", 
             full.names = T,
             recursive = T) %>%
  str_subset("conjunto_de_datos") %>%
  #head(2) %>%
  lapply(function(f){
    df <- read_csv(f) %>%
      dplyr::select(id, codigo_act, per_ocu, latitud, longitud)
    return(df)
  }) %>%
  bind_rows() %>%
  dplyr::mutate(year = 2017) 

firmdata_18 <- file.path(data_file_path, "Mexico Industry Data", "RawData", "DENUE 2018") %>%
  list.files(pattern = "*.csv", 
             full.names = T,
             recursive = T) %>%
  str_subset("conjunto_de_datos") %>%
  #head(2) %>%
  lapply(function(f){
    df <- read_csv(f) %>%
      dplyr::select(id, codigo_act, per_ocu, latitud, longitud)
    return(df)
  }) %>%
  bind_rows() %>%
  dplyr::mutate(year = 2018)

# 2019 provides exact employee numbers
firmdata_19 <- file.path(data_file_path, "Mexico Industry Data", "RawData", "DENUE 2019", "denue_ce") %>%
  list.files(pattern = "*.csv", 
             full.names = T) %>%
  #head(2) %>%
  lapply(function(f){
    df <- read_csv(f) %>%
      dplyr::select(id, codigo_act, per_ocu, latitud, longitud, H001A) 
    return(df)
  }) %>%
  bind_rows() %>%
  dplyr::mutate(year = 2019) %>%
  dplyr::rename(employment = H001A)

firmdata_20 <- file.path(data_file_path, "Mexico Industry Data", "RawData", "DENUE 2020") %>%
  list.files(pattern = "*.csv", 
             full.names = T,
             recursive = T) %>%
  str_subset("conjunto_de_datos") %>%
  #head(2) %>%
  lapply(function(f){
    df <- read_csv(f) %>%
      dplyr::select(id, codigo_act, per_ocu, latitud, longitud)
    return(df)
  }) %>%
  bind_rows() %>%
  dplyr::mutate(year = 2020) 

# Append and Clean Data --------------------------------------------------------
firmdata_df <- bind_rows(firmdata_17,
                         firmdata_18,
                         firmdata_19,
                         firmdata_20)

## Assign Employment Number
firmdata_df$empl_med <- NA
firmdata_df$empl_med[firmdata_df$per_ocu %in% "0 a 5 personas"]     <- 2.5
firmdata_df$empl_med[firmdata_df$per_ocu %in% "6 a 10 personas"]    <- 8
firmdata_df$empl_med[firmdata_df$per_ocu %in% "11 a 30 personas"]   <- 20.5
firmdata_df$empl_med[firmdata_df$per_ocu %in% "31 a 50 personas"]   <- 40.5
firmdata_df$empl_med[firmdata_df$per_ocu %in% "51 a 100 personas"]  <- 75.5
firmdata_df$empl_med[firmdata_df$per_ocu %in% "101 a 250 personas"] <- 175.5
firmdata_df$empl_med[firmdata_df$per_ocu %in% "251 y más personas"] <- 300
firmdata_df$empl_med[grepl("251", firmdata_df$per_ocu)]             <- 300 # 251 y m\xe1s personas

## Factor Variable
firmdata_df$empl_med_fact <- firmdata_df$empl_med %>% as.factor() %>% as.numeric()

## Rename/Mutate
firmdata_df <- firmdata_df %>%
  dplyr::rename(naics6   = codigo_act,
                lat      = latitud,
                lon      = longitud,
                empl_cat = per_ocu) %>%
  dplyr::mutate(naics2 = naics6 %>% substring(1,2) %>% as.numeric()) %>%
  dplyr::select(-naics6)

# Spatially Define -------------------------------------------------------------
coordinates(firmdata_df) <- ~lon+lat
crs(firmdata_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Extract VIIRS ----------------------------------------------------------------
firmdata_df$viirs <- NA
viirs_17 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_mean_",2017,".tif")))
viirs_18 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_mean_",2018,".tif")))
viirs_19 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_mean_",2019,".tif")))
viirs_20 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_mean_",2020,".tif")))

firmdata_df$viirs[firmdata_df$year %in% 2017] <- raster::extract(viirs_17, firmdata_df[firmdata_df$year %in% 2017,]) %>% as.numeric()
firmdata_df$viirs[firmdata_df$year %in% 2018] <- raster::extract(viirs_18, firmdata_df[firmdata_df$year %in% 2018,]) %>% as.numeric()
firmdata_df$viirs[firmdata_df$year %in% 2019] <- raster::extract(viirs_19, firmdata_df[firmdata_df$year %in% 2019,]) %>% as.numeric()
firmdata_df$viirs[firmdata_df$year %in% 2020] <- raster::extract(viirs_20, firmdata_df[firmdata_df$year %in% 2020,]) %>% as.numeric()

# Extract VIIRS - Corrected ----------------------------------------------------
firmdata_df$viirs_corrected <- NA
viirs_17c <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_corrected_mean_",2017,".tif")))
viirs_18c <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_corrected_mean_",2018,".tif")))
viirs_19c <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_corrected_mean_",2019,".tif")))
viirs_20c <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", paste0("mex_viirs_corrected_mean_",2020,".tif")))

firmdata_df$viirs_corrected[firmdata_df$year %in% 2017] <- raster::extract(viirs_17c, firmdata_df[firmdata_df$year %in% 2017,]) %>% as.numeric()
firmdata_df$viirs_corrected[firmdata_df$year %in% 2018] <- raster::extract(viirs_18c, firmdata_df[firmdata_df$year %in% 2018,]) %>% as.numeric()
firmdata_df$viirs_corrected[firmdata_df$year %in% 2019] <- raster::extract(viirs_19c, firmdata_df[firmdata_df$year %in% 2019,]) %>% as.numeric()
firmdata_df$viirs_corrected[firmdata_df$year %in% 2020] <- raster::extract(viirs_20c, firmdata_df[firmdata_df$year %in% 2020,]) %>% as.numeric()

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
saveRDS(firmdata_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_17_20.Rds"))






