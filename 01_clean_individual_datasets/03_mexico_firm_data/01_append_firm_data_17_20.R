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

## Use empl_med in years without employment
firmdata_df$employment[!(firmdata_df$year %in% 2019)] <- firmdata_df$empl_med[!(firmdata_df$year %in% 2019)]

## Rename/Mutate
firmdata_df <- firmdata_df %>%
  dplyr::rename(naics6   = codigo_act,
                lat      = latitud,
                lon      = longitud,
                empl_cat = per_ocu) %>%
  dplyr::mutate(naics2 = naics6 %>% substring(1,2) %>% as.numeric()) %>%
  dplyr::select(-naics6)

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
saveRDS(firmdata_df, file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_17_20.Rds"))






