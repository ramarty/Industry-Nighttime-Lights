# Spatial lag of firms data

# NOTE: If there are no nearby units, will not return a row for this. However,
# this is fine. When merging with the full dataset, these values will be "NA";
# which we then convert to 0.

# LOOP OVER COUNTRY ------------------------------------------------------------
for(country in c("canada", "mexico")){
  
  grids <- file.path(data_file_path, "Grid", "FinalData", country, 
                     "individual_datasets") %>%
    list.files(pattern = "firms.Rds")
  
  # LOOP OVER UNITS ------------------------------------------------------------
  for(grid_i in grids){
    ## Load Data
    df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country, 
                            "individual_datasets", grid_i))
    
    ## Load Coordinates
    sp_coords <- readRDS(file.path(data_file_path, "Grid", "RawData", grid_i %>% str_replace_all("_firms", ""))) 
    df_coords <- coordinates(sp_coords) %>%
      as.data.frame()
    names(df_coords) <- c("lon", "lat")
    df_coords$id <- sp_coords$id
    
    ## Merge Data with Coordintes
    df <- merge(df, df_coords, by = "id")
    
    ## Only select relevant variables
    df <- df %>%
      dplyr::select(id, year, lat, lon, contains("all"))
    
    ## Deterimine buffer to use (for spatial lag)
    if(grepl("city", grid_i)) buffer_sizes <- c("1km", "2km", "5km", "10km")
    
    if(grepl("hex", grid_i)){
      unit_size <- grid_i %>% str_replace_all("mex_hex_|can_hex_", "") %>% str_replace_all("_firms.Rds", "")
      buffer_sizes <- unit_size
    }
    
    ## Replace NAs with 0s
    df <- df %>%
      dplyr::mutate_at(vars(contains("firm"),
                            contains("employ")), replace_na, 0)
    
    # LOOP OVER BUFFERS --------------------------------------------------------
    for(buffer_i in buffer_sizes){
      print(paste(country, "//", grid_i, "//", buffer_i, "-------------------"))
      
      if(grepl("city", grid_i)) suffix_name <- paste0("_splag", buffer_i)
      if(grepl("hex",  grid_i)) suffix_name <- paste0("_splag", "unit")
      
      buffer_i_value <- buffer_i %>% str_replace_all("km", "") %>% as.numeric
      
      ## Buffer values
      print(paste("TOTAL IDS:", length(unique(df$id))))
      df_splag <- map_df(unique(df$id), function(id_i){
        if((id_i %% 100) %in% 0) print(id_i)
        
        # contains multiple years, so keep/remove this way
        df_i    <- df[df$id %in% id_i,]
        df_noti <- df[!(df$id %in% id_i),] 
        
        dist <- sqrt((df_i$lat - df_noti$lat)^2 + (df_i$lon - df_noti$lon)^2)
        df_noti_col <- suppressMessages(
          df_noti[dist < buffer_i_value*1000,] %>%
            dplyr::select(year, 
                          contains("firm"),
                          contains("employ")) %>%
            group_by(year) %>%
            dplyr::summarise_all(list(mean = mean, sum = sum)) %>%
            rename_at(vars(-year),function(x) paste0(x,suffix_name))
        )
        
        df_noti_col$id <- id_i
        
        return(df_noti_col)
      })
      
      ## Export 
      saveRDS(df_splag,
              file.path(data_file_path, "Grid", "FinalData", country, 
                        "individual_datasets",
                        grid_i %>% str_replace_all("firms.Rds", 
                                                   paste0("firms_splag_",buffer_i,".Rds"))))
      
      
    }
    
  }
}

