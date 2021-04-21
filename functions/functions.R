# Load grid data without type

extract_coefs <- function(model){
  # Extract dataframe of coefficients and 95% CI from model
  
  ## Extract coefs
  df_coefs   <- tidy(model)
  
  ## Extract 95% CI
  df_confint <- confint(model) %>% as.data.frame() 
  df_confint$term <- row.names(df_confint) 
  
  ## Merge
  df_all <- merge(df_coefs, df_confint, by = "term")
  
  ## Subset and rename
  df_all <- df_all %>%
    dplyr::select("term", "estimate", "2.5 %", "97.5 %") %>%
    dplyr::rename("var" = "term",
                  "coef" = "estimate",
                  "ci2_5" = "2.5 %",
                  "ci97_5" = "97.5 %")
  
  return(df_all)
}

readRDS_exclude_type_vars <- function(filepath, year){
  print(filepath)
  
  df <- readRDS(filepath)
  #df <- df[,names(df) %in% c("unit", "year", 
  #                           "dmspols_sum_log", "dmspols_mean_log", 
  #                           "viirs_mean_log", "viirs_sum_log", "viirs_median_log",
  #                           "employment_sum_all_log", "N_firms_sum_all_log")]
  
  rm_vars <- names(df)[str_detect(names(df), "_([[:digit:]])([[:digit:]])_")]
  df <- df[,!(names(df) %in% rm_vars)]
  
  if(year != "all") df <- df[df$year %in% year,]
  
  return(df)
}

load_grid_data_no_type <- function(country, pattern, year, units = "all"){
  
  grid <- list.files(file.path(project_file_path, "Data", 
                               "Grid",
                               "FinalData",
                               country,
                               "merged_datasets"), pattern = pattern, full.names = T) %>%
    lapply(readRDS_exclude_type_vars, year=year) %>%
    bind_rows() %>%
    filter(!is.na(unit)) %>%
    mutate(unit = unit %>% 
             str_replace_all("hex_", "") %>% 
             str_replace_all("_dmspols", "") %>%
             str_replace_all("_viirs", "") %>%
             paste0(" Grid"))
  
  # No 250, 500 or 1000km grid
  grid <- grid[!(grid$unit %in% c("250km Grid", "500km Grid", "1000km Grid")),]
  
  grid$unit <- grid$unit %>% factor(levels = c("5km", "10km", "25km", "50km", "100km") %>%
                                      paste0(" Grid"))
  
  if(units != "all") grid <- grid[grid$unit %in% units,]
  
  return(grid)
}

# Extraction Functions ---------------------------------------------------------
collapse_firm_to_grid <- function(year, country_cap, r){
  print(year)
  
  # Determine Polygon ID -------------------------------------------------------
  firms_i <- readRDS(file.path(data_file_path, 
                               paste0(country_cap, " Industry Data"), "FinalData",
                               paste0("firms_", year, ".Rds")))
  
  # Cleanup
  firms_i$dmspols <- NULL
  firms_i$viirs <- NULL
  firms_i$viirs_corrected <- NULL
  firms_i$viirs_lead <- NULL
  firms_i$viirs_lead_corrected <- NULL
  firms_i$year <- NULL
  firms_i$empl_cat <- NULL
  firms_i$id <- NULL
  firms_i$naics2 <- firms_i$naics2 %>% as.character()
  
  if(class(r) %in% "SpatialPolygonsDataFrame"){
    firms_i$poly_id <- over_chunks(firms_i, r, "none", 100000)$id %>% as.vector()
    
  } else{
    firms_i$poly_id <- raster::extract(r, firms_i) %>% as.numeric()
  }
  
  firms_i <- firms_i[!is.na(firms_i$poly_id),]
  
  # Collapse -------------------------------------------------------------------
  ## Mean
  df_mean_all <- firms_i@data %>%
    group_by(poly_id) %>%
    summarise_if(is.numeric, mean, na.rm=T) %>%
    dplyr::mutate(naics2 = "all")
  
  df_mean_type <- firms_i@data %>%
    group_by(poly_id, naics2) %>%
    summarise_if(is.numeric, mean, na.rm=T)
  
  ## Sum
  firms_i$N_firms <- 1
  df_sum_all <- firms_i@data %>%
    group_by(poly_id) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    dplyr::mutate(naics2 = "all")
  
  df_sum_type <- firms_i@data %>%
    group_by(poly_id, naics2) %>%
    summarise_if(is.numeric, sum, na.rm=T)
  
  #### Append/Merge
  df_sum  <- bind_rows(df_sum_all,  df_sum_type)
  df_mean <- bind_rows(df_mean_all, df_mean_type)
  
  df_sum  <- df_sum  %>% rename_at(vars(-poly_id, -naics2), ~ paste0(., '_sum'))
  df_mean <- df_mean %>% rename_at(vars(-poly_id, -naics2), ~ paste0(., '_mean'))
  
  df_merged <- merge(df_sum, df_mean, by = c("poly_id", "naics2"), all = T)
  
  df_merged <- df_merged[!is.na(df_merged$naics2),]
  
  # Reshape --------------------------------------------------------------------
  df_merged_r <- df_merged %>%
    pivot_wider(names_from = naics2, values_from = -c(poly_id, naics2))
  df_merged_r$year <- year
  
  df_merged_r <- df_merged_r %>%
    dplyr::rename(id = poly_id)
  
  return(df_merged_r)
}

extract_firm_stats <- function(polygon, firms_i){
  
  #### Where are we?
  print(paste(year, "------------------------------------------------------"))
  
  #### Load Data
  #firms_i <- readRDS(file.path(path_to_firm_dir, paste0("firms_", year, ".Rds")))
  #if(!is.null(naics2_i)) firms_i <- firms_i[firms_i$naics2 %in% naics2_i,]
  
  # Remove all variables don't need to aggregate
  firms_i$id <- NULL
  #firms_i$naics2 <- NULL
  firms_i$naicsname <- NULL
  firms_i$dmspols <- NULL
  firms_i$viirs <- NULL
  firms_i$viirs_lead <- NULL
  firms_i$year <- NULL
  
  #### Remove character variables
  #for(var in names(firms_i)){
  #  if(is.character(firms_i[[var]][1])){
  #    firms_i[[var]] <- NULL
  #  }
  #}
  
  #### Grab firm data in year i
  #firms_i <- firms[firms$year %in% year,]
  #firms_i$year <- NULL # don't need to aggregate year variable
  
  #### Determine Polygon ID
  if(nrow(polygon) %in% 1){ # Country Level
    firms_i$poly_id <- 1
  } else{
    firms_i$poly_id <- over_chunks(firms_i, polygon, "none", 50000)$id
  }
  
  #### Collapse
  
  ## Mean
  df_mean_all <- firms_i@data %>%
    group_by(poly_id) %>%
    summarise_if(is.numeric, mean, na.rm=T) %>%
    dplyr::mutate(naics2 = "all")
  
  df_mean_type <- firms_i@data %>%
    group_by(poly_id, naics2) %>%
    summarise_if(is.numeric, mean, na.rm=T)
  
  ## Sum
  firms_i$N_firms <- 1
  df_sum_all <- firms_i@data %>%
    group_by(poly_id) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    dplyr::mutate(naics2 = "all")

  df_sum_type <- firms_i@data %>%
    group_by(poly_id, naics2) %>%
    summarise_if(is.numeric, sum, na.rm=T)
  
  #### Append/Merge
  df_sum  <- bind_rows(df_sum_all,  df_sum_type)
  df_mean <- bind_rows(df_mean_all, df_mean_type)
  
  df_sum  <- df_sum  %>% rename_at(vars(-poly_id, -naics2), ~ paste0(., '_sum'))
  df_mean <- df_mean %>% rename_at(vars(-poly_id, -naics2), ~ paste0(., '_mean'))
  
  df_merged <- merge(df_sum, df_mean, by = c("poly_id", "naics2"), all = T)

  df_merged <- df_merged[!is.na(df_merged$naics2),]
  
  return(df_merged)
}

extract_ntl <- function(year, polygon, country, ntl_type, suffix = ""){
  print(paste(year, "------------------------------------------------------"))
  
  year_ntl <- year
  
  if(ntl_type %in% c("dmspols", "dmspolsharmon")){
    # accounting for mexico in 2014
    if(year_ntl > 2013){
      year_ntl <- 2013
    }
  }
  
  if(ntl_type %in% c("dmspolszhang", "dmspolselvidge")){
    # accounting for mexico in 2014
    if(year_ntl > 2012){
      year_ntl <- 2012
    }
  }
  
  if(ntl_type %in% c("viirs", "viirs_corrected")){
    # accounting for mexico in 2014
    if(year_ntl < 2012){
      year_ntl <- 2012
    }
  }
  
  if(ntl_type %in% "dmspols"){
    ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", 
                              paste0(country,"_dmspols_",year_ntl,".tif")))
  }
  
  if(ntl_type %in% "dmspolsharmon"){
    country_iso <- country %>% substring(1,3) %>% toupper()
    
    ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED", 
                              paste0("Harmonized_DN_NTL_",year_ntl,"_calDMSP_",country_iso,".tif")))
    
    # if(year_ntl <= 2013){
    #   ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED", 
    #                             paste0("Harmonized_DN_NTL_",year_ntl,"_calDMSP_",country_iso,".tif")))
    # } 

  }
  
  if(ntl_type %in% "dmspolszhang"){
    ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_Zhang", "FinalData",
                              paste0(country,"_dmspolszhang_",year_ntl,".tif")))
  }
  
  if(ntl_type %in% "dmspolselvidge"){
    ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_Elvidge", "RawData",
                              paste0(country %>% substr(1,3),"_dmspolselvidge_",year_ntl,".tif")))
  }
  
  if(ntl_type %in% "viirs"){
    ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", 
                              paste0(country %>% substr(1,3),"_viirs_mean_",year_ntl,".tif")))
  }
  
  if(ntl_type %in% "viirs_corrected"){
    ntl_r <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", 
                              paste0(country %>% substr(1,3),"_viirs_corrected_mean_",year_ntl,".tif")))
  }
  
  
  if(nrow(polygon) == 1){
    polygon$ntl_mean   <- mean(ntl_r[], na.rm=T)
    polygon$ntl_median <- median(ntl_r[], na.rm=T)
    polygon$ntl_sum    <- sum(ntl_r[], na.rm=T)
    
    polygon_data <- polygon@data
  } else{
    
    # velox extraction is slow with all of dmspols. So break into chunks.
    polygon_data <- lapply(unique(polygon$group), function(i){
      print(paste("velox", i))
      
      polygon_i <- polygon[polygon$group %in% i,]
      ntl_r_i   <- ntl_r %>% crop(polygon_i) %>% velox()
      
      polygon_i$ntl_mean   <- ntl_r_i$extract(sp=polygon_i, fun = function(x) mean(x, na.rm=T), small=T) %>% as.vector()
      polygon_i$ntl_median <- ntl_r_i$extract(sp=polygon_i, fun = function(x) median(x, na.rm=T), small=T) %>% as.vector()
      polygon_i$ntl_sum    <- ntl_r_i$extract(sp=polygon_i, fun = function(x) sum(x, na.rm=T), small=T) %>% as.vector()
      
      return(polygon_i@data)
    }) %>%
      bind_rows()
    
  }
  
  polygon_data$year <- year
  
  names(polygon_data)[names(polygon_data) %in% "ntl_mean"]   <- paste0(ntl_type, "_mean",   suffix)
  names(polygon_data)[names(polygon_data) %in% "ntl_median"] <- paste0(ntl_type, "_median", suffix)
  names(polygon_data)[names(polygon_data) %in% "ntl_sum"]    <- paste0(ntl_type, "_sum",    suffix)
  
  return(polygon_data)
}

extract_firms_dmspols <- function(polygon, firms, firm_name_suffix){
  
  polygon_firms   <- lapply(FIRM_YEARS, extract_firm_stats, polygon, firms, firm_name_suffix) %>% bind_rows()
  polygon_dmspols <- lapply(FIRM_YEARS, extract_dmspols, polygon) %>% bind_rows()
  
  polygon_data <- merge(polygon_firms, polygon_dmspols, by=c("year", "id"))
  return(polygon_data)
}

# Creating Hexagons ------------------------------------------------------------
st_intersects_chunks <- function(sdf1, sdf2, chunk_size){
  
  starts <- seq(from=1,to=nrow(sdf1),by=chunk_size)
  
  st_intersects_i <- function(start, sdf1, sdf2, chunk_size){
    end <- min(start + chunk_size - 1, nrow(sdf1))
    out_i <- st_intersects(sdf1[start:end,], sdf2) %>% as.numeric()
    print(start)
    return(out_i)
  }
  
  out <- lapply(starts, st_intersects_i, sdf1, sdf2, chunk_size) %>% unlist %>% as.numeric
  
  return(out)
}

create_hexagons <- function(spdf, cellsize, keep_inter_coord, coords_buff_agg_sf){
  
  # Ensures hexagons cover whole area. If don't buffer, some areas might be 
  # left out due to irregular shapes
  spdf <- gBuffer_chunks(spdf, cellsize*1.25, 5000)
  
  hexagons <- spsample(spdf, type="hexagonal", cellsize=cellsize)
  HexPols <- HexPoints2SpatialPolygons(hexagons)
  HexPols$id <- 1:length(HexPols)
  
  if(keep_inter_coord){
    HexPols_sf <- HexPols %>% st_as_sf()
    
    print(paste(nrow(HexPols_sf), "----"))
    
    #inter <- lapply(1:nrow(HexPols_sf), function(i){
    #  print(i)
    #  st_intersects(HexPols_sf[i,], coords_buff_agg_sf) %>% as.numeric()
    #}) %>% unlist()
    
    inter <- st_intersects_chunks(HexPols_sf, coords_buff_agg_sf, 150)
    
    HexPols_sf <- HexPols_sf[inter %in% 1,]
    HexPols <- HexPols_sf %>% as("Spatial")
  }
  
  return(HexPols)
}

# Buffer and Remove Center -----------------------------------------------------
remove_center_polygon <- function(polygon, polygon_buff){
  # Uses polygon and polygon_buff as inputs. polygon_buff is a buffered version
  # of polygon. Removes the 'polygon' area from 'polygon_buff'
  
  lapply(1:nrow(polygon), function(i){
    if((i %% 100) %in% 0) print(paste0(i, " Erase Center"))
    
    raster::erase(polygon_buff[i,], polygon[i,])
  }) %>% 
    do.call(what = "rbind")
  
}

buffer_rm_center <- function(polygon,
                                 width,
                                 chunk_size = 1000){
  # Buffers polygon and removes original area
  
  polygon_buff <- gBuffer_chunks(polygon, width = width, chunk_size = chunk_size)
  polygon_buff_nocenter <- remove_center_polygon(polygon, polygon_buff)
  
  return(polygon_buff_nocenter)
}





