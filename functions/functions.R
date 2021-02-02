# Extraction Functions ---------------------------------------------------------
extract_firm_stats <- function(year, polygon, path_to_firm_dir, naics2_i, firm_name_suffix){
  
  #### Where are we?
  print(paste(year, "------------------------------------------------------"))
  
  #### Load Data
  firms_i <- readRDS(file.path(path_to_firm_dir, paste0("firms_", year, ".Rds")))
  if(!is.null(naics2_i)) firms_i <- firms_i[firms_i$naics2 %in% naics2_i,]
  
  # Remove all variables don't need to aggregate
  firms_i$id <- NULL
  firms_i$naics2 <- NULL
  firms_i$naicsname <- NULL
  firms_i$dmspols <- NULL
  firms_i$viirs <- NULL
  firms_i$viirs_lead <- NULL
  firms_i$year <- NULL
  
  #### Remove character variables
  for(var in names(firms_i)){
    if(is.character(firms_i[[var]][1])){
      firms_i[[var]] <- NULL
    }
  }
  
  #### Grab firm data in year i
  #firms_i <- firms[firms$year %in% year,]
  #firms_i$year <- NULL # don't need to aggregate year variable
  
  #### Country Level
  if(nrow(polygon) %in% 1){
    
    polygon_OVER_firm_mean <- apply(firms_i@data, 2, mean, na.rm=T) %>% t %>% as.data.frame()
    polygon_OVER_firm_sum <- apply(firms_i@data, 2, sum, na.rm=T) %>% t %>% as.data.frame()
    
    polygon_OVER_firm_mean$year <- NULL
    polygon_OVER_firm_sum$year <- NULL
    
  } else{
    polygon_OVER_firm_mean <- over_chunks(polygon, firms_i, "mean", 100) 
    polygon_OVER_firm_sum <- over_chunks(polygon, firms_i, "sum", 100) 
  }
  
  names(polygon_OVER_firm_mean) <- paste0(names(polygon_OVER_firm_mean), "_mean")
  names(polygon_OVER_firm_sum) <- paste0(names(polygon_OVER_firm_sum), "_sum")
  
  polygon_OVER_firm <- bind_cols(polygon_OVER_firm_mean, 
                                 polygon_OVER_firm_sum)
  names(polygon_OVER_firm) <- paste0(names(polygon_OVER_firm), firm_name_suffix)
  
  polygon_OVER_firm$id <- polygon$id
  polygon_OVER_firm$year <- year
  
  return(polygon_OVER_firm)
}

extract_ntl <- function(year, polygon, country, ntl_type){
  print(paste(year, "------------------------------------------------------"))
  
  year_ntl <- year
  
  if(ntl_type %in% "dmspols"){
    # accounting for mexico in 2014
    if(year_ntl > 2013){
      year_ntl <- 2013
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
  
  names(polygon_data)[names(polygon_data) %in% "ntl_mean"]   <- paste0(ntl_type, "_mean")
  names(polygon_data)[names(polygon_data) %in% "ntl_median"] <- paste0(ntl_type, "_median")
  names(polygon_data)[names(polygon_data) %in% "ntl_sum"]    <- paste0(ntl_type, "_sum")
  
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

