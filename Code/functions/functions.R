# Extraction Functions ---------------------------------------------------------
extract_firm_stats <- function(year, polygon, firms, firm_name_suffix){
  
  #### Where are we?
  print(paste(year, "------------------------------------------------------"))
  
  #### Grab firm data in year i
  firms_i <- firms[firms$year %in% year,]
  
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

extract_dmspols <- function(year, polygon, country){
  print(paste(year, "------------------------------------------------------"))
  
  year_dmspols <- year
  
  # accounting for mexico in 2014
  if(year_dmspols > 2013){
    year_dmspols <- 2013
  }
  
  if(country %in% "canada"){
    dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("canada_dmspols_",year_dmspols,".tif")))
  }
  
  if(country %in% "mexico"){
    dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", paste0("mexico_dmspols_",year_dmspols,".tif")))
  }

  if(nrow(polygon) == 1){
    polygon$dmspols_mean <- mean(dmspols[], na.rm=T)
    polygon$dmspols_median <- median(dmspols[], na.rm=T)
    polygon$dmspols_sum <- sum(dmspols[], na.rm=T)
    
    polygon_data <- polygon@data
  } else{
    
    # velox extraction is slow with all of dmspols. So break into chunks.
    polygon_data <- lapply(unique(polygon$group), function(i){
      print(paste("velox", i))
      
      polygon_i <- polygon[polygon$group %in% i,]
      dmspols_i <- dmspols %>% crop(polygon_i) %>% velox()
      
      polygon_i$dmspol_mean <- dmspols_i$extract(sp=polygon_i, fun = function(x) mean(x, na.rm=T), small=T) %>% as.vector()
      polygon_i$dmspol_median <- dmspols_i$extract(sp=polygon_i, fun = function(x) median(x, na.rm=T), small=T) %>% as.vector()
      polygon_i$dmspol_sum <- dmspols_i$extract(sp=polygon_i, fun = function(x) sum(x, na.rm=T), small=T) %>% as.vector()
      
      return(polygon_i@data)
    }) %>%
      bind_rows()
    
  }
  
  polygon_data$year <- year
  
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

