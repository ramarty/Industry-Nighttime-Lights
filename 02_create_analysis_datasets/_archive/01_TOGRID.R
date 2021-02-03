# Summarize data in polygons

firms <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_17_20.Rds"))
firms <- as.data.frame(firms)

firms_coords <- firms %>%
  distinct(lat, lon)

firms_coords$idtemp <- 1:nrow(firms_coords)
coordinates(firms_coords) <- ~lon+lat
crs(firms_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


over_all_chunks <- function(sdf1,sdf2,chunk_size,mc.cores=1){
  starts <- seq(from=1,to=nrow(sdf1),by=chunk_size)
  
  over_i <- function(start, sdf1, sdf2, chunk_size){
    end <- min(start + chunk_size - 1, nrow(sdf1))
    
    out <- sp::over(sdf1[start:end,], sdf2, fn=function(x) sum(x, na.rm=T))

    print(start)
    return(out)
  }
  
  if(mc.cores > 1){
    library(parallel)
    out_all <- pbmclapply(starts, over_i, sdf1, sdf2, chunk_size, mc.cores=mc.cores) %>% unlist
  } else{
    out_all <- lapply(starts, over_i, sdf1, sdf2, chunk_size) %>% unlist
  }
  
  return(out_all)
}



