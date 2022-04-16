# Identify Clusters of Lights

canada <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_CAN_0_sp.rds"))
mexico <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_MEX_0_sp.rds"))

NTL_RAW_DIR <- file.path(data_file_path, "Nighttime Lights", "DMSPOLS_Zhang", "RawData")
NTL_FINAL_DIR <- file.path(data_file_path, "Nighttime Lights", "DMSPOLS_Zhang", "FinalData")

extract_dmspols <- function(year, country, country_suffix, NTL_RAW_DIR, NTL_FINAL_DIR){
  print(year)
  
  r_paths <- list.files(NTL_RAW_DIR, 
                        full.names = T) %>%
    str_subset(year)
  
  if(length(r_paths) %in% 2){
    r1 <- raster(r_paths[1]) %>% crop(country)
    r2 <- raster(r_paths[2]) %>% crop(country)
    
    r <- r1
    r[] <- (r1[] + r2[])/2
  } else{
    r <- raster(r_paths) %>% crop(country)
  }
  
  r[] <- r[]*0.01
  
  writeRaster(r, file.path(NTL_FINAL_DIR, paste0(country_suffix,"_dmspolszhang_",  year, ".tif")))
  return(NULL)
}

lapply(as.character(1992:2012),
       extract_dmspols,
       mexico,
       "mexico",
       NTL_RAW_DIR, 
       NTL_FINAL_DIR)


lapply(as.character(1992:2012),
       extract_dmspols,
       canada,
       "canada",
       NTL_RAW_DIR, 
       NTL_FINAL_DIR)

