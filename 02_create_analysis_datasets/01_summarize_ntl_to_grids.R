# Summarize data in polygons

EXTRACT_DMSPOLS         <- T
EXTRACT_DMSPOLSZHANG    <- T
EXTRACT_DMSPOLSELVIDGE  <- T
EXTRACT_DMSPOLSHARMON   <- T
EXTRACT_VIIRS           <- T
EXTRACT_VIIRS_CORRECTED <- T

# LOOP OVER COUNTRY ------------------------------------------------------------
for(country in c("canada", "mexico")){
  
  ## Prep Parmaeters
  country_cap <- capitalize(country)
  
  if(country %in% "canada") FIRM_YEARS <- FIRM_YEARS_CANADA
  if(country %in% "mexico") FIRM_YEARS <- FIRM_YEARS_MEXICO
  
  grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
    stri_subset_fixed("raster", negate = TRUE)  %>% # remove "_raster.Rds"
    str_replace_all(".Rds", "") %>%
    str_subset(country %>% substring(1,3)) 
  
  OUT_PATH <- file.path(data_file_path, "Grid", "FinalData", country,  "individual_datasets")
  
  # LOOP OVER GRID -------------------------------------------------------------
  for(grid_i in grid_files){

    # Deterimine buffer to use (for spatial lag)
    if(grepl("city", grid_i)) buffer_sizes <- c("none", "1km", "2km", "5km", "10km")
    
    if(grepl("hex", grid_i)){
      unit_size <- grid_i %>% str_replace_all("mex_hex_|can_hex_", "")
      buffer_sizes <- c("none", unit_size)
    }
    
    # LOOP OVER BUFFER ---------------------------------------------------------
    for(buffer_i in buffer_sizes){
      print(paste(buffer_i, grid_i, country, "-------------------------------"))
      
      suffix_name <- ""
      polygon <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0(grid_i, ".Rds")))
      
      if(buffer_i != "none"){
        if(grepl("city", grid_i)) suffix_name <- paste0("_splag", buffer_i)
        if(grepl("hex",  grid_i)) suffix_name <- paste0("_splag", "unit")
        
        buffer_i_value <- buffer_i %>% str_replace_all("km", "") %>% as.numeric
        
        polygon <- buffer_rm_center(polygon, width = buffer_i_value*1000)
      }
      
      # Add group variable
      polygon <- spTransform(polygon, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      polygon$group <- 1

      ## DMSPOLS - ZHANG
      print("dmspols zhang -----------------------------------------------------")
      if(EXTRACT_DMSPOLSZHANG){
        polygon_dmspols_z <- lapply(FIRM_YEARS[FIRM_YEARS <= 2014], extract_ntl, polygon, country, "dmspolszhang", suffix_name) %>% bind_rows()
        saveRDS(polygon_dmspols_z, file.path(OUT_PATH, paste0(grid_i,"_dmspolszhang_",".Rds")))
      }
      
      ## DMSPOLS - ELVIDGE
      print("dmspols elvidge ---------------------------------------------------")
      if(EXTRACT_DMSPOLSELVIDGE){
        polygon_dmspols_e <- lapply(FIRM_YEARS[FIRM_YEARS <= 2014], extract_ntl, polygon, country, "dmspolselvidge", suffix_name) %>% bind_rows()
        saveRDS(polygon_dmspols_e, file.path(OUT_PATH, paste0(grid_i,"_dmspolselvidge_",".Rds")))
      }
      
      ## DMSPOLS - HARMON
      print("dmspols harmon ---------------------------------------------------")
      if(EXTRACT_DMSPOLSHARMON){
        polygon_dmspols_h <- lapply(FIRM_YEARS[FIRM_YEARS <= 2018], extract_ntl, polygon, country, "dmspolsharmon", suffix_name) %>% bind_rows()
        saveRDS(polygon_dmspols_h, file.path(OUT_PATH, paste0(grid_i,"_dmspolsharmon_",".Rds")))
      }
      
      ## VIIRS
      print("viirs -------------------------------------------------------------")
      if(EXTRACT_VIIRS){
        polygon_viirs <- lapply(FIRM_YEARS[FIRM_YEARS >= 2011], extract_ntl, polygon, country, "viirs", suffix_name) %>% bind_rows()
        saveRDS(polygon_viirs, file.path(OUT_PATH, paste0(grid_i,"_viirs",".Rds")))
      }
      
      ## VIIRS Corrected
      print("viirs corrected ---------------------------------------------------")
      if(EXTRACT_VIIRS_CORRECTED & (country %in% "mexico")){
        polygon_viirs_c <- lapply(FIRM_YEARS[FIRM_YEARS >= 2014], extract_ntl, polygon, country, "viirs_corrected", suffix_name) %>% bind_rows()
        saveRDS(polygon_viirs_c, file.path(OUT_PATH, paste0(grid_i,"_viirs_corrected",".Rds")))
      }
      
    }
  }
}

