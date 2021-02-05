# Summarize data in polygons

EXTRACT_DMSPOLS         <- T
EXTRACT_VIIRS           <- T
EXTRACT_VIIRS_CORRECTED <- T

for(country in c("canada", "mexico")){
  
  ## Prep Parmaeters
  country_cap <- capitalize(country)
  
  if(country %in% "canada") FIRM_YEARS <- FIRM_YEARS_CANADA
  if(country %in% "mexico") FIRM_YEARS <- FIRM_YEARS_MEXICO
  
  grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
    str_subset("km.Rds") %>% # remove "_raster.Rds"
    str_replace_all(".Rds", "") %>%
    str_subset(country %>% substring(1,3)) 
  
  OUT_PATH <- file.path(data_file_path, "Grid", "FinalData", country,  "individual_datasets")
  
  for(grid_i in grid_files){
    print(paste(grid_i, "----------------------------------------------------"))
    
    ## Load Grid
    polygon <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0(grid_i, ".Rds")))
    polygon <- spTransform(polygon, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    polygon$group <- 1
    
    ## DMSPOLS
    print("dmspols -----------------------------------------------------------")
    if(EXTRACT_DMSPOLS){
      polygon_dmspols <- lapply(FIRM_YEARS[FIRM_YEARS <= 2014], extract_ntl, polygon, country, "dmspols") %>% bind_rows()
      saveRDS(polygon_dmspols, file.path(OUT_PATH, paste0(grid_i,"_dmspols",".Rds")))
    }
    
    ## VIIRS
    print("viirs -------------------------------------------------------------")
    if(EXTRACT_VIIRS){
      polygon_viirs <- lapply(FIRM_YEARS[FIRM_YEARS >= 2011], extract_ntl, polygon, country, "viirs") %>% bind_rows()
      saveRDS(polygon_viirs, file.path(OUT_PATH, paste0(grid_i,"_viirs",".Rds")))
    }
    
    ## VIIRS Corrected
    print("viirs corrected ---------------------------------------------------")
    if(EXTRACT_VIIRS_CORRECTED & country %in% "mexico"){
      polygon_viirs_c <- lapply(FIRM_YEARS[FIRM_YEARS >= 2014], extract_ntl, polygon, country, "viirs_corrected") %>% bind_rows()
      saveRDS(polygon_viirs_c, file.path(OUT_PATH, paste0(grid_i,"_viirs_corrected",".Rds")))
    }
    
  }
}

