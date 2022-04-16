# Crop DMSP-OLS Harmonized by Country
# Prevents loading full raster at once

for(country in c("canada", "mexico")){
  
  ## Load country
  if(country %in% "canada"){
    country_sp <- readRDS(file.path(data_file_path, "GADM", "FinalData", "canada_chull", "canada_chull.Rds"))
  } 
  
  if(country %in% "mexico"){
    country_sp <- readRDS(file.path(data_file_path, "GADM", "FinalData", "blank_files", "gadm36_MEX_0_sp.rds"))
  } 
  
  for(year in c(1992:2021)){
    print(paste(country, year))
    
    ## Load raster
    if(year <= 2013){
      r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED",
                            "RawData",
                            paste0("Harmonized_DN_NTL_",year,"_calDMSP.tif")))
    } else{
      r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED",
                            "RawData",
                            paste0("Harmonized_DN_NTL_",year,"_simVIIRS.tif")))
    }
    
    ## Crop
    r_crop <- crop(r, country_sp)
    
    ## Export
    saveRDS(r_crop, file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED",
                              "FinalData", paste0(country, "_dmspols_harmon_", year, ".Rds")))
    
  }
}

