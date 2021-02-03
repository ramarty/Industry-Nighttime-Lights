# Summarize data in polygons

for(country in c("mexico", "canada")){
  print(paste(country, "-----------------------------------------------------"))
  
  ## Prep Parmaeters
  country_cap <- capitalize(country)
  
  if(country %in% "canada") FIRM_YEARS <- FIRM_YEARS_CANADA
  if(country %in% "mexico") FIRM_YEARS <- FIRM_YEARS_MEXICO
  
  grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
    str_replace_all(".Rds", "") %>%
    str_subset(country %>% substring(1,3)) %>%
    str_subset("_raster")
  
  for(grid_i in grid_files){
    print(paste(grid_i, "----------------------------------------------------"))
    
    r <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0(grid_i, ".Rds")))
    
    df_all <- lapply(FIRM_YEARS, collapse_firm_to_grid, country_cap, r) %>%
      bind_rows()
    
    saveRDS(df_all, file.path(data_file_path, "Grid", "FinalData", country, 
                              "individual_datasets",
                              paste0(grid_i %>% 
                                       str_replace_all("_raster",
                                                       "_firms"), 
                                     ".Rds")))
  }
}

