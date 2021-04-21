# Summarize data in polygons

# LOOP OVER COUNTRY ------------------------------------------------------------
for(country in c("canada", "mexico")){
  print(paste(country, "-----------------------------------------------------"))
  
  ## Prep Parmaeters
  country_cap <- capitalize(country)
  
  if(country %in% "canada") FIRM_YEARS <- FIRM_YEARS_CANADA
  if(country %in% "mexico") FIRM_YEARS <- FIRM_YEARS_MEXICO
  
  grid_files <- list.files(file.path(data_file_path, "Grid", "RawData"), pattern = "*.Rds") %>%
    str_replace_all(".Rds", "") %>%
    str_subset(country %>% substring(1,3)) %>%
    stri_subset_fixed("raster", negate = TRUE) 
  
  # LOOP OVER GRID -------------------------------------------------------------
  for(grid_i in grid_files){
    print(paste(grid_i, "----------------------------------------------------"))
    
    # If hexagon (not city), use raster approach
    if(!grepl("city", grid_i)) grid_i <- paste0(grid_i, "_raster") 
    
    r <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0(grid_i, ".Rds")))
    
    df_all <- lapply(FIRM_YEARS, collapse_firm_to_grid, country_cap, r) %>%
      bind_rows()
    
    grid_i_name <- grid_i %>% str_replace_all("_raster", "") %>% paste0("_firms")
    saveRDS(df_all, file.path(data_file_path, "Grid", "FinalData", country, 
                              "individual_datasets",
                              paste0(grid_i_name, ".Rds")))
  }
}


