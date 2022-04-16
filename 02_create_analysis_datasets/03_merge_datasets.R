# Merge Polygon Datasets

for(country in c("canada", "mexico")){
  
  #### Load Firms
  firms_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", 
                                "firms.Rds"))
  firms_df$id <- NULL # originally firm ID
  firms_df <- firms_df %>%
    dplyr::rename(id = unit_id)
  
  #### Load NTL
  ## Grab individual units
  units <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl") %>%
    list.files() %>%
    str_replace_all("_.*", "") %>%
    unique()
  
  ## Append data
  ntl_df <- map_df(units, function(unit_i){
    print(unit_i)
    
    ## For each unit, merge all datasets (different ntl variables)
    out_df <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl") %>%
      list.files(full.names = T,
                 pattern = "*.Rds") %>%
      str_subset(paste0("/", unit_i, "_")) %>%
      lapply(readRDS) %>%
      reduce(left_join, by = c("id", "year"))
    
    out_df$unit <- unit_i
    
    return(out_df)
  })
  
  #### Merge
  df <- merge(ntl_df, firms_df, by = c("unit", "id", "year"))

  #### Export
  saveRDS(df, file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets", "firm_ntl.Rds"))
}

