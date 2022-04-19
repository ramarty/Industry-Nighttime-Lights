# Merge Polygon Datasets

for(country in c("canada", "mexico")){
  
  #### Load Firms
  firms_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", 
                                "firms.Rds"))
  firms_df$id <- NULL # originally firm ID
  firms_df <- firms_df %>%
    dplyr::rename(id = unit_id)
  
  firms_df <- firms_df[firms_df$unit != "hexgrid8",]
  
  #### Load NTL
  ## Grab individual units
  units <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl") %>%
    list.files() %>%
    str_replace_all("_.*", "") %>%
    unique()
  
  units <- units[units != "hexgrid8"]
  
  ## Append NTL data
  ntl_df <- map_df(units, function(unit_i){
    print(unit_i)
    
    ## For each unit, merge all datasets (different ntl variables)
    out_df <- file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets", "ntl") %>%
      list.files(full.names = T,
                 pattern = "*.Rds") %>%
      str_subset(paste0("/", unit_i, "_")) %>%
      lapply(readRDS) %>%
      reduce(full_join, by = c("id", "year")) # Need full join as not all years in all satellites
    
    out_df$unit <- unit_i
    
    return(out_df)
  })
  
  #### Merge
  df <- left_join(ntl_df, firms_df, by = c("unit", "id", "year"))
  
  #### Fill in firm variables with NA
  df <- df %>%
    mutate_at(vars(contains("n_firms"), contains("employment")),
              ~replace_na(., 0))
  
  #### Export
  saveRDS(df, file.path(data_file_path, "Grid", "FinalData", country, "merged_datasets", "firm_ntl.Rds"))
}



