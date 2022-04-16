# Summarize firm data to datasets

# Firm datasets are big, to reduce computational burden import one year at a time
# then append together

# LOOP OVER COUNTRY ------------------------------------------------------------
for(country in c("canada", "mexico")){
  
  # Prep country info ----------------------------------------------------------
  country_cap <- capitalize(country)
  if(country %in% "canada") FIRM_YEARS <- FIRM_YEARS_CANADA
  if(country %in% "mexico") FIRM_YEARS <- FIRM_YEARS_MEXICO
  
  # Process data ---------------------------------------------------------------
  firm_append_df <- map_df(FIRM_YEARS, function(year){
    print(paste(country, year))
    
    #### Load data
    firm_df <- readRDS(file.path(data_file_path, paste0(country_cap, " Industry Data"), 
                                 "FinalData", 
                                 paste0("firms_", year, ".Rds")))
    
    #### Pivot, so panel with unit
    firm_long_df <- firm_df %>%
      pivot_longer(cols = c(hexgrid1, hexgrid2, hexgrid3, hexgrid4, hexgrid5, 
                            hexgrid6, hexgrid7, hexgrid8),
                   names_to = "unit",
                   values_to = "unit_id")
    
    #### Aggregate
    ## Across all units
    firm_sum_df <- firm_long_df %>%
      group_by(unit, unit_id) %>%
      dplyr::summarise(n_firms = n(),
                       employment = sum(employment, na.rm = T)) %>%
      ungroup() %>%
      mutate_at(vars(n_firms, employment), ~replace_na(., 0)) 
    
    ## By naics/firm type
    firm_sum_type_df <- firm_long_df %>%
      group_by(unit, unit_id, naics2) %>%
      dplyr::summarise(n_firms = n(),
                       employment = sum(employment, na.rm = T)) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(unit, unit_id),
                  names_from = naics2,
                  values_from = c(n_firms, employment)) %>%
      mutate_at(vars(contains("n_firms"), contains("employment")), ~replace_na(., 0)) 
    
    ## Merge
    firms_merge_df <- firm_sum_df %>%
      left_join(firm_sum_type_df, by = c("unit", "unit_id"))
    
    ## Add year
    firms_merge_df$year <- year
    
    return(firms_merge_df)
  })
  
  # Export ---------------------------------------------------------------------
  saveRDS(firm_append_df, file.path(data_file_path, "Grid", "FinalData", country, "individual_datasets",
                                    "firms.Rds"))
}

