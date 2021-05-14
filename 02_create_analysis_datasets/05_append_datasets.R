# Append Datasets Across Units
# Ignore firm type

# For each country, append data across different units - so panel between
# (1) unit-type, (2) unit, and (3) year. For city-grid units, there's a separate
# unit for DMSP-OLS and VIIRS (different original grid size). For Canada, for
# city-grid-dmsp, we make all VIIRS variables NA (and vice versa for VIIRS). For
# Mexico, we have separate datasets for DMSP and VIIRS, and we just remove the
# relevant unit. In addition, defines correct spatial lag variable

# Function for Loading Merged Data ---------------------------------------------
append_data_no_type <- function(country, pattern){
  # Append data, but ignore employment variables with "type"
  
  grid <- list.files(file.path(project_file_path, "Data", 
                               "Grid",
                               "FinalData",
                               country,
                               "merged_datasets"), pattern = pattern, full.names = T) %>%
    lapply(readRDS_exclude_type_vars, year="all") %>%
    bind_rows() %>%
    filter(!is.na(unit)) %>%
    mutate(unit = unit %>% 
             str_replace_all("hex_", "") %>% 
             str_replace_all("_dmspols", "") %>%
             str_replace_all("_viirs", "") %>%
             paste0(" Grid")) %>%
    mutate(unit = case_when(
      unit %in% "city Grid" ~ "City",
      unit %in% "citygriddmsp Grid" ~ "Grid in Cities [DMSP]",
      unit %in% "citygridviirs Grid" ~ "Grid in Cities [VIIRS]",
      TRUE ~ unit
    ))
  
  # No 250, 500 or 1000km grid
  grid <- grid[!(grid$unit %in% c("250km Grid", "500km Grid", "1000km Grid")),]
  
  return(grid)
}

clean_slag <- function(df){
  # Create standardized variable for spatial lag
  
  df_clean <- lapply(unique(df$unit), function(unit_i){
    
    print(unit_i)
    df_i <- df[df$unit %in% unit_i,]
    
    if(grepl("km Grid", unit_i)){

      rm_var <- names(df_i) %>% str_subset("splag[:digit:]km|splag[:digit:][:digit:]km|splag[:digit:][:digit:][:digit:]km") 
      for(var in rm_var) df_i[[var]] <- NULL
      
    } else{
    
      if(unit_i %in% "City")                   lag_name <- "splag5km" # "1km", "2km", "5km", "10km"
      if(unit_i %in% "Grid in Cities [VIIRS]") lag_name <- "splag1km" # "1km", "2km", "5km", "10km"
      if(unit_i %in% "Grid in Cities [DMSP]")  lag_name <- "splag1km" # "1km", "2km", "5km", "10km"
      if(unit_i %in% "Grid in Cities")         lag_name <- "splag1km" # "1km", "2km", "5km", "10km"
      
      rm_unit_var <- names(df_i) %>% str_subset("splagunit") 
      for(var in rm_unit_var) df_i[[var]] <- NULL
      
      names(df_i) <- names(df_i) %>% str_replace_all(lag_name, "splagunit")
      
      rm_km_var <- names(df_i) %>% str_subset("splag[:digit:]km|splag[:digit:][:digit:]km|splag[:digit:][:digit:][:digit:]km") 
      for(var in rm_km_var) df_i[[var]] <- NULL
    }
    
    return(df_i)
  }) %>%
    bind_rows()
  
  return(df_clean)
}

# Canada -----------------------------------------------------------------------
can <- append_data_no_type("canada", "*_clean.Rds") %>% clean_slag()

dmsp_vars  <- can %>% names %>% str_subset("dmsp")
viirs_vars <- can %>% names %>% str_subset("viirs")

for(var in dmsp_vars)  can[[var]][can$unit %in% "Grid in Cities [VIIRS]"] <- NA
for(var in viirs_vars) can[[var]][can$unit %in% "Grid in Cities [DMSP]"]  <- NA

can$unit[can$unit %in% c("Grid in Cities [VIIRS]", "Grid in Cities [DMSP]")] <- "Grid in Cities"

can$unit <- can$unit %>% factor(levels = c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid",
                                           "City",
                                           "Grid in Cities"))

saveRDS(can, file.path(project_file_path, "Data", 
                       "Grid",
                       "FinalData",
                       "canada",
                       "merged_appended_allunits",
                       "can_notype.Rds"))

# Mexico: DMSP -----------------------------------------------------------------
mex_dmspols <- append_data_no_type("mexico", "*_dmspols_clean.Rds") %>% clean_slag()

mex_dmspols <- mex_dmspols %>% dplyr::filter(unit != "Grid in Cities [VIIRS]")

mex_dmspols$unit[mex_dmspols$unit %in% c("Grid in Cities [VIIRS]", "Grid in Cities [DMSP]")] <- "Grid in Cities"

mex_dmspols$unit <- mex_dmspols$unit %>% factor(levels = c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid",
                                                           "City",
                                                           "Grid in Cities"))

saveRDS(mex_dmspols, file.path(project_file_path, "Data", 
                               "Grid",
                               "FinalData",
                               "mexico",
                               "merged_appended_allunits",
                               "mex_dmspols_notype.Rds"))

# Mexico: VIIRS ----------------------------------------------------------------
mex_viirs <- append_data_no_type("mexico", "*_viirs_clean.Rds") %>% clean_slag()

mex_viirs <- mex_viirs %>% dplyr::filter(unit != "Grid in Cities [DMSP]")

mex_viirs$unit[mex_viirs$unit %in% c("Grid in Cities [VIIRS]", "Grid in Cities [DMSP]")] <- "Grid in Cities"

mex_viirs$unit <- mex_viirs$unit %>% factor(levels = c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid",
                                                       "City",
                                                       "Grid in Cities"))

saveRDS(mex_viirs, file.path(project_file_path, "Data", 
                             "Grid",
                             "FinalData",
                             "mexico",
                             "merged_appended_allunits",
                             "mex_viirs_notype.Rds"))


