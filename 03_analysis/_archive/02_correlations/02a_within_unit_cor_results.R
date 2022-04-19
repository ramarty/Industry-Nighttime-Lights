# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                                 "mexico", "merged_clean_appended_allunits", "mex_dmspols_notype.Rds")) 

mex_viirs <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                               "mexico", "merged_clean_appended_allunits", "mex_viirs_notype.Rds")) 

can <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData",
                         "canada", "merged_clean_appended_allunits", "can_notype.Rds")) 

# Correlation ------------------------------------------------------------------
country <- "Canada"
ntl_var <- "dmspolsharmon_mean"
year <- "2001"
unit <- "25km Grid"
firm_var <- "employment_sum_all"
transform <- "log"

df_out_all <- data.frame(NULL)
for(country in c("Canada", "Mexico")){
  for(ntl_var in c("dmspolsharmon_mean", "viirs_mean")){
    
    ## Grab dataset
    if(country %in% "Canada")                             df <- can
    if(country %in% "Mexico" & grepl("dmspols", ntl_var)) df <- mex_dmspols
    if(country %in% "Mexico" & grepl("viirs", ntl_var))   df <- mex_viirs
    
    ## Grab units to loop through
    UNITS <- df$unit %>% unique() %>% as.character()
    
    for(unit in UNITS){
      for(firm_var in c("employment_sum_all", 
                        "N_firms_sum_all")){
        for(transform in c("log")){
          print(paste(country, unit, transform, ntl_var, firm_var, sep = " - "))
          
          ## Define base data and variables
          df_temp    <- df
          ntl_var_i  <- ntl_var
          firm_var_i <- firm_var
          
          ## Check for skips
          if(is.null(df_temp[[ntl_var_i]]))  next
          if(is.null(df_temp[[firm_var_i]])) next
          if(country %in% "Mexico" & firm_var == "employment_sum_all") next
          if(country %in% "Canada" & firm_var == "viirs_sum")          next
          
          ## Subset by unit
          df_temp <- df_temp[(df_temp$unit %in% unit),]
          
          ## Transform
          if(transform != "level"){
            ntl_var_splag_i  <- paste0(ntl_var_i, "_splagunit_", transform)
            firm_var_splag_i <- paste0(firm_var_i, "_mean_splagunit_", transform)
            
            ntl_var_i  <- paste0(ntl_var_i, "_", transform)
            firm_var_i <- paste0(firm_var_i, "_", transform)
          } else{
            ntl_var_splag_i  <- paste0(ntl_var_i, "_splagunit")
            firm_var_splag_i <- paste0(firm_var_i, "_mean_splagunit")
          }
          
          ## Add variables to dataframe 
          df_temp$ntl_var  <- df_temp[[ntl_var_i]]
          df_temp$firm_var <- df_temp[[firm_var_i]]
          
          df_temp$ntl_var_splag  <- df_temp[[ntl_var_splag_i]]
          df_temp$firm_var_splag <- df_temp[[firm_var_splag_i]]
          
          ## Remove NAs
          df_temp <- df_temp %>%
            filter(!is.na(ntl_var),
                   !is.na(firm_var))
          
          ## Remove cells if no firms at any point in time
          df_temp <- df_temp %>% 
            dplyr::group_by(id) %>%
            dplyr::mutate(firm_var_max = max(firm_var)) %>%
            ungroup() %>%
            dplyr::filter(firm_var_max > 0)
          
          ## Correlation
          if(nrow(df_temp) > 1){
            
            df_out <- df_temp %>%
              group_by(id) %>%
              dplyr::summarise(cor = cor(ntl_var, firm_var),
                               
                               ntl_var_min = min(ntl_var),
                               ntl_var_max = max(ntl_var),
                               ntl_var_sd = sd(ntl_var),
                               
                               ntl_var_splag_min= min(ntl_var_splag),
                               ntl_var_splag_max = max(ntl_var_splag),
                               ntl_var_splag_sd = sd(ntl_var_splag),
                               
                               firm_var_min = min(firm_var),
                               firm_var_max = max(firm_var),
                               firm_var_sd = sd(firm_var),
                               
                               firm_var_splag_min = min(firm_var_splag),
                               firm_var_splag_max = max(firm_var_splag),
                               firm_var_splag_sd = sd(firm_var_splag))
            
            df_out$country <- country
            df_out$ntl_var <- ntl_var
            df_out$unit <- unit
            df_out$firm_var <- firm_var
            df_out$transform <- transform
            
            df_out_all <- bind_rows(df_out, df_out_all)
            
          }
        }
      }
    }
  }
}


saveRDS(df_out_all, file.path(data_file_path, "Results", "correlation_within_unit.Rds"))




