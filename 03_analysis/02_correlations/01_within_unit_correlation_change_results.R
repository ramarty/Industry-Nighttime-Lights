# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all")

# Only use DMSP-Harmon data in the time frame
#can$dmspolsharmon_mean[can$year > 2013] <- NA
#mex_viirs$dmspolsharmon_mean[mex_viirs$year > 2013] <- NA
#mex_dmspols$dmspolsharmon_mean[mex_dmspols$year > 2013] <- NA

# Correlation ------------------------------------------------------------------
country <- "Canada"
ntl_var <- "dmspolsharmon_mean"
year <- "2001"
unit <- "25km Grid"
firm_var <- "employment_sum_all"
transform <- "log"

df_out_all <- data.frame(NULL)
for(country in c("Canada", "Mexico")){
  for(ntl_var in c("dmspols_mean", "dmspolsharmon_mean", "viirs_mean")){
    
    ## Grab dataset
    if(country %in% "Canada")                             df <- can
    if(country %in% "Mexico" & grepl("dmspols", ntl_var)) df <- mex_dmspols
    if(country %in% "Mexico" & grepl("viirs", ntl_var))   df <- mex_viirs
    
    ## Grab units to loop through
    UNITS <- df$unit %>% unique() %>% as.character()
    
    for(unit in UNITS){
      for(firm_var in c("employment_sum_all", "firms_sum_all",
                        "empl_med_sum_all", "N_firms_sum_all")){
        for(transform in c("level", "log")){
          print(paste(year, country, unit, transform, ntl_var, firm_var, sep = " - "))
          
          ## Define base data and variables
          df_temp    <- df
          ntl_var_i  <- ntl_var
          firm_var_i <- firm_var
          
          ## Check for skips
          if(is.null(df_temp[[ntl_var_i]]))  next
          if(is.null(df_temp[[firm_var_i]])) next
          
          ## Subset by unit
          df_temp <- df_temp[(df_temp$unit %in% unit),]
          
          ## Transform
          if(transform != "level"){
            ntl_var_i  <- paste0(ntl_var_i, "_", transform)
            firm_var_i <- paste0(firm_var_i, "_", transform)
          } 
          
          ## Add variables to dataframe 
          df_temp$ntl_var  <- df_temp[[ntl_var_i]]
          df_temp$firm_var <- df_temp[[firm_var_i]]
          
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
                               firm_var_min = min(firm_var),
                               firm_var_max = max(firm_var))
            
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




