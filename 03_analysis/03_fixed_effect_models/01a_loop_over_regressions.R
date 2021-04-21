# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all")

mex_dmspols$year %>% table()
mex_viirs$year %>% table()
can$year %>% table()

# Estimate Models --------------------------------------------------------------
df_out_all <- data.frame(NULL)

for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  for(ntl_var in c("dmspolsharmon_mean_log", "dmspolselvidge_mean_log", "viirs_mean_log")){
    for(unit in c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")){
      for(country in c("mexico", "canada")){
        
        print(paste(industry_var, ntl_var, unit, country))
        
        ## Define data
        if(country %in% "canada") df <- can
        if(country %in% "mexico" & grepl("dmsp", ntl_var))  df <- mex_dmspols
        if(country %in% "mexico" & grepl("viirs", ntl_var)) df <- mex_viirs
        
        ## Add variables
        df$industry_var <- df[[industry_var]]
        df$ntl_var      <- df[[ntl_var]]
        
        ## Only keep if firms exist
        df <- df %>%
          group_by(id) %>%
          mutate(industry_var_max = max(industry_var)) %>%
          ungroup() %>%
          filter(industry_var_max > 0)
        
        ## Regressions
        lm_yr   <- felm(industry_var ~ ntl_var | year      | 0 | 0, data = df[df$unit %in% unit,]) %>% extract_coefs() %>% mutate(FE = "yr") 
        lm_id   <- felm(industry_var ~ ntl_var | id        | 0 | 0, data = df[df$unit %in% unit,]) %>% extract_coefs() %>% mutate(FE = "id") 
        lm_yrid <- felm(industry_var ~ ntl_var | year + id | 0 | 0, data = df[df$unit %in% unit,]) %>% extract_coefs() %>% mutate(FE = "year_id") 
        
        df_out <- bind_rows(lm_id,
                            lm_yr,
                            lm_yrid) %>%
          mutate(industry_var = industry_var,
                 ntl_var = ntl_var,
                 unit = unit,
                 country = country)
        
        df_out_all <- bind_rows(df_out_all, df_out)
        
      }
    }
  }
}

# Regression Results -----------------------------------------------------------
saveRDS(df_out_all, file.path(data_file_path, "Results", "regression_results_yrid.Rds"))





