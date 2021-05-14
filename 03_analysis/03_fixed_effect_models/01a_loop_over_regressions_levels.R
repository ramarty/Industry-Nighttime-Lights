# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                                 "mexico", "merged_appended_allunits", "mex_dmspols_notype.Rds")) 

mex_viirs <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                               "mexico", "merged_appended_allunits", "mex_viirs_notype.Rds")) 

can <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData",
                         "canada", "merged_appended_allunits", "can_notype.Rds")) 

mex_dmspols$year %>% table()
mex_viirs$year %>% table()
can$year %>% table()

# Estimate Models --------------------------------------------------------------
industry_var = "N_firms_sum_all_log"
ntl_var = "dmspolsharmon_mean_log"
unit = "Grid in Cities"
country = "canada"

df_out_all <- data.frame(NULL)

long_diff <- F

for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  for(ntl_var in c("dmspolsharmon_mean_log", "viirs_mean_log")){
    for(unit in c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid", "City" ,"Grid in Cities")){
      for(country in c("mexico", "canada")){
        
        print(paste(long_diff, industry_var, ntl_var, unit, country))
        
        ## Define data
        if(country %in% "canada") df <- can
        if(country %in% "mexico" & grepl("dmsp", ntl_var))  df <- mex_dmspols
        if(country %in% "mexico" & grepl("viirs", ntl_var)) df <- mex_viirs
        
        ## Add variables
        df$industry_var <- df[[industry_var]]
        df$ntl_var      <- df[[ntl_var]]
        
        df$industry_var_splag <- df[[paste0(industry_var %>% str_replace_all("_log", ""), "_sum_splagunit_log")]]
        df$ntl_var_splag      <- df[[paste0(ntl_var      %>% str_replace_all("_log", ""), "_splagunit_log")]]
        
        df <- df[df$unit %in% unit,]
        
        ## Only keep if firms exist
        if(unit != "Grid in Cities"){
          df <- df %>%
            group_by(id) %>%
            mutate(industry_var_max = max(industry_var, na.rm = T)) %>%
            ungroup() %>%
            filter(industry_var_max > 0)
        }
        
        ## Regressions - Industry DV
        dvindust_lm_yr   <- felm(industry_var ~ ntl_var | year      | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "yr", splag = F) 
        dvindust_lm_id   <- felm(industry_var ~ ntl_var | id        | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "id", splag = F) 
        dvindust_lm_yrid <- felm(industry_var ~ ntl_var | year + id | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "year_id", splag = F) 
        
        dvindust_lag_lm_yr   <- felm(industry_var ~ ntl_var + ntl_var_splag | year      | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "yr", splag = T) 
        dvindust_lag_lm_id   <- felm(industry_var ~ ntl_var + ntl_var_splag | id        | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "id", splag = T) 
        dvindust_lag_lm_yrid <- felm(industry_var ~ ntl_var + ntl_var_splag | year + id | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "year_id", splag = T) 
        
        dvindust_out <- bind_rows(dvindust_lm_yr,
                                  dvindust_lm_id,
                                  dvindust_lm_yrid,
                                  dvindust_lag_lm_yr,
                                  dvindust_lag_lm_id,
                                  dvindust_lag_lm_yrid) %>%
          mutate(industry_var = industry_var,
                 ntl_var = ntl_var,
                 unit = unit,
                 country = country,
                 dv_type = "industry",
                 long_diff = long_diff)
        
        ## Regressions - NTL DV
        dvntl_lm_yr   <- felm(ntl_var ~ industry_var | year      | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "yr", splag = F) 
        dvntl_lm_id   <- felm(ntl_var ~ industry_var | id        | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "id", splag = F) 
        dvntl_lm_yrid <- felm(ntl_var ~ industry_var | year + id | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "year_id", splag = F) 
        
        dvntl_lag_lm_yr   <- felm(ntl_var ~ industry_var + industry_var_splag | year      | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "yr", splag = T) 
        dvntl_lag_lm_id   <- felm(ntl_var ~ industry_var + industry_var_splag | id        | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "id", splag = T) 
        dvntl_lag_lm_yrid <- felm(ntl_var ~ industry_var + industry_var_splag | year + id | 0 | 0, data = df) %>% extract_coefs() %>% mutate(FE = "year_id", splag = T) 
        
        dvntl_out <- bind_rows(dvntl_lm_yr,
                               dvntl_lm_id,
                               dvntl_lm_yrid,
                               dvntl_lag_lm_yr,
                               dvntl_lag_lm_id,
                               dvntl_lag_lm_yrid) %>%
          mutate(industry_var = industry_var,
                 ntl_var = ntl_var,
                 unit = unit,
                 country = country,
                 dv_type = "ntl",
                 long_diff = long_diff)
        
        df_out_all <- bind_rows(df_out_all, dvindust_out, dvntl_out)
        
      }
    }
  }
}


# Regression Results -----------------------------------------------------------
saveRDS(df_out_all, file.path(data_file_path, "Results", "regression_results_yrid.Rds"))





