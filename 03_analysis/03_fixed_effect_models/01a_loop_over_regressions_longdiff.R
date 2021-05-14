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
unit = "5km Grid"
country = "mexico"

df_out_all <- data.frame(NULL)

long_diff <- T

for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  for(ntl_var in c("dmspolsharmon_mean_log", "viirs_mean_log")){
    for(unit in c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid", "City" ,"Grid in Cities")){
      for(country in c("mexico", "canada")){
        
        print(paste(long_diff, industry_var, ntl_var, unit, country))
        
        ## Define data
        if(country %in% "canada") df <- can
        if(country %in% "mexico" & grepl("dmsp", ntl_var))  df <- mex_dmspols
        if(country %in% "mexico" & grepl("viirs", ntl_var)) df <- mex_viirs
        
        ## Make long difference
        if(country %in% "canada" & grepl("dmsp", ntl_var))  diff_lag <- 6
        if(country %in% "canada" & grepl("viirs", ntl_var)) diff_lag <- 1
        
        if(country %in% "mexico" & grepl("dmsp", ntl_var))  diff_lag <- 2
        if(country %in% "mexico" & grepl("viirs", ntl_var)) diff_lag <- 6
        
        ## Add variables
        df$industry_var <- df[[paste0(industry_var, "_diff", diff_lag)]]
        df$ntl_var      <- df[[paste0(ntl_var,      "_diff", diff_lag)]]
        
        df$industry_var_splag <- df[[paste0(industry_var %>% str_replace_all("_log", ""), "_sum_splagunit_log", "_diff", diff_lag)]]
        df$ntl_var_splag      <- df[[paste0(ntl_var      %>% str_replace_all("_log", ""), "_splagunit_log",     "_diff", diff_lag)]]
        
        df <- df[df$unit %in% unit,] 
        
        ## Only keep if firms exist
        if(unit != "Grid in Cities"){
          df <- df %>%
            group_by(id) %>%
            mutate(industry_var_max = max(industry_var, na.rm=T)) %>%
            ungroup() %>%
            filter(industry_var_max > 0)
        }
        
        ## Regressions - Industry DV
        dvindust_lm     <- lm(industry_var ~ ntl_var,                 data = df) %>% extract_coefs() %>% mutate(splag = F) 
        dvindust_lag_lm <- lm(industry_var ~ ntl_var + ntl_var_splag, data = df) %>% extract_coefs() %>% mutate(splag = T) 
        
        dvindust_out <- bind_rows(dvindust_lm,
                                  dvindust_lag_lm) %>%
          mutate(industry_var = industry_var,
                 ntl_var = ntl_var,
                 unit = unit,
                 country = country,
                 dv_type = "industry",
                 long_diff = long_diff)
        
        ## Regressions - NTL DV
        dvntl_lm     <- lm(ntl_var ~ industry_var ,                      data = df) %>% extract_coefs() %>% mutate(splag = F) 
        dvntl_lag_lm <- lm(ntl_var ~ industry_var + industry_var_splag , data = df) %>% extract_coefs() %>% mutate(splag = T) 
        
        dvntl_out <- bind_rows(dvntl_lm,
                               dvntl_lag_lm) %>%
          mutate(industry_var = industry_var,
                 ntl_var = ntl_var,
                 unit = unit,
                 country = country,
                 dv_type = "ntl",
                 long_diff = long_diff)
        
        df_out_all <- bind_rows(df_out_all, dvindust_out, dvntl_out)
        print(head(df_out_all))
        
      }
    }
  }
}


# Regression Results -----------------------------------------------------------
saveRDS(df_out_all, file.path(data_file_path, "Results", "regression_results_longdiff.Rds"))





