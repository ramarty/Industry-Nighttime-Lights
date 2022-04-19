# Analysis

# Cor function -----------------------------------------------------------------
# Account for errors

cor.test.naerror <- function(x, y){
  x_len <- length(x[!is.na(x)])
  y_len <- length(y[!is.na(y)])
  
  #print(x_len)
  #print(y_len)
  
  if(x_len >= 3 & y_len >= 3){
    out <- cor.test(x, y)$estimate
  } else{
    out <- NA
  }
  
  return(out)
}



# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                                 "mexico", "merged_appended_allunits", "mex_dmspols_notype.Rds")) %>%
  dplyr::filter(unit %in% c("Grid in Cities")) 

mex_viirs <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                               "mexico", "merged_appended_allunits", "mex_viirs_notype.Rds")) %>%
  dplyr::filter(unit %in% c("Grid in Cities")) 

can <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData",
                         "canada", "merged_appended_allunits", "can_notype.Rds")) %>%
  dplyr::filter(unit %in% c("Grid in Cities"))

# Estimate Models --------------------------------------------------------------
country <- "mexico"
industry_var = "employment_sum_all_log"
ntl_var = "dmspolsharmon_mean_log"

cor_all_df <- data.frame(NULL)

for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  for(ntl_var in c("dmspolsharmon_mean_log", "viirs_mean_log")){
    for(country in c("mexico", "canada")){
      
      print(paste(industry_var, ntl_var, country))
      
      ## Select dataset
      if(country %in% "canada")                           df <- can
      if(country %in% "mexico" & grepl("dmsp", ntl_var))  df <- mex_dmspols
      if(country %in% "mexico" & grepl("viirs", ntl_var)) df <- mex_viirs
      
      ## Select variables
      df$ntl_var <- df[[ntl_var]]
      df$industry_var <- df[[industry_var]]
      
      ## Subset
      df <- df %>%
        group_by(city_name) %>%
        dplyr::mutate(industry_var_sum = sum(industry_var, na.rm=T),
                      ntl_var_sum = sum(ntl_var, na.rm=T)) %>%
        ungroup() %>%
        dplyr::filter(industry_var_sum > 0,
                      ntl_var_sum > 0)
      
      ##### Levels -------------------------------------------------------------
      
      ## Oneyear dataset (for correlation across levels)
      if(country %in% "canada") df_onyear <- df[df$year %in% 2013,]
      if(country %in% "mexico") df_onyear <- df[df$year %in% 2014,]
      
      ## Correlation across levels
      results_levels_df <- df_onyear %>%
        group_by(city_name) %>%
        dplyr::summarise(cor = cor.test.naerror(ntl_var, industry_var),
                         N = n(),
                         industry_var_mean = mean(industry_var_sum, na.rm=T),
                         ntl_var_mean = mean(ntl_var, na.rm=T),
                         industry_var_sd = sd(industry_var, na.rm=T),
                         ntl_var_sd = sd(ntl_var, na.rm=T)) %>%
        dplyr::mutate(industry_var = industry_var,
                      ntl_var = ntl_var,
                      country = country,
                      type = "cor_levels_within")
      
      #### Long difference -----------------------------------------------------
      ## Make long difference
      if(country %in% "canada" & grepl("dmsp", ntl_var))  diff_lag <- 6
      if(country %in% "canada" & grepl("viirs", ntl_var)) diff_lag <- 1
      
      if(country %in% "mexico" & grepl("dmsp", ntl_var))  diff_lag <- 2
      if(country %in% "mexico" & grepl("viirs", ntl_var)) diff_lag <- 6
      
      df$industry_var_ld <- df[[paste0(industry_var, "_diff", diff_lag)]]
      df$ntl_var_ld      <- df[[paste0(ntl_var,      "_diff", diff_lag)]]
      
      ## Correlation across long difference
      results_ld_df <- df %>%
        group_by(city_name) %>%
        dplyr::summarise(cor = cor.test.naerror(ntl_var_ld, industry_var_ld),
                         N = n(),
                         industry_var_mean = mean(industry_var_sum, na.rm=T),
                         ntl_var_mean = mean(ntl_var, na.rm=T),
                         industry_var_sd = sd(industry_var, na.rm=T),
                         ntl_var_sd = sd(ntl_var, na.rm=T),
                         
                         industry_var_ld_mean = mean(industry_var_ld, na.rm=T),
                         industry_var_ld_max  = max(industry_var_ld, na.rm=T),
                         industry_var_ld_min  = min(industry_var_ld, na.rm=T),
                         
                         ntl_var_ld_mean = mean(ntl_var_ld, na.rm=T),
                         ntl_var_ld_max  = max(ntl_var_ld, na.rm=T),
                         ntl_var_ld_min  = min(ntl_var_ld, na.rm=T)) %>%
        dplyr::mutate(industry_var = industry_var,
                      ntl_var = ntl_var,
                      country = country,
                      type = "cor_levels_longdiff")
      
      #### Append Together -----------------------------------------------------
      cor_all_df <- bind_rows(cor_all_df,
                     results_levels_df,
                     results_ld_df)
      

    }
  }
}


# Regression Results -----------------------------------------------------------
saveRDS(cor_all_df, file.path(data_file_path, "Results", "cor_cities_levels_ldwithin.Rds"))






