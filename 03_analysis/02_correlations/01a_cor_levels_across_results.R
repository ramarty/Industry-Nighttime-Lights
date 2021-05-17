# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                                 "mexico", "merged_clean_appended_allunits", "mex_dmspols_notype.Rds")) 

mex_viirs <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                               "mexico", "merged_clean_appended_allunits", "mex_viirs_notype.Rds")) 

can <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData",
                         "canada", "merged_clean_appended_allunits", "can_notype.Rds")) 


mex_dmspols <- mex_dmspols[mex_dmspols$year %in% 2014,]
mex_viirs <- mex_viirs[mex_viirs$year %in% 2014,]

mex_dmspols <- mex_dmspols[mex_dmspols$unit %in% "25km Grid",]
mex_viirs <- mex_viirs[mex_viirs$unit %in% "25km Grid",]

mex_dmspols <- mex_dmspols[mex_dmspols$N_firms_sum_all > 0,]
mex_viirs <- mex_viirs[mex_viirs$N_firms_sum_all > 0,]

# Correlation ------------------------------------------------------------------
country <- "Canada"
ntl_var <- "dmspols_mean"
year <- "2001"
unit <- "25km Grid"
firm_var <- "employment_sum_all"
difference <- "level"

df_out_all <- data.frame(NULL)
for(country in c("Canada", "Mexico")){
  for(ntl_var in c("dmspolsharmon_sum", "viirs_sum")){
    
    ## Grab dataset
    if(country %in% "Canada")                             df <- can
    if(country %in% "Mexico" & grepl("dmspols", ntl_var)) df <- mex_dmspols
    if(country %in% "Mexico" & grepl("viirs", ntl_var))   df <- mex_viirs
    
    ## Grab variables to loop through
    YEARS <- df$year %>% unique() %>% as.character()
    MAX_DIFF <- df %>% 
      names %>% 
      str_subset("_diff") %>% 
      str_replace_all(".*_", "") %>% 
      str_replace_all("diff", "") %>%
      as.numeric() %>%
      max()
    UNITS <- df$unit %>% unique() %>% as.character()
    
    for(year in c(YEARS, "All")){
      for(unit in UNITS){
        for(firm_var in c("employment_sum_all", "N_firms_sum_all")){
          for(transform in c("level", "log")){
            for(difference in c("level", paste0("diff",1:MAX_DIFF))){
              print(paste(year, country, unit, difference, transform, ntl_var, firm_var, sep = " - "))
              
              if(year %in% as.character(2000:2010) & grepl("viirs", ntl_var)) next
              if(year %in% as.character(2015:2020) & grepl("dmspols", ntl_var)) next
              
              ## Define base data and variables
              df_temp    <- df
              ntl_var_i  <- ntl_var
              firm_var_i <- firm_var
              
              ## Check for skips
              if(is.null(df_temp[[ntl_var_i]]))  next
              if(is.null(df_temp[[firm_var_i]])) next
              
              ## Subset by unit and year
              if(year %in% "All"){
                df_temp <- df_temp[(df_temp$unit %in% unit),]
              } else{
                year_i <- year %>% as.numeric
                df_temp <- df_temp[(df_temp$unit %in% unit) & (df_temp$year %in% year_i),]
              }
              
              ## Transform
              if(transform != "level"){
                ntl_var_i  <- paste0(ntl_var_i, "_", transform)
                firm_var_i <- paste0(firm_var_i, "_", transform)
              } 
              
              ## Difference
              # If looking at differences
              # use non-diff variable later
              # in subsetting
              df_temp$firm_var_nodiff <- df_temp[[firm_var_i]] 
              
              if(difference != "level"){
                ntl_var_i  <- paste0(ntl_var_i, "_", difference)
                firm_var_i <- paste0(firm_var_i, "_", difference)
              } 
              
              ## Add variables to dataframe 
              df_temp$ntl_var  <- df_temp[[ntl_var_i]]
              df_temp$firm_var <- df_temp[[firm_var_i]]
              
              ## Remove NAs
              df_temp <- df_temp %>%
                filter(!is.na(ntl_var),
                       !is.na(firm_var))
              
              ## Remove cells with no firm data
              if(difference == "level"){
                df_temp <- df_temp %>% 
                  filter(firm_var > 0)
              } else{
                # If difference, remove if both (1) level is zero and (2) difference is zero
                df_temp <- df_temp %>% 
                  filter(!((firm_var_nodiff %in% 0) & (firm_var %in% 0)))
              }
              
              ## Correlation
              if(nrow(df_temp) > 1){
                
                cor_out <- cor.test(df_temp$ntl_var, df_temp$firm_var)
                
                ci <- cor_out$conf.int %>% as.numeric()
                df_out <- data.frame(b = cor_out$estimate,
                                     p = cor_out$p.value,
                                     ci_low = ci[1],
                                     ci_high = ci[2],
                                     country = country,
                                     ntl_var = ntl_var,
                                     year = year,
                                     unit = unit,
                                     firm_var = firm_var,
                                     transform = transform,
                                     difference = difference,
                                     N = nrow(df_temp))
                
                df_out_all <- bind_rows(df_out, df_out_all)
                
              }
            }
          }
        }
      }
    }
  }
}

saveRDS(df_out_all, file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))




