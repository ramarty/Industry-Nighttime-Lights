# Analysis

country <- "mexico"

if(country %in% "canada"){
  YEARS <- c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "All")
  MAX_DIFF <- 6
}

if(country %in% "mexico"){
  YEARS <- c("2004", "2009", "2014", "All")
  MAX_DIFF <- 2
} 

# Load Data --------------------------------------------------------------------
readRDS_excludevars <- function(filepath){
  # Exclude variables by category
  df <- readRDS(filepath)
  rm_vars <- names(df)[str_detect(names(df), "t([[:digit:]])([[:digit:]])")]
  
  df <- df %>%
    dplyr::select(-all_of(rm_vars))
  
  return(df)
}


grid <- list.files(file.path(project_file_path, "Data", "Grid", "FinalData", country, "merged_datasets"),
                   pattern = "*_clean.Rds", full.names = T) %>%
  lapply(readRDS_excludevars) %>%
  bind_rows() %>%
  filter(!is.na(unit)) %>%
  filter(!is.na(dmspol_mean)) %>%
  mutate(unit = unit %>% str_replace_all("hex_", ""))

grid      <- grid[(grid$dmspol_mean > 0) | !is.na(grid$employment_sum_all),]
grid_non0 <- grid[(grid$dmspol_mean > 0) & !is.na(grid$employment_sum_all),]

unit <- "100km"
transform <- "log"
year <- 2001
dmspols_var <- "dmspol_mean"
firm_var <- "employment_sum_all"

df_out_all <- data.frame(NULL)

# TODO: All element of loop - baseline NTL.

counter <- 1
for(year in YEARS){
  
  print(year)
  
  for(unit in unique(grid_non0$unit)){
    for(dmspols_var in c("dmspol_mean")){
      for(firm_var in c("employment_sum_all", "firms_sum_all")){
        for(transform in c("level", "log", "g5", "g25")){
          for(difference in c("level", paste0("diff",1:MAX_DIFF))){
            
            
            #### Data Subset
            if(year %in% "All"){
              df_temp <- grid_non0[(grid_non0$unit %in% unit),]
            } else{
              year_i <- year %>% as.numeric
              df_temp <- grid_non0[(grid_non0$unit %in% unit) & (grid_non0$year %in% year_i),]
            }
            
            #### Transformation
            if(transform %in% "log_level"){
              dmspols_var_i <- paste0(dmspols_var, "_log")
              firm_var_i    <- firm_var
            } else if(transform %in% "level_log"){
              dmspols_var_i <- dmspols_var
              firm_var_i    <- paste0(firm_var, "_log")
            } else if(transform %in% "level"){
              dmspols_var_i <- dmspols_var
              firm_var_i    <- firm_var
            } else{
              dmspols_var_i <- paste0(dmspols_var, "_", transform)
              firm_var_i    <- paste0(firm_var, "_", transform)
            }
            
            #### Difference
            if(difference != "level"){
              dmspols_var_i <- paste0(dmspols_var_i, "_", difference)
              firm_var_i    <- paste0(firm_var_i, "_", difference)
            }
            
            df_temp$dmspols_var <- df_temp[[dmspols_var_i]]
            df_temp$firm_var    <- df_temp[[firm_var_i]]
            
            if((sum(!is.na(df_temp$dmspols_var)) > 0) & (sum(!is.na(df_temp$firm_var)) > 0)){
              
              cor_out <- cor.test(df_temp$dmspols_var, df_temp$firm_var)
              
              ci <- cor_out$conf.int %>% as.numeric()
              df_out <- data.frame(b = cor_out$estimate,
                                   p = cor_out$p.value,
                                   ci_low = ci[1],
                                   ci_high = ci[2],
                                   unit = unit,
                                   transform = transform,
                                   dmspols_var = dmspols_var,
                                   firm_var = firm_var,
                                   difference = difference,
                                   year = year)
              
              df_out_all <- bind_rows(df_out, df_out_all)
            }
            
          }
        }
      }
    }
  }
}

df_out_all$unit <- df_out_all$unit %>% factor(levels = c("5km", "10km", "25km", "50km", "100km", "250km", "500km", "1000km"))
df_out_all$year <- factor(df_out_all$year)

df_out_all <- df_out_all %>%
  
  mutate(transform = case_when(transform == "g50" ~ "50 Groups",
                               transform == "g25" ~ "25 Groups",
                               transform == "g5" ~ "5 Groups",
                               transform == "log" ~ "Logs",
                               transform == "level" ~ "Levels")) %>%
  mutate(transform = transform %>% factor(levels = c("Levels",
                                                     "Logs",
                                                     "5 Groups",
                                                     "25 Groups",
                                                     "50 Groups")))

saveRDS(df_out_all, file.path(data_file_path, "Results", country, "polygon_correlation_results.Rds"))

