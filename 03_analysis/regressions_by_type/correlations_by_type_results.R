# Regressions by Code

country <- "canada"

# NAISC Code List --------------------------------------------------------------
naics_codes_df <- read_dta(file.path(project_file_path, "Data", "NAICS Codes", "naics2digit.dta"))
naics_codes_df$naicsname <- paste0(naics_codes_df$naics2, " - ", naics_codes_df$naicsname)

if(country %in% "canada"){
  # CHANGE AFTER CLEANING
  YEARS <- c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "All")
  naics_codes <- c("11",  "21",  "22",  "23",  "31",  "32",  "33",  "41",  "44",  "45",  "48",
                   "49",  "51",  "52",  "53",  "54",  "55",  "56",  "61",  "62",  "71",  "72",  "81")
  
  MAX_DIFF <- 5
  # excluding 91
}

if(country %in% "mexico"){
  YEARS <- c("2004", "2009", "2014", "All")
  naics_codes <- c("11",  "21",  "22",  "23",  "31",  "32",  "33",  "43",  "46",  
                   "48"  ,"49" , "51" , "52" , "53" , "54" , "55" , "56",  "61",  "62" , "71" , "72",  "81")
  
  MAX_DIFF <- 2
}

# Load Data --------------------------------------------------------------------
unit = 5
dmspols_var = "dmspol_sum"
firm_var = "employment_sum"
naics_code_i <- 11
transform = "level"
difference = "level"

df_out_all <- data.frame(NULL)
for(unit in c(5,10,25,50,100,250,500,1000)){
  
  print(unit)
  
  grid_all <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", country, "merged_datasets",
                                paste0("hex_",unit,"km_clean.Rds")))
  
  for(dmspols_var in c("dmspol_sum")){
    for(firm_var in c("employment_sum", "firms_sum")){
      for(naics_code_i in naics_codes){
        for(transform in c("level", "log", "g5")){
          for(difference in c("level", paste0("diff",1:MAX_DIFF))){
            
            #print(firm_var)
            print(paste(unit, dmspols_var, firm_var, naics_code_i, transform, difference))
            
            grid <- grid_all
            
            firm_var_i <- paste0(firm_var, "_t", naics_code_i)
            firm_var_notransf_i <- firm_var_i
            
            #### Transformation
            if(transform %in% "log_level"){
              dmspols_var_i <- paste0(dmspols_var, "_log")
              firm_var_i    <- firm_var_i
            } else if(transform %in% "level_log"){
              dmspols_var_i <- dmspols_var
              firm_var_i    <- paste0(firm_var_i, "_log")
            } else if(transform %in% "level"){
              dmspols_var_i <- dmspols_var
              firm_var_i    <- firm_var_i
            } else{
              dmspols_var_i <- paste0(dmspols_var, "_", transform)
              firm_var_i    <- paste0(firm_var_i, "_", transform)
            }
            
            #### Difference
            if(difference != "level"){
              dmspols_var_i <- paste0(dmspols_var_i, "_", difference)
              firm_var_i    <- paste0(firm_var_i, "_", difference)
            }
            
            # dmspols_sum_all_log_diff1
            
            grid$dmspols_var <- grid[[dmspols_var_i]]
            grid$firm_var    <- grid[[firm_var_i]]
            grid$firm_var_notransf    <- grid[[firm_var_notransf_i]]
            
            #table((sum(!is.na(grid$dmspols_var)) > 0) & (sum(!is.na(grid$firm_var)) > 0)) %>% print()
            if((sum(!is.na(grid$dmspols_var)) > 0) & (sum(!is.na(grid$firm_var)) > 0)){
              
              # Restrict: at least one firm in grid across all time periods
              grid <- grid %>%
                group_by(id) %>%
                mutate(sum_firms_id_time = sum(firm_var_notransf, na.rm = T)) %>%
                ungroup() %>%
                filter(sum_firms_id_time > 0)
              
              cor_out <- cor.test(grid$dmspols_var, grid$firm_var)
              
              ci <- cor_out$conf.int %>% as.numeric()
              df_out <- data.frame(b = cor_out$estimate,
                                   p = cor_out$p.value,
                                   ci_low = ci[1],
                                   ci_high = ci[2],
                                   unit = paste0(unit, "km"),
                                   transform = transform,
                                   dmspols_var = dmspols_var,
                                   firm_var = firm_var,
                                   naics_code_i = naics_code_i,
                                   difference = difference,
                                   N = nrow(grid))
              
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

saveRDS(df_out_all, file.path(data_file_path, "Results", country, "polygon_correlation_results_bytype.Rds"))

