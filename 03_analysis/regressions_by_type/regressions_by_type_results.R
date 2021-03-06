# Regressions by Code

country <- "canada"

lm_confint_tidy <- function(lm){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$variable <- row.names(lm_confint)
  
  lm_coefs <- lm %>%
    summary() %>%
    coefficients() %>%
    as.data.frame()
  lm_coefs$variable <- row.names(lm_coefs)
  
  lm_all <- merge(lm_confint, lm_coefs, by="variable")
  
  return(lm_all)
}

# NAISC Code List --------------------------------------------------------------
naics_codes_df <- read_dta(file.path(project_file_path, "Data", "NAICS Codes", "naics2digit.dta"))
naics_codes_df$naicsname <- paste0(naics_codes_df$naics2, " - ", naics_codes_df$naicsname)

if(country %in% "canada"){
  # CHANGE AFTER CLEANING
  naics_codes <- c("11",  "21",  "22",  "23",  "31",  "32",  "33",  "41",  "44",  "45",  "48",
                   "49",  "51",  "52",  "53",  "54",  "55",  "56",  "61",  "62",  "71",  "72",  "81")
  
  diffs <- 1:5
  # excluding 91
}

if(country %in% "mexico"){
  naics_codes <- c("11",  "21",  "22",  "23",  "31",  "32",  "33",  "43",  "46",  
                   "48"  ,"49" , "51" , "52" , "53" , "54" , "55" , "56",  "61",  "62" , "71" , "72",  "81")
  
  diffs <- 1:2
}

naics_codes_rx <- paste0("t", naics_codes) %>% paste(collapse = "|")

# Load Data --------------------------------------------------------------------
grid = "25"
dv = "dmspols_sum_all"
var_type = "employment_sum"
var_transform = ""
difference <- "_diff1"

results_all <- data.frame(NULL)

for(grid in c(5,10,25,50,100, 250, 500)){
  
  ## Load Data
  df <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", country, 
                          "merged_datasets", 
                          paste0("hex_",grid,"km_clean.Rds")))
  
  for(dv in c("dmspols_sum_all")){
    for(var_type in c("firms_sum", "employment_sum")){
      for(var_transform in c("", "_log")){
        for(difference in c("", paste0("_diff", diffs))){
          
          print(paste(grid, var_type, var_transform, difference, "---------------------------"))
          
          ## IVs
          firm_ivs <- paste0(var_type, "_t", naics_codes, var_transform, difference) %>% paste(collapse = " + ")
          
          ## DV
          dv_i <- paste0(dv, var_transform, difference)
          
          ## Results
          # felm(as.formula(paste(dv_i, 
          #                       " ~ ", 
          #                       firm_ivs, 
          #                       " | 0 | 0 | 0")), 
          #      data = df) %>%
            
          result_df <- lm_robust(as.formula(paste(dv_i, 
                                             " ~ ", 
                                             firm_ivs)), 
                            data = df) %>%
            lm_confint_tidy() %>%
            mutate(naics2 = str_extract(variable, "t([[:digit:]])([[:digit:]])") %>% str_replace_all("t", "") %>% as.numeric()) %>%
            left_join(naics_codes_df, by = "naics2") %>%
            mutate(country = country,
                   grid = grid,
                   var_type = var_type,
                   var_transform = var_transform,
                   dv = dv,
                   difference = difference)
          
          ## Append
          results_all <- bind_rows(results_all, result_df)
          
        }
      }
    }
  }
}

# Export -----------------------------------------------------------------------
saveRDS(results_all, file.path(project_file_path, "Data", "results", country, "reg_by_naics2.Rds"))







