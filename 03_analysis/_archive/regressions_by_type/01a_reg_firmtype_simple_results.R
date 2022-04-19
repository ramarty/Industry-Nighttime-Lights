# Analysis

# TODO:
# Variable exists at any point in time
# Variable exists in given time
# "not[##]" variables
# long difference? 

# models: [cross section], [within diff], [across diff]

# REPORT
# N time periods
# N units

BUFFER <- 5

#mex_ind_ids <- c(11, 21, 23, 31, 42, 44, 48, 51, 61, 71, 81, 93)
#can_ind_ids <- c(11, 21, 23, 31, 42, 44, 48, 51, 61, 71, 81)

# Agriculture
# 11

# Physical Change // Big Industry
# 21, 23, 31, 42, 48, 

# Services
# 44, 51, 61, 71, 81, 93

mex_ind_id_list <- list(agriculture = c(11),
                        bigindustry = c(21,23,31,42,48),
                        services = c(44,51,61,71,81,93))

can_ind_id_list <- list(agriculture = c(11),
                        bigindustry = c(21,23,31,42,48),
                        services = c(44,51,61,71,81))

ind_types_all <- c("agriculture", "bigindustry", "services")

# Regression Function ----------------------------------------------------------
ind_type <- "bigindustry"
ntl_var <- "viirs_sum_log"
firm_var <- "N_firms_sum"
year_ld_start <- 2001
year_ld_end <- 2013
country <- "mexico"
df_type <- "city"

run_regs <- function(ind_type,
                     ind_id_list,
                     ntl_var, 
                     firm_var, 
                     year_ld_start, 
                     year_ld_end, 
                     country, 
                     df_type){
  
  print(paste(ind_type, ntl_var, firm_var, country, df_type))
  
  ind_ids <- ind_id_list[[ind_type]]
  
  ind_ids_alltypes <- ind_id_list %>% unlist() %>% as.numeric()
  ind_not_ids <- ind_ids_alltypes[!(ind_ids_alltypes %in% ind_ids)]
  
  ## Load Data
  if(grepl("hex", df_type))                           df_name <- df_type
  if(df_type == "city")                               df_name <- df_type
  if(df_type == "citygrid" & grepl("dmsp", ntl_var))  df_name <- "citygriddmsp"
  if(df_type == "citygrid" & grepl("viirs",ntl_var))  df_name <- "citygridviirs"
  
  if(country == "mexico" & grepl("viirs", ntl_var)) df_name <- paste0(df_name, "_viirs")
  if(country == "mexico" & grepl("dmsp", ntl_var)) df_name <- paste0(df_name, "_dmspols")
  
  df_name <- paste0(df_name, "_clean.Rds")
  
  df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country, "merged_clean_datasets", df_name))
  df <- df %>% dplyr::select(id, year, 
                             dmspolsharmon_sum_log, viirs_sum_log,
                             matches("N_firms_sum_[0-9]{2}$"),
                             matches("employment_sum_[0-9]{2}$"))
  
  ## Prep Variables
  firm_var_is <- paste0(firm_var, "_", ind_ids)
  firm_var_not_is <- paste0(firm_var, "_", ind_not_ids)
  
  df$ntl_var    <- df[,ntl_var] 
  df$firm_var_i <- df[,firm_var_is] %>% as.data.frame() %>% apply(1, sum, na.rm=T) # as.data.frame needed in cases only 1 id
  df$firm_var_noti <- df[,firm_var_not_is] %>% apply(1, sum, na.rm = T) # as.data.frame needed in cases only 1 id
  
  if(sd(df$firm_var_i) > 0){
    
    df <- df %>%
      dplyr::mutate(firm_var_i_log    = log(firm_var_i + 1),
                    firm_var_noti_log = log(firm_var_noti + 1))
    
    ## Subset data - firm var at some point in time
    df <- df %>%
      group_by(id) %>%
      mutate(max_firm_var_i = max(firm_var_i, na.rm = T)) %>%
      ungroup() %>%
      filter(max_firm_var_i > 0)
    
    ## Long Difference
    df_ld <- df %>%
      dplyr::arrange(year %in% c(year_ld_start, year_ld_end)) %>%
      dplyr::arrange(year) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(ntl_var = diff(ntl_var),
                       firm_var_i_log = diff(firm_var_i_log),
                       firm_var_noti_log = diff(firm_var_noti_log))
    
    ## Regressions
    lm_yr <- felm(ntl_var ~ firm_var_i_log | year | 0 | 0, data = df) %>% extract_coefs()
    lm_id <- felm(ntl_var ~ firm_var_i_log | id   | 0 | 0, data = df) %>% extract_coefs()
    lm_ld <- felm(ntl_var ~ firm_var_i_log | 0    | 0 | 0, data = df_ld) %>% extract_coefs()
    
    lm_yr_other <- felm(ntl_var ~ firm_var_i_log + firm_var_noti_log | year | 0 | 0, data = df) %>% extract_coefs()
    lm_id_other <- felm(ntl_var ~ firm_var_i_log + firm_var_noti_log | id   | 0 | 0, data = df) %>% extract_coefs()
    lm_ld_other <- felm(ntl_var ~ firm_var_i_log + firm_var_noti_log | 0    | 0 | 0, data = df_ld) %>% extract_coefs()
    
    ## Model Output Data
    df_out <- bind_rows(
      lm_yr       %>% mutate(type = "year_FE",   control_other = F),
      lm_id       %>% mutate(type = "id_FE",     control_other = F),
      lm_ld       %>% mutate(type = "long_diff", control_other = F),
      lm_yr_other %>% mutate(type = "year_FE",   control_other = T),
      lm_id_other %>% mutate(type = "id_FE",     control_other = T),
      lm_ld_other %>% mutate(type = "long_diff", control_other = T)
    ) %>%
      mutate(ind_type   = ind_type,
             ntl_var  = ntl_var,
             firm_var = firm_var,
             df_type = df_type,
             country = country)
    
  } else{
    df_out = data.frame(NULL)
  }
  
  return(df_out)
  
}

# Implement Function -----------------------------------------------------------
results_all <- data.frame(NULL)

for(dataset in c("city", "citygrid", "hex_5km", "hex_10km", "hex_25km", "hex_50km", "hex_100km")){
  for(dep_var in c("employment_sum", "N_firms_sum")){
    
    print(paste(dataset, "---", dep_var))
    
    results_all <- bind_rows(
      results_all,
      map_df(ind_types_all, run_regs, can_ind_id_list, "dmspolsharmon_sum_log", "N_firms_sum", 2001, 2013, "canada", dataset),
      map_df(ind_types_all, run_regs, can_ind_id_list, "viirs_sum_log",         "N_firms_sum", 2011, 2013, "canada", dataset),
      
      map_df(ind_types_all, run_regs, can_ind_id_list, "dmspolsharmon_sum_log", "employment_sum", 2001, 2013, "canada", dataset),
      map_df(ind_types_all, run_regs, can_ind_id_list, "viirs_sum_log",         "employment_sum", 2011, 2013, "canada", dataset),
      
      map_df(ind_types_all, run_regs, mex_ind_id_list, "dmspolsharmon_sum_log", "N_firms_sum", 2004, 2014, "mexico", dataset),
      map_df(ind_types_all, run_regs, mex_ind_id_list, "viirs_sum_log",         "N_firms_sum", 2014, 2020, "mexico", dataset)
    )
    
  }
}

saveRDS(results_all, file.path(data_file_path, "Results", "by_firm_type_ind_reg.Rds"))


