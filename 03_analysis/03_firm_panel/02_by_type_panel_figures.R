# Analysis

BUFFER <- 5

mex_ind_ids <- c(11, 21, 23, 31, 42, 44, 48, 51, 61, 71, 81, 93)
can_ind_ids <- c(11, 21, 23, 31, 42, 44, 48, 51, 61, 71, 81)

# Load Data --------------------------------------------------------------------
mex_viirs_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", "merged_datasets", paste0("hex_",BUFFER,"km_viirs_clean.Rds")))
mex_dmspols_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", "merged_datasets", paste0("hex_",BUFFER,"km_dmspols_clean.Rds")))
can_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", "canada", "merged_datasets", paste0("hex_",BUFFER,"km_clean.Rds")))

mex_viirs_df$unit   <- NULL
mex_dmspols_df$unit <- NULL
can_df$unit         <- NULL

vars <- names(mex_viirs_df)[!grepl("log", names(mex_viirs_df))]
for(v in vars) mex_viirs_df[[paste0(v, "_log")]] <- log(mex_viirs_df[[v]]+1)

vars <- names(mex_dmspols_df)[!grepl("log", names(mex_dmspols_df))]
for(v in vars) mex_dmspols_df[[paste0(v, "_log")]] <- log(mex_dmspols_df[[v]]+1)

vars <- names(can_df)[!grepl("log", names(can_df))]
for(v in vars) can_df[[paste0(v, "_log")]] <- log(can_df[[v]]+1)

prep_data <- function(df, ind_ids){
  
  df <- df %>%
    dplyr::select(paste0("N_firms_sum_",ind_ids,"_log"),
                  paste0("employment_sum_",ind_ids,"_log"),
                  dmspols_mean_log,
                  viirs_mean_log,
                  year,
                  id) %>%
    pivot_longer(cols = -c(year, id, viirs_mean_log, dmspols_mean_log)) %>%
    mutate(indust_id = name %>% str_replace_all("N_firms_sum_|employment_sum_|_log", "") %>% as.numeric()) %>%
    mutate(firm_var = name %>% str_replace_all("[[:digit:]]", "") %>% str_replace_all("__", "_")) %>%
    dplyr::select(-name) %>%
    pivot_wider(names_from = firm_var,
                values_from = value)
  
  return(df)
}

mex_viirs_df   <- prep_data(mex_viirs_df, mex_ind_ids)
mex_dmspols_df <- prep_data(mex_dmspols_df, mex_ind_ids)
can_df         <- prep_data(can_df, can_ind_ids)

## FIRMS
firm_dmspols_mex_yr <- felm(N_firms_sum_log  ~ dmspols_mean_log:factor(indust_id) | year | 0 | 0, data = mex_dmspols_df) 
firm_dmspols_mex_id <- felm(N_firms_sum_log ~ dmspols_mean_log:factor(indust_id) | id | 0 | 0,   data = mex_dmspols_df) 

firm_viirs_mex_yr <- felm(N_firms_sum_log ~ viirs_mean_log:factor(indust_id) | year | 0 | 0, data = mex_viirs_df) 
firm_viirs_mex_id <- felm(N_firms_sum_log ~ viirs_mean_log:factor(indust_id) | id | 0 | 0,   data = mex_viirs_df) 

firm_dmspols_can_yr <- felm(N_firms_sum_log ~ dmspols_mean_log:factor(indust_id) | year | 0 | 0, data = can_df) 
firm_dmspols_can_id <- felm(N_firms_sum_log ~ dmspols_mean_log:factor(indust_id) | id | 0 | 0,   data = can_df) 

firm_viirs_can_yr <- felm(N_firms_sum_log ~ viirs_mean_log:factor(indust_id) | year | 0 | 0, data = can_df) 
firm_viirs_can_id <- felm(N_firms_sum_log ~ viirs_mean_log:factor(indust_id) | id | 0 | 0,   data = can_df) 

## EMPLOYMENT
employ_dmspols_mex_yr <- felm(employment_sum_log ~ dmspols_mean_log:factor(indust_id) | year | 0 | 0, data = mex_dmspols_df) 
employ_dmspols_mex_id <- felm(employment_sum_log ~ dmspols_mean_log:factor(indust_id) | id | 0 | 0,   data = mex_dmspols_df) 

employ_viirs_mex_yr <- felm(employment_sum_log ~ viirs_mean_log:factor(indust_id) | year | 0 | 0, data = mex_viirs_df) 
employ_viirs_mex_id <- felm(employment_sum_log ~ viirs_mean_log:factor(indust_id) | id | 0 | 0,   data = mex_viirs_df) 

employ_dmspols_can_yr <- felm(employment_sum_log ~ dmspols_mean_log:factor(indust_id) | year | 0 | 0, data = can_df) 
employ_dmspols_can_id <- felm(employment_sum_log ~ dmspols_mean_log:factor(indust_id) | id | 0 | 0,   data = can_df) 

employ_viirs_can_yr <- felm(employment_sum_log ~ viirs_mean_log:factor(indust_id) | year | 0 | 0, data = can_df) 
employ_viirs_can_id <- felm(employment_sum_log ~ viirs_mean_log:factor(indust_id) | id | 0 | 0,   data = can_df) 

stargazer(employ_dmspols_can_yr,
          employ_dmspols_can_id,
          employ_dmspols_mex_yr,
          employ_dmspols_mex_id,
          firm_dmspols_can_yr,
          firm_dmspols_can_id,         
          firm_dmspols_mex_yr,
          firm_dmspols_mex_id,
          dep.var.labels.include = T,
          dep.var.labels = c("Employment", "Firms"),
          covariate.labels = c("DMSPOLS X Industry: 11",
                               "DMSPOLS X Industry: 21",
                               "DMSPOLS X Industry: 23",
                               "DMSPOLS X Industry: 31",
                               "DMSPOLS X Industry: 42",
                               "DMSPOLS X Industry: 44",
                               "DMSPOLS X Industry: 48",
                               "DMSPOLS X Industry: 51",
                               "DMSPOLS X Industry: 61",
                               "DMSPOLS X Industry: 71",
                               "DMSPOLS X Industry: 81",
                               "DMSPOLS X Industry: 93"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("Y", "N"), 4)),
            c("Unit FE",rep(c("N", "Y"), 4)),
            c("Country",rep("CAN", 2), rep("MEX", 2),rep("CAN", 2), rep("MEX", 2))),
          out=file.path(tables_file_path, paste0("lm_bytype_dmspols_panel_",BUFFER,"km.tex")))

stargazer(employ_viirs_can_yr,
          employ_viirs_can_id,
          employ_viirs_mex_yr,
          employ_viirs_mex_id,
          firm_viirs_can_yr,
          firm_viirs_can_id,         
          firm_viirs_mex_yr,
          firm_viirs_mex_id,
          dep.var.labels.include = T,
          dep.var.labels = c("Employment", "Firms"),
          covariate.labels = c("VIIRS X Industry: 11",
                               "VIIRS X Industry: 21",
                               "VIIRS X Industry: 23",
                               "VIIRS X Industry: 31",
                               "VIIRS X Industry: 42",
                               "VIIRS X Industry: 44",
                               "VIIRS X Industry: 48",
                               "VIIRS X Industry: 51",
                               "VIIRS X Industry: 61",
                               "VIIRS X Industry: 71",
                               "VIIRS X Industry: 81",
                               "VIIRS X Industry: 93"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("Y", "N"), 4)),
            c("Unit FE",rep(c("N", "Y"), 4)),
            c("Country",rep("CAN", 2), rep("MEX", 2),rep("CAN", 2), rep("MEX", 2))),
          out=file.path(tables_file_path, paste0("lm_bytype_viirs_panel_",BUFFER,"km.tex")))
