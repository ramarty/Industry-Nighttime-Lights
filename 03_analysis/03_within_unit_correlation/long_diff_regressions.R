# Analysis

mex_viirs_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", "merged_datasets", "hex_5km_viirs_clean.Rds"))
mex_dmspols_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", "merged_datasets", "hex_5km_dmspols_clean.Rds"))
can_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", "canada", "merged_datasets", "hex_5km_clean.Rds"))

vars <- names(mex_viirs_df)[!grepl("log", names(mex_viirs_df))]
for(v in vars) mex_viirs_df[[paste0(v, "_log")]] <- log(mex_viirs_df[[v]]+1)

vars <- names(mex_dmspols_df)[!grepl("log", names(mex_dmspols_df))]
for(v in vars) mex_dmspols_df[[paste0(v, "_log")]] <- log(mex_dmspols_df[[v]]+1)

vars <- names(can_df)[!grepl("log", names(can_df))]
for(v in vars) can_df[[paste0(v, "_log")]] <- log(can_df[[v]]+1)

mex_viirs_df   <- mex_viirs_df[mex_viirs_df$year %in% c(2014, 2019),]
mex_dmspols_df <- mex_dmspols_df[mex_dmspols_df$year %in% c(2004, 2014),]
can_viirs_df   <- can_df[can_df$year %in% c(2011, 2013),]
can_dmspols_df <- can_df[can_df$year %in% c(2001, 2013),]

prep_data <- function(df){
  df <- df %>%
    dplyr::select(-contains("_diff")) %>%
    dplyr::select(-contains("_[[:digit:]][[:digit:]]_"))
  
  df_diff <- df %>%
    arrange(year) %>%
    group_by(id) %>%
    summarise_if(is.numeric, diff)
  
  df_base <- df[df$year %in% min(df$year),]
  df_base <- df_base %>%
    dplyr::select(-year) %>%
    rename_at(vars(-id), ~ paste0(., '_BASE'))
  
  df_out <- merge(df_diff, df_base, by = "id", all = T)
  
  return(df_out)
}

mex_viirs_diff_df <- prep_data(mex_viirs_df)
mex_dmspols_diff_df <- prep_data(mex_dmspols_df)
can_viirs_diff_df <- prep_data(can_viirs_df)
can_dmspols_diff_df <- prep_data(can_dmspols_df)

## MEX
mex_1 <- felm(N_firms_sum_all_log ~ viirs_mean_log, data = mex_viirs_diff_df) 
mex_2 <- felm(N_firms_sum_all_log ~ viirs_mean_log*N_firms_sum_all_log_BASE, data = mex_viirs_diff_df) 

mex_3 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log, data = mex_dmspols_diff_df) 
mex_4 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log*N_firms_sum_all_log_BASE, data = mex_dmspols_diff_df) 

mex_5 <- felm(employment_sum_all_log ~ viirs_mean_log, data = mex_viirs_diff_df) 
mex_6 <- felm(employment_sum_all_log ~ viirs_mean_log*employment_sum_all_log_BASE, data = mex_viirs_diff_df) 

mex_7 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log, data = mex_dmspols_diff_df) 
mex_8 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log*employment_sum_all_log_BASE, data = mex_dmspols_diff_df) 

## CAN
can_1 <- felm(N_firms_sum_all_log ~ viirs_mean_log, data = can_viirs_diff_df) 
can_2 <- felm(N_firms_sum_all_log ~ viirs_mean_log*N_firms_sum_all_log_BASE, data = can_viirs_diff_df) 

can_3 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log, data = mex_dmspols_diff_df) 
can_4 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log*N_firms_sum_all_log_BASE, data = can_dmspols_diff_df) 

can_5 <- felm(employment_sum_all_log ~ viirs_mean_log, data = can_viirs_diff_df) 
can_6 <- felm(employment_sum_all_log ~ viirs_mean_log*employment_sum_all_log_BASE, data = can_viirs_diff_df) 

can_7 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log, data = can_dmspols_diff_df) 
can_8 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log*employment_sum_all_log_BASE, data = can_dmspols_diff_df) 

stargazer(mex_1,
          mex_2,
          mex_3,
          mex_4,
          mex_5,
          mex_6,
          mex_7,
          mex_8,
          dep.var.labels.include = T,
          dep.var.labels = c("N Firms", "Employment"),
          covariate.labels = c("VIIRS",
                               "N Firms (Initial)",
                               "VIIRS$\\times$N Firms (Initial)", 
                               
                               "DMSP-OLS$\\times$N Firms (Initial)", 
                               "DMSP-OLS",
                               "Employ (Initial)",
                               "VIIRS$\\times$Employ (Initial)", 
                               "DMSP-OLS$\\times$Employ (Initial)"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          out=file.path(tables_file_path, "lm_longdiff_mex.tex"))

stargazer(can_1,
          can_2,
          can_3,
          can_4,
          can_5,
          can_6,
          can_7,
          can_8,
          dep.var.labels.include = T,
          dep.var.labels = c("N Firms", "Employment"),
          covariate.labels = c("VIIRS",
                               "N Firms (Initial)",
                               "VIIRS$\\times$N Firms (Initial)", 
                               
                               "DMSP-OLS$\\times$N Firms (Initial)", 
                               "DMSP-OLS",
                               "Employ (Initial)",
                               "VIIRS$\\times$Employ (Initial)", 
                               "DMSP-OLS$\\times$Employ (Initial)"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          out=file.path(tables_file_path, "lm_longdiff_can.tex"))

