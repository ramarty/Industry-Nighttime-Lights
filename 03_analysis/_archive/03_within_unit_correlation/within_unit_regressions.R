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

prep_data <- function(df){
  df <- df %>%
    group_by(id) %>%
    dplyr::mutate(N_firms_sum_all_log_max = max(N_firms_sum_all_log),
                  N_firms_sum_all_log_min = min(N_firms_sum_all_log),
                  N_firms_sum_all_log_diff = max(N_firms_sum_all_log) - min(N_firms_sum_all_log),
                  
                  employment_sum_all_log_max = max(employment_sum_all_log),
                  employment_sum_all_log_min = min(employment_sum_all_log),
                  employment_sum_all_log_diff = max(employment_sum_all_log) - min(employment_sum_all_log))
  return(df)
}

can_df         <- prep_data(can_df)
mex_viirs_df   <- prep_data(mex_viirs_df)
mex_dmspols_df <- prep_data(mex_dmspols_df)

# CAN - DMSP
can_dmpsols_1 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log | id | 0 | 0, data = can_df) 
can_dmpsols_2 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:N_firms_sum_all_log_min | id | 0 | 0, data = can_df) 
can_dmpsols_3 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:N_firms_sum_all_log_diff | id | 0 | 0, data = can_df) 
can_dmpsols_4 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:N_firms_sum_all_log_min + dmspolselvidge_mean_log:N_firms_sum_all_log_diff + dmspolselvidge_mean_log:N_firms_sum_all_log_diff:N_firms_sum_all_log_min | id | 0 | 0, data = can_df) 

can_dmpsols_5 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log | id | 0 | 0, data = can_df) 
can_dmpsols_6 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:employment_sum_all_log_min | id | 0 | 0, data = can_df) 
can_dmpsols_7 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:employment_sum_all_log_diff | id | 0 | 0, data = can_df) 
can_dmpsols_8 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:employment_sum_all_log_min + dmspolselvidge_mean_log:employment_sum_all_log_diff + dmspolselvidge_mean_log:employment_sum_all_log_diff:employment_sum_all_log_min | id | 0 | 0, data = can_df) 

# MEX - DMSP
mex_dmpsols_1 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log | id | 0 | 0, data = mex_dmspols_df) 
mex_dmpsols_2 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:N_firms_sum_all_log_min | id | 0 | 0, data = mex_dmspols_df) 
mex_dmpsols_3 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:N_firms_sum_all_log_diff | id | 0 | 0, data = mex_dmspols_df) 
mex_dmpsols_4 <- felm(N_firms_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:N_firms_sum_all_log_min + dmspolselvidge_mean_log:N_firms_sum_all_log_diff + dmspolselvidge_mean_log:N_firms_sum_all_log_diff:N_firms_sum_all_log_min | id | 0 | 0, data = mex_dmspols_df) 

mex_dmpsols_5 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log | id | 0 | 0, data = mex_dmspols_df) 
mex_dmpsols_6 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:employment_sum_all_log_min | id | 0 | 0, data = mex_dmspols_df) 
mex_dmpsols_7 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:employment_sum_all_log_diff | id | 0 | 0, data = mex_dmspols_df) 
mex_dmpsols_8 <- felm(employment_sum_all_log ~ dmspolselvidge_mean_log + dmspolselvidge_mean_log:employment_sum_all_log_min + dmspolselvidge_mean_log:employment_sum_all_log_diff + dmspolselvidge_mean_log:employment_sum_all_log_diff:employment_sum_all_log_min | id | 0 | 0, data = mex_dmspols_df) 

# MEX - VIIRS
mex_viirs_1 <- felm(N_firms_sum_all_log ~ viirs_mean_log | id | 0 | 0, data = mex_viirs_df) 
mex_viirs_2 <- felm(N_firms_sum_all_log ~ viirs_mean_log + viirs_mean_log:N_firms_sum_all_log_min | id | 0 | 0, data = mex_viirs_df) 
mex_viirs_3 <- felm(N_firms_sum_all_log ~ viirs_mean_log + viirs_mean_log:N_firms_sum_all_log_diff | id | 0 | 0, data = mex_viirs_df) 
mex_viirs_4 <- felm(N_firms_sum_all_log ~ viirs_mean_log + viirs_mean_log:N_firms_sum_all_log_min + viirs_mean_log:N_firms_sum_all_log_diff + viirs_mean_log:N_firms_sum_all_log_diff:N_firms_sum_all_log_min | id | 0 | 0, data = mex_viirs_df) 

mex_viirs_5 <- felm(employment_sum_all_log ~ viirs_mean_log | id | 0 | 0, data = mex_viirs_df) 
mex_viirs_6 <- felm(employment_sum_all_log ~ viirs_mean_log + viirs_mean_log:employment_sum_all_log_min | id | 0 | 0, data = mex_viirs_df) 
mex_viirs_7 <- felm(employment_sum_all_log ~ viirs_mean_log + viirs_mean_log:employment_sum_all_log_diff | id | 0 | 0, data = mex_viirs_df) 
mex_viirs_8 <- felm(employment_sum_all_log ~ viirs_mean_log + viirs_mean_log:employment_sum_all_log_min + viirs_mean_log:employment_sum_all_log_diff + viirs_mean_log:employment_sum_all_log_diff:employment_sum_all_log_min | id | 0 | 0, data = mex_viirs_df) 

stargazer(can_dmpsols_1,
          can_dmpsols_2,
          can_dmpsols_3,
          can_dmpsols_4,
          can_dmpsols_5,
          can_dmpsols_6,
          can_dmpsols_7,
          can_dmpsols_8,
          dep.var.labels.include = T,
          dep.var.labels = c("N Firms", "Employment"),
          covariate.labels = c("NTL",
                               "NTL$\\times$Firms Min",
                               "NTL$\\times$Firms Growth",
                               "NTL$\\times$Firms Min$\\times$Firms Growth",
                               "NTL$\\times$Employ Min",
                               "NTL$\\times$Employ Growth",
                               "NTL$\\times$Employ Min$\\times$Employ Growth"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("N"), 8)),
            c("Unit FE",rep(c("Y"), 8))),
          out=file.path(tables_file_path, "lm_id_interactions_can_dmspols.tex"))

stargazer(mex_dmpsols_1,
          mex_dmpsols_2,
          mex_dmpsols_3,
          mex_dmpsols_4,
          mex_dmpsols_5,
          mex_dmpsols_6,
          mex_dmpsols_7,
          mex_dmpsols_8,
          dep.var.labels.include = T,
          dep.var.labels = c("N Firms", "Employment"),
          covariate.labels = c("NTL",
                               "NTL$\\times$Firms Min",
                               "NTL$\\times$Firms Growth",
                               "NTL$\\times$Firms Min$\\times$Firms Growth",
                               "NTL$\\times$Employ Min",
                               "NTL$\\times$Employ Growth",
                               "NTL$\\times$Employ Min$\\times$Employ Growth"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("N"), 8)),
            c("Unit FE",rep(c("Y"), 8))),
          out=file.path(tables_file_path, "lm_id_interactions_mex_dmspols.tex"))

stargazer(mex_viirs_1,
          mex_viirs_2,
          mex_viirs_3,
          mex_viirs_4,
          mex_viirs_5,
          mex_viirs_6,
          mex_viirs_7,
          mex_viirs_8,
          dep.var.labels.include = T,
          dep.var.labels = c("N Firms", "Employment"),
          covariate.labels = c("NTL",
                               "NTL$\\times$Firms Min",
                               "NTL$\\times$Firms Growth",
                               "NTL$\\times$Firms Min$\\times$Firms Growth",
                               "NTL$\\times$Employ Min",
                               "NTL$\\times$Employ Growth",
                               "NTL$\\times$Employ Min$\\times$Employ Growth"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("N"), 8)),
            c("Unit FE",rep(c("Y"), 8))),
          out=file.path(tables_file_path, "lm_id_interactions_mex_viirs.tex"))

