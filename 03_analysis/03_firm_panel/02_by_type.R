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

run_regs <- function(df_dmspols, df_viirs, dmspols_var, viirs_var, indep_var){
  
  for(i in c(11,21,23,31,42,44,48,51,61,71,81)) df_dmspols[[paste0("indep_", i)]] <- df_dmspols[[paste0(indep_var, i, "_log")]]
  for(i in c(11,21,23,31,42,44,48,51,61,71,81)) df_viirs[[paste0("indep_", i)]]   <- df_viirs[[paste0(indep_var, i, "_log")]]
  
  df_dmspols$dmspols_var <- df_dmspols[[dmspols_var]]
  df_viirs$viirs_var     <- df_viirs[[viirs_var]]
  
  lm_dmspols_yr <- felm(dmspols_var ~ 
                          indep_11 +
                          indep_21 + 
                          indep_23 + 
                          indep_31 +
                          indep_42 + 
                          indep_44 + 
                          indep_48 + 
                          indep_51 + 
                          indep_61 +
                          indep_71 + 
                          indep_81 | year | 0 | 0, data = df_dmspols)
  
  lm_dmspols_id <- felm(dmspols_var ~ 
                          indep_11 +
                          indep_21 + 
                          indep_23 + 
                          indep_31 +
                          indep_42 + 
                          indep_44 + 
                          indep_48 + 
                          indep_51 + 
                          indep_61 +
                          indep_71 + 
                          indep_81 | id | 0 | 0, data = df_dmspols)
  
  lm_dmspols_yrid <- felm(dmspols_var ~ 
                            indep_11 +
                            indep_21 + 
                            indep_23 + 
                            indep_31 +
                            indep_42 + 
                            indep_44 + 
                            indep_48 + 
                            indep_51 + 
                            indep_61 +
                            indep_71 + 
                            indep_81 | year + id | 0 | 0, data = df_dmspols)
  
  # VIIRS
  lm_viirs_yr <- felm(viirs_var ~ 
                        indep_11 +
                        indep_21 + 
                        indep_23 + 
                        indep_31 +
                        indep_42 + 
                        indep_44 + 
                        indep_48 + 
                        indep_51 + 
                        indep_61 +
                        indep_71 + 
                        indep_81 | year | 0 | 0, data = df_viirs)
  
  lm_viirs_id <- felm(viirs_var ~ 
                        indep_11 +
                        indep_21 + 
                        indep_23 + 
                        indep_31 +
                        indep_42 + 
                        indep_44 + 
                        indep_48 + 
                        indep_51 + 
                        indep_61 +
                        indep_71 + 
                        indep_81 | id | 0 | 0, data = df_viirs)
  
  lm_viirs_yrid <- felm(viirs_var ~ 
                          indep_11 +
                          indep_21 + 
                          indep_23 + 
                          indep_31 +
                          indep_42 + 
                          indep_44 + 
                          indep_48 + 
                          indep_51 + 
                          indep_61 +
                          indep_71 + 
                          indep_81 | year + id | 0 | 0, data = df_viirs)
  
  return(list(lm_dmspols_yr = lm_dmspols_yr,
              lm_dmspols_id = lm_dmspols_id,
              lm_dmspols_yrid = lm_dmspols_yrid,
              lm_viirs_yr = lm_viirs_yr,
              lm_viirs_id = lm_viirs_id,
              lm_viirs_yrid = lm_viirs_yrid))
  
}

lm_can_firms  <- run_regs(can_df, can_df, "dmspolselvidge_mean_log", "viirs_mean_log", "N_firms_sum_")
lm_can_employ <- run_regs(can_df, can_df, "dmspolselvidge_mean_log", "viirs_mean_log", "employment_sum_")

lm_mex_firms  <- run_regs(mex_dmspols_df, mex_viirs_df, "dmspolselvidge_mean_log", "viirs_mean_log", "N_firms_sum_")
lm_mex_employ <- run_regs(mex_dmspols_df, mex_viirs_df, "dmspolselvidge_mean_log", "viirs_mean_log", "employment_sum_")

stargazer(lm_can_firms$lm_dmspols_yr,
          lm_can_firms$lm_dmspols_id,
          lm_can_firms$lm_dmspols_yrid,
          lm_can_firms$lm_viirs_yr,
          lm_can_firms$lm_viirs_id,
          lm_can_firms$lm_viirs_yrid,
          
          lm_mex_firms$lm_dmspols_yr,
          lm_mex_firms$lm_dmspols_id,
          lm_mex_firms$lm_dmspols_yrid,
          lm_mex_firms$lm_viirs_yr,
          lm_mex_firms$lm_viirs_id,
          lm_mex_firms$lm_viirs_yrid,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS", "VIIRS", "DMSP-OLS", "VIIRS"),
          covariate.labels = c("Firms: 11",
                               "Firms: 21",
                               "Firms: 23",
                               "Firms: 31",
                               "Firms: 42",
                               "Firms: 44",
                               "Firms: 48",
                               "Firms: 51",
                               "Firms: 61",
                               "Firms: 71",
                               "Firms: 81"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("Y", "N", "Y"), 4)),
            c("Unit FE",rep(c("N", "Y", "Y"), 4)),
            c("Country",rep("CAN", 6), rep("MEX", 6))),
          out=file.path(tables_file_path, "lm_bytype_firms.tex"))

stargazer(lm_can_employ$lm_dmspols_yr,
          lm_can_employ$lm_dmspols_id,
          lm_can_employ$lm_dmspols_yrid,
          lm_can_employ$lm_viirs_yr,
          lm_can_employ$lm_viirs_id,
          lm_can_employ$lm_viirs_yrid,
          
          lm_mex_employ$lm_dmspols_yr,
          lm_mex_employ$lm_dmspols_id,
          lm_mex_employ$lm_dmspols_yrid,
          lm_mex_employ$lm_viirs_yr,
          lm_mex_employ$lm_viirs_id,
          lm_mex_employ$lm_viirs_yrid,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS", "VIIRS", "DMSP-OLS", "VIIRS"),
          covariate.labels = c("Firms: 11",
                               "Firms: 21",
                               "Firms: 23",
                               "Firms: 31",
                               "Firms: 42",
                               "Firms: 44",
                               "Firms: 48",
                               "Firms: 51",
                               "Firms: 61",
                               "Firms: 71",
                               "Firms: 81"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          report="vcs*",
          digits = 2,
          add.lines = list(
            c("Year FE",rep(c("Y", "N", "Y"), 4)),
            c("Unit FE",rep(c("N", "Y", "Y"), 4)),
            c("Country",rep("CAN", 6), rep("MEX", 6))),
          out=file.path(tables_file_path, "lm_bytype_employ.tex"))



