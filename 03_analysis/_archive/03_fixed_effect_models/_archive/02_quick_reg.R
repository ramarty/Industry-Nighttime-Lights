# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all")

# Estimate Models --------------------------------------------------------------
can <- can %>%
  group_by(id) %>%
  mutate(N_firms_sum_all_log_max = max(N_firms_sum_all_log)) %>%
  ungroup() %>%
  filter(N_firms_sum_all_log_max > 0)
mex_dmspols <- mex_dmspols %>%
  group_by(id) %>%
  mutate(N_firms_sum_all_log_max = max(N_firms_sum_all_log)) %>%
  ungroup() %>%
  filter(N_firms_sum_all_log_max > 0)
mex_viirs <- mex_viirs %>%
  group_by(id) %>%
  mutate(N_firms_sum_all_log_max = max(N_firms_sum_all_log)) %>%
  ungroup() %>%
  filter(N_firms_sum_all_log_max > 0)

unit <- "5km Grid"
lm_dmspols_can_employ_yr <- felm(employment_sum_all_log ~ dmspols_mean_log | year | 0 | 0, data = can[can$unit %in% unit,])
lm_viirs_can_employ_yr   <- felm(employment_sum_all_log ~ viirs_mean_log   | year | 0 | 0, data = can[can$unit %in% unit,]) 
lm_dmspols_can_firms_yr <- felm(N_firms_sum_all_log ~ dmspols_mean_log | year | 0 | 0, data = can[can$unit %in% unit,])
lm_viirs_can_firms_yr   <- felm(N_firms_sum_all_log ~ viirs_mean_log   | year | 0 | 0, data = can[can$unit %in% unit,]) 

lm_dmspols_mex_employ_yr <- felm(employment_sum_all_log ~ dmspols_mean_log | year | 0 | 0, data = mex_dmspols[mex_dmspols$unit %in% unit,])
lm_viirs_mex_employ_yr   <- felm(employment_sum_all_log ~ viirs_mean_log   | year | 0 | 0, data = mex_viirs[mex_viirs$unit %in% unit,]) 
lm_dmspols_mex_firms_yr <- felm(N_firms_sum_all_log ~ dmspols_mean_log | year | 0 | 0, data = mex_dmspols[mex_dmspols$unit %in% unit,])
lm_viirs_mex_firms_yr   <- felm(N_firms_sum_all_log ~ viirs_mean_log   | year | 0 | 0, data = mex_viirs[mex_viirs$unit %in% unit,]) 

stargazer(lm_dmspols_can_employ_yr,
          lm_viirs_can_employ_yr,
          lm_dmspols_can_firms_yr,
          lm_viirs_can_firms_yr,
          
          lm_dmspols_mex_employ_yr,
          lm_viirs_mex_employ_yr,
          lm_dmspols_mex_firms_yr,
          lm_viirs_mex_firms_yr,
          dep.var.labels.include = T,
          dep.var.labels = c("Employment", "Firms", "Employment", "Firms"),
          #column.labels   = c("Values Index"),
          #keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNA.30km.bin"),
          covariate.labels = c("DMSP-OLS","VIIRS"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("Country", rep("CAN", 4), rep("MEX", 4))),
          out=file.path("~/Desktop", "regs", "table.tex"))


stargazer(lm_dmspols_yr,
          lm_dmspolselvidge_yr,
          lm_dmspolszhang_yr,
          lm_viirs_mean_yr,
          lm_dmspols_yrid,
          lm_dmspolselvidge_yrid,
          lm_dmspolszhang_yrid,
          lm_viirs_mean_yrid,
          dep.var.labels.include = F,
          #column.labels   = c("Values Index"),
          #keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNA.30km.bin"),
          #covariate.labels = c("China Completed","China Planned"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          add.lines = list(
            c("FE", "Y", "Y", "Y", "Y","Y", "Y")),
          out=file.path(tables_file_path, paste0("reg_level_",industry_var, "_", country, "_", unit_simple,".tex")))



for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  #for(ntl_var in c("dmspols_mean_log", "dmspolselvidge_mean_log", "dmspolszhang_mean_log", "viirs_mean_log")){
  for(unit in c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid", "250km Grid", "500km Grid", "1000km Grid")){
    for(country in c("mexico", "canada")){
      


      ## Stargzer
      stargazer(lm_dmspols_yr,
                lm_dmspolselvidge_yr,
                lm_dmspolszhang_yr,
                lm_viirs_mean_yr,
                lm_dmspols_yrid,
                lm_dmspolselvidge_yrid,
                lm_dmspolszhang_yrid,
                lm_viirs_mean_yrid,
                dep.var.labels.include = F,
                #column.labels   = c("Values Index"),
                #keep=c("completed_near_china.plNA.30km.bin", "planned_near_china.plNA.30km.bin"),
                #covariate.labels = c("China Completed","China Planned"),
                dep.var.caption = "",
                omit.stat = c("f","ser"), 
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width = "8pt",
                #report="vcs*",
                digits = 2,
                add.lines = list(
                  c("FE", "Y", "Y", "Y", "Y","Y", "Y")),
                out=file.path(tables_file_path, paste0("reg_level_",industry_var, "_", country, "_", unit_simple,".tex")))

    }
  }
}






