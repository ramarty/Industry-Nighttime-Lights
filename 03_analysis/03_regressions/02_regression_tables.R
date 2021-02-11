# Analysis

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all")

# Estimate Models --------------------------------------------------------------
for(industry_var in c("employment_sum_all_log", "N_firms_sum_all_log")){
  #for(ntl_var in c("dmspols_mean_log", "dmspolselvidge_mean_log", "dmspolszhang_mean_log", "viirs_mean_log")){
  for(unit in c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid", "250km Grid", "500km Grid", "1000km Grid")){
    for(country in c("mexico", "canada")){
      
      print(paste(industry_var, unit, country, sep=" - "))
      
      ## Define data
      if(country %in% "canada"){
        df_dmsp <- can
        df_viirs <- can
      } 
      if(country %in% "mexico"){
        df_dmsp <- mex_dmspols
        df_viirs <- mex_viirs
      } 

      ## Text
      unit_simple <- unit %>% str_replace_all(" Grid", "")
      
      ## Add variables
      df_dmsp$industry_var  <- df_dmsp[[industry_var]]
      df_viirs$industry_var <- df_viirs[[industry_var]]
      
      ## Regressions
      lm_dmspols_yr        <- felm(industry_var ~ dmspols_mean_log        | year        | 0 | 0, data = df_dmsp[df_dmsp$unit %in% unit,])
      lm_dmspolselvidge_yr <- felm(industry_var ~ dmspolselvidge_mean_log | year        | 0 | 0, data = df_dmsp[df_dmsp$unit %in% unit,])  
      lm_dmspolszhang_yr   <- felm(industry_var ~ dmspolszhang_mean_log   | year        | 0 | 0, data = df_dmsp[df_dmsp$unit %in% unit,]) 
      lm_viirs_mean_yr     <- felm(industry_var ~ viirs_mean_log          | year        | 0 | 0, data = df_viirs[df_viirs$unit %in% unit,]) 
      
      lm_dmspols_yrid        <- felm(industry_var ~ dmspols_mean_log        | year + id | 0 | 0, data = df_dmsp[df_dmsp$unit %in% unit,]) 
      lm_dmspolselvidge_yrid <- felm(industry_var ~ dmspolselvidge_mean_log | year + id | 0 | 0, data = df_dmsp[df_dmsp$unit %in% unit,]) 
      lm_dmspolszhang_yrid   <- felm(industry_var ~ dmspolszhang_mean_log   | year + id | 0 | 0, data = df_dmsp[df_dmsp$unit %in% unit,]) 
      lm_viirs_mean_yrid     <- felm(industry_var ~ viirs_mean_log          | year + id | 0 | 0, data = df_viirs[df_viirs$unit %in% unit,]) 
      
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






