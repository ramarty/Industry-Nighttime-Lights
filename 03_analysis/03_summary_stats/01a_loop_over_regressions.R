# Summary Statistics

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all")

mex_dmspols <- mex_dmspols[mex_dmspols$unit %in% "5km Grid",]
mex_viirs   <- mex_viirs[mex_viirs$unit %in% "5km Grid",]
can         <- can[can$unit %in% "5km Grid",]

# Estimate Models --------------------------------------------------------------
make_stats <- function(df, var, year){
  
  df <- df[df$year %in% year,]
  
  if(var %in% "dmspols_mean")       varname <- "DMSP-OLS"
  if(var %in% "viirs_mean")         varname <- "VIIRS"
  if(var %in% "N_firms_sum_all")    varname <- "Establishment Count"
  if(var %in% "employment_sum_all") varname <- "Number of Employees"
  
  cat(
    varname, " & ",
    nrow(df) %>% prettyNum(big.mark=","), " & ",
    df[[var]] %>% min(na.rm=T) %>% round(2) %>% prettyNum(big.mark=","), " & ",
    df[[var]] %>% max(na.rm=T) %>% round(2) %>% prettyNum(big.mark=","), " & ",
    df[[var]] %>% mean(na.rm=T) %>% round(2) %>% prettyNum(big.mark=","), " & ",
    df[[var]] %>% median(na.rm=T) %>% round(2) %>% prettyNum(big.mark=","), " & ",
    df[[var]] %>% sd(na.rm=T) %>% round(2) %>% prettyNum(big.mark=","), " \\\\ \n"
  )
}

sink(file.path(tables_file_path, "sumstats.tex"))

cat("\\begin{tabular}{lllllll} \n")
cat("\\hline \n")
cat("Variable & N & Min & Max & Mean & Medain & Std. Dev. \\\\ \n" )
cat("\\hline \n")
cat("\\multicolumn{7}{c}{\\bf Canada, 2001} \\\\ \n")
make_stats(can, "dmspols_mean",       2001)
make_stats(can, "N_firms_sum_all",    2001)
make_stats(can, "employment_sum_all", 2001)

cat("\\hline \n")
cat("\\multicolumn{7}{c}{\\bf Canada, 2013} \\\\ \n")
make_stats(can, "dmspols_mean",       2013)
make_stats(can, "N_firms_sum_all",    2013)
make_stats(can, "employment_sum_all", 2013)

cat("\\hline \n")
cat("\\multicolumn{7}{c}{\\bf Mexico, 2004} \\\\ \n")
make_stats(mex_dmspols, "dmspols_mean",       2004)
make_stats(mex_dmspols, "N_firms_sum_all",    2004)
make_stats(mex_dmspols, "employment_sum_all", 2004)

cat("\\hline \n")
cat("\\multicolumn{7}{c}{\\bf Mexico, 2014} \\\\ \n")
make_stats(mex_viirs,   "viirs_mean",         2014)
make_stats(mex_dmspols, "dmspols_mean",       2014)
make_stats(mex_dmspols, "N_firms_sum_all",    2014)
make_stats(mex_dmspols, "employment_sum_all", 2014)

cat("\\hline \n")
cat("\\multicolumn{7}{c}{\\bf Mexico, 2020} \\\\ \n")
make_stats(mex_viirs,   "viirs_mean",         2020)
make_stats(mex_viirs, "N_firms_sum_all",    2020)
make_stats(mex_viirs, "employment_sum_all", 2020)

cat("\\hline \n")
cat("\\end{tabular}")

sink()
