# Summary Stats Table

# Load/prep data ---------------------------------------------------------------
## Canada
df_can <- readRDS(file.path(data_file_path, "Grid", "FinalData", "canada", 
                            "merged_datasets", "firm_ntl.Rds"))
df_can <- df_can %>%
  dplyr::filter(unit %in% "hexgrid6")

## Mexico
df_mex <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", 
                            "merged_datasets", "firm_ntl.Rds"))
df_mex <- df_mex %>%
  dplyr::filter(unit %in% "hexgrid6")

# Table ------------------------------------------------------------------------
df_can$dmspharmon %>% is.na %>% table()
df_can$n_firms %>% is.na %>% table()

make_sum_stats <- function(name,
                           var, 
                           df, 
                           r_num = 2){
  df$var <- df[[var]]
  df <- df[!is.na(df$var),]
  
  cat(name, " & ",
      df$var %>% min()  %>% round(r_num), " & ",
      df$var %>% max()  %>% round(r_num), " & ",
      df$var %>% mean() %>% round(r_num), " & ",
      df$var %>% sd()   %>% round(r_num), " \\\\ \n ")
}

sink(file.path(tables_file_path, "sumstats.tex"))

cat("\\begin{tabular}{l | cccc} \n")
cat("\\hline \n")
cat("Variable & Min & Max & Mean & Std. Dev. \\\\ \n")

cat("\\multicolumn{2}{c}{Canada} \n")
make_sum_stats("N Firms", "n_firms", df_can)
make_sum_stats("Employment", "employment", df_can)
make_sum_stats("DMSP Harmon", "dmspharmon", df_can)

cat("\\multicolumn{2}{c}{Mexico} \n")
make_sum_stats("N Firms", "n_firms", df_mex)
make_sum_stats("DMSP Harmon", "dmspharmon", df_mex)
make_sum_stats("VIIRS", "viirs_c", df_mex)

cat("\\hline \n")
cat("\\end{tabular}")

sink()
