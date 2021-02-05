# Analysis

country <- "mexico"

mex <- readRDS(file.path(project_file_path, "Data", 
                         "Grid",
                         "FinalData",
                         country,
                         "merged_datasets",
                         "hex_100km_viirs_clean.Rds"))

cor.test(mex$empl_med_fact_sum_all_diff3, mex$viirs_mean_log_diff3)

mex$empl_med_sum_log



