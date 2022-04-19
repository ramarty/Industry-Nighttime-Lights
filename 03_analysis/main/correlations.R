df_can <- readRDS(file.path(data_file_path, "Grid", "FinalData", "canada", 
                            "merged_datasets", "firm_ntl.Rds"))

df_mex <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", 
                            "merged_datasets", "firm_ntl.Rds"))

df_dmsp_can <- df_can %>%
  dplyr::filter(!is.na(dmspharmon)) %>%
  group_by(unit, year) %>%
  dplyr::summarise(n_firms_dmsp = cor(n_firms,    dmspharmon),
                   n_firms_dmsp = cor(employment, dmspharmon))

df_dmsp_mex <- df_mex %>%
  dplyr::filter(!is.na(dmspharmon)) %>%
  group_by(unit, year) %>%
  dplyr::summarise(n_firms_dmsp = cor(n_firms, dmspharmon))

df_viirs_mex <- df_can %>%
  dplyr::filter(!is.na(viirs_c)) %>%
  group_by(unit, year) %>%
  dplyr::summarise(n_firms_viirs = cor(n_firms, viirs_c))

