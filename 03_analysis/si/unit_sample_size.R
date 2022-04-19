# Size of Dataset

# Load/prep data ---------------------------------------------------------------
## Canada
df_can <- readRDS(file.path(data_file_path, "Grid", "FinalData", "canada", 
                            "merged_datasets", "firm_ntl.Rds"))
df_can <- df_can %>%
  dplyr::filter(year %in% 2013)

## Mexico
df_mex <- readRDS(file.path(data_file_path, "Grid", "FinalData", "mexico", 
                            "merged_datasets", "firm_ntl.Rds"))
df_mex <- df_mex %>%
  dplyr::filter(year %in% 2020)

# Aggregate --------------------------------------------------------------------
n_can <- df_can %>%
  group_by(unit) %>%
  dplyr::summarise(n_can = n()) 

n_mex <- df_mex %>%
  group_by(unit) %>%
  dplyr::summarise(n_mex = n())

n_all <- n_can %>%
  left_join(n_mex, by = "unit")

## Add hexagon edge length
n_all$edge_km[n_all$unit %in% "hexgrid1"] <- 418.676005500	
n_all$edge_km[n_all$unit %in% "hexgrid2"] <- 158.244655800
n_all$edge_km[n_all$unit %in% "hexgrid3"] <- 59.810857940
n_all$edge_km[n_all$unit %in% "hexgrid4"] <- 22.606379400
n_all$edge_km[n_all$unit %in% "hexgrid5"] <- 8.544408276	
n_all$edge_km[n_all$unit %in% "hexgrid6"] <- 3.229482772
n_all$edge_km[n_all$unit %in% "hexgrid7"] <- 1.220629759	
n_all$edge_km[n_all$unit %in% "hexgrid8"] <- 0.461354684

## h3 res number
n_all$res <- n_all$unit %>% 
  str_replace_all("hexgrid", "")

# Table ------------------------------------------------------------------------
n_all <- n_all %>%
  dplyr::mutate(tex = paste(res, " & ",
                            edge_km %>% round(2) %>% formatC(format="d", big.mark=","), " & ",
                            n_can %>% formatC(format="d", big.mark=","), " & ", 
                            n_mex %>% formatC(format="d", big.mark=","), " \\\\ \n"))

sink(file.path(tables_file_path, "unit_sample_size.tex"))

cat("\\begin{tabular}{cccc} \n")
cat("\\hline \n")
cat("H3 Resolution & Edge Length & N, Canada & N, Mexico \\\\ \n")

for(i in 1:nrow(n_all)) cat(n_all$tex[i])

cat("\\hline \n")
cat("\\end{tabular}")

sink()




