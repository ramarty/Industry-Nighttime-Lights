# Analysis

# TODO: If want multiple DMSPOLS, include in same plot with different colors

country_i <- "canada"
industry_var_i <- "employment_sum_all_log"

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "Results", "regression_results_yrid.Rds"))

df <- df %>%
  distinct(FE, splag, industry_var, ntl_var, unit, country, dv_type, .keep_all = T) %>%
  dplyr::select(-var) %>%
  dplyr::mutate(unit = unit %>% factor(levels = c("5km Grid",
                                                  "10km Grid",
                                                  "25km Grid",
                                                  "50km Grid",
                                                  "100km Grid",
                                                  "City",
                                                  "Grid in Cities")))

df <- df %>%
  dplyr::filter(dv_type %in% "ntl",
                FE %in% "yr")

df <- df %>%
  dplyr::filter(country %in% "mexico",
               industry_var %in% "N_firms_sum_all_log",
               ntl_var %in% "viirs_mean_log")

df %>%
  ggplot() +
  geom_point(aes(y = unit,
                 x = P.adj.r.squared,
                 color = splag))

