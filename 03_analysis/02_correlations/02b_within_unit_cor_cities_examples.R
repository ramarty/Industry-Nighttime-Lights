# Analysis

library(ggtext)

# Load Data --------------------------------------------------------------------

#can <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData",
#                         "canada", "merged_appended_allunits", "can_notype.Rds")) 

mex_viirs <- readRDS(file.path(project_file_path, "Data", "Grid", "FinalData", 
                               "mexico", "merged_appended_allunits", "mex_viirs_notype.Rds")) 

# Prep Data --------------------------------------------------------------------
mex_viirs <- mex_viirs %>%
  dplyr::filter(unit == "5km Grid") %>%
  group_by(id) %>%
  mutate(firms_min = min(N_firms_sum_all_log),
         firms_max = max(N_firms_sum_all_log)) %>%
  mutate(cor = cor(N_firms_sum_all_log, viirs_mean_log)) %>%
  ungroup() %>%
  mutate(diff = abs(firms_max - firms_min)) %>%
  arrange(desc(cor))

#mex_viirs$city_name <- mex_viirs$city_name %>% str_replace_all("[:digit:]", "")

#cities <- mex_viirs$city_name %>% unique() 
#cities <- cities[100:105]
ids <- mex_viirs$id %>% unique() %>% head()

v <- mex_viirs %>% 
  filter(id %in% ids) %>%
  ggplot(aes(x = year,
             y = viirs_mean)) +
  geom_line() +
  geom_point() +
  facet_wrap(~id,
             ncol = 1,
             scales="free_y") + 
  labs(title = "VIIRS") +
  theme_minimal()

f <- mex_viirs %>% 
  filter(id %in% ids) %>%
  ggplot(aes(x = year,
             y = N_firms_sum_all)) +
  geom_line() +
  geom_point() +
  facet_wrap(~id,
             ncol = 1,
             scales="free_y") +
  labs(title = "N Firms") +
  theme_minimal()

p <- ggarrange(v, f, 
               ncol = 2)

ggsave(p, filename = file.path(figures_file_path,
                               "example_within_unit.png"),
       height = 5,
       width = 5)







