# Analysis

# Load Data --------------------------------------------------------------------
grid <- readRDS(file.path(merged_data_grid, paste0("hex_10km_clean",".Rds")))
#gadm <- readRDS(file.path(data_file_path, "GADM", "FinalData", paste0("gadm36_CAN_0_sp",".Rds")))

grid <- list.files(merged_data_grid, pattern = "*_clean.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  filter(!is.na(unit)) %>%
  filter(!is.na(dmspol_mean)) %>%
  mutate(unit = unit %>% str_replace_all("hex_", ""))
grid$unit <- grid$unit %>% factor(levels = c("5km", "10km", "25km", "50km", "100km", "250km", "500km", "1000km"))

grid <- grid[(grid$dmspol_mean > 0) | !is.na(grid$employment_mean_all),]
grid_non0 <- grid[(grid$dmspol_mean > 0) & !is.na(grid$employment_mean_all),]

p <- ggplot(data = grid_non0, 
       aes(x = dmspol_mean_log, y = employment_sum_all_log)) +
  geom_point(size=.1) +
  labs(x="Log(Nighttime Lights)",
       y="Log(Total Employment)") +
  theme_ipsum() +
  facet_wrap(~unit,
             nrow = 2,
             scales = "free") 
ggsave(p, filename = file.path(figures_file_path, "employlog_ntllog_scatter.png"), height=6, width=10)





