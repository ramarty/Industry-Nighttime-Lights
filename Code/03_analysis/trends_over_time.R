# Analysis

# Load Data --------------------------------------------------------------------
grid <- readRDS(file.path(merged_data_grid, paste0("hex_5km_clean",".Rds")))

grid_sum <- grid %>%
  group_by(year) %>%
  dplyr::summarise(dmspol_mean = mean(dmspol_mean, na.rm=T),
                   employment_sum_all = sum(employment_sum_all, na.rm=T),
                   N_firms_sum_all = sum(N_firms_sum_all, na.rm=T))


p_employ <- ggplot() +
  geom_line(data=grid_sum, aes(x=year, y=employment_sum_all), size=2) +
  labs(x= "",
       y="",
       title = "Employment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 
p_firms <- ggplot() +
  geom_line(data=grid_sum, aes(x=year, y=N_firms_sum_all), size=2) +
  labs(x= "",
       y="",
       title = "Number of Firms") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

p_dmspols <- ggplot() +
  geom_line(data=grid_sum, aes(x=year, y=dmspol_mean), size=2) +
  labs(x= "",
       y="",
       title = "Average Nighttime Lights") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

p <- ggarrange(p_dmspols,
          p_employ,
          p_firms,
          ncol=1)
ggsave(p, filename = file.path(figures_file_path, paste0("trends_over_time.png")), height=8, width=6)



