# Extract DMSP-OLS

# Only check for long term changes --where have values for start and end years

# Load Data --------------------------------------------------------------------
hexagon_data <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "hexagon_data", "hexagons_data.Rds"))
hexagon_data <- hexagon_data[!is.na(hexagon_data$employment_mean),]
hexagon_data <- hexagon_data[!is.na(hexagon_data$dmspols_mean),]

hexagon_data <- hexagon_data[hexagon_data$year %in% c(2005, 2013),]

# Only keep cells where have observations in every year
hexagon_data <- hexagon_data %>%
  group_by(id) %>%
  mutate(N_id = n()) %>%
  ungroup()
hexagon_data <- hexagon_data[hexagon_data$N_id %in% 2,]

# First difference and logs
hexagon_data <- hexagon_data %>% 
  group_by(id) %>% 
  arrange(id, year) %>%
  
  mutate(employment_mean.log = log(employment_mean),
         dmspols_mean.log    = log(dmspols_mean)) %>%
  
  mutate(employment_mean.diff = employment_mean - lag(employment_mean),
         dmspols_mean.diff    = dmspols_mean    - lag(dmspols_mean),
         employment_mean.log.diff = employment_mean.log - lag(employment_mean.log),
         dmspols_mean.log.diff    = dmspols_mean.log    - lag(dmspols_mean.log)) %>%

  filter(!is.na(employment_mean.diff) & !is.na(dmspols_mean.diff)) 

hexagon_data$employment_mean.log.diff[hexagon_data$employment_mean.log.diff %in% c(-Inf, Inf)] <- NA
hexagon_data$dmspols_mean.log.diff[hexagon_data$dmspols_mean.log.diff %in% c(-Inf, Inf)] <- NA


hexagon_data$dmspols_mean.log.diff


lm(employment_mean.diff ~ dmspols_mean.diff, data=hexagon_data[abs(hexagon_data$dmspols_mean.log.diff) < .5,]) %>% summary()

p <- ggplot(data=hexagon_data[abs(hexagon_data$dmspols_mean.log.diff) < 2,], 
            aes(x=dmspols_mean.log.diff, employment_mean.log.diff)) +
  geom_point() +
  stat_smooth() +
  labs(x="Luminosity",
       y="Average\nEmployment\nof Firms\nwithin Cell") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = .5))
p

ggplot(data = hexagon_data,
       aes(x=dmspols_mean.diff.log, y=employment_mean.diff.log)) +
  geom_point()

plot(employment_mean.diff.log, log(hexagon_data$dmspols_mean.diff)+1)



head(hexagon_data)


#### Round to nearest DMSP-OLS whole numbers
hexagon_data$dmspols_round <- hexagon_data$dmspols_mean %>% round()
hexagon_data$dmspols_round %% 2

#### Aggregate to DMSP Whole Number
hexagon_data_sum_df <- hexagon_data %>%
  group_by(dmspols_round) %>%
  summarise(N_mean = sum(N_sum, na.rm=T),
            employment_mean = mean(employment_mean, na.rm=T),
            employment_median = median(employment_mean, na.rm=T))

#### Figure
p <- ggplot(data=hexagon_data_sum_df, aes(x=dmspols_round, employment_mean)) +
  geom_point() +
  stat_smooth() +
  labs(x="Luminosity",
       y="Average\nEmployment\nof Firms\nwithin Cell") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = .5))

ggsave(p, filename = file.path(figures_file_path, "avg_employ_lum_trend_hex.png"), height=3.5, width=5)



