# Extract DMSP-OLS

country <- "canada" 

country_title <- country %>% tools::toTitleCase()

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", 
                               paste0(country_title, " Industry Data"), 
                               "FinalData","firms_clean.Rds"))
df <- df@data

df_sum <- df %>%
  mutate(firms = 1) %>%
  group_by(naicsname, year) %>%
  summarise(employment = sum(employment, na.rm = T),
            firms = sum(firms)) %>%
  ungroup()

# Figures ----------------------------------------------------------------------
p_firms <- ggplot(data = df_sum, 
                  aes(x = year, y= firms)) +
  geom_line(color = "dodgerblue4") +
  geom_point(color = "dodgerblue4") +
  facet_wrap(~naicsname, 
             scales = "free_y", ncol = 2) +
  labs(title = "Firms", x = "", y = "") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

# Employment
p_employ <- ggplot(data = df_sum, 
                   aes(x = year, y= employment)) +
  geom_line(color = "darkorange3") +
  geom_point(color = "darkorange3") +
  facet_wrap(~naicsname, 
             scales = "free_y", ncol = 2) +
  labs(title = "Employment", x = "", y = "") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

# Export -----------------------------------------------------------------------
p_all <- ggarrange(p_firms,
                   p_employ)

ggsave(p_all, filename = file.path(figures_file_path, 
                               paste0(country, "_employment_firms_trends.png")),
       height = 18, width = 18)



