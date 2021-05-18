# Correlation Figure: Changes

country <- "Canada"
firm_var_i <- "N_firms_sum_all"
ntl_var_i <- "DMSP-OLS"

# Load/Prep Across Unit Correlation --------------------------------------------
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

# Subset/Prep
df_out_all <- df_out_all %>%
  filter(difference != "level",
         transform %in% c("log"),
         ntl_var %in% c("dmspolsharmon_sum", "viirs_sum"),
         !(year %in% "All"),
         !is.na(b)) %>%
  mutate(ntl_var = ntl_var %>% as.character,
         transform = transform %>% as.character) %>%
  mutate(year = year %>% as.character() %>% as.numeric() %>% as.factor(),
         ntl_var = case_when(ntl_var %in% "dmspolsharmon_sum" ~ "DMSP-OLS",
                             ntl_var %in% "viirs_sum"         ~ "VIIRS"),
         firm_var = case_when(firm_var %in% "N_firms_sum_all"    ~ "N Firms",
                              firm_var %in% "employment_sum_all" ~ "Employment")) %>%
  mutate(diffyear   = paste(difference, year),
         difference = difference %>% str_replace_all("diff", "") %>% as.numeric,
         title      = paste0(country, "\n", ntl_var, " & ", firm_var)) %>%
  mutate(difference = case_when(country == "Mexico" & ntl_var == "VIIRS"    ~ 1 * difference,
                                country == "Mexico" & ntl_var == "DMSP-OLS" ~ 5 * difference,
                                country == "Canada"                         ~ 2 * difference) %>%
           as.factor()) %>%
  mutate(unit = unit %>% factor(levels = c("City",
                                           "Grid in Cities",
                                           "5km Grid",
                                           "10km Grid",
                                           "25km Grid",
                                           "50km Grid",
                                           "100km Grid")))

# Make Figure ------------------------------------------------------------------
N_colors <- df_out_all$difference %>% unique() %>% length()

p <- df_out_all %>%
  ggplot(aes(y = b,
             x = unit,
             color = difference,
             group = diffyear,
             ymin = ci_low,
             ymax = ci_high)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_linerange(position = position_dodge(width = 0.8)) +
  geom_point(position = position_dodge(width = 0.8),
             pch = 21) +
  scale_fill_manual(values = c("white", "black")) +
  labs(#title = title,
    y = "Correlation Coefficient (+/- 95% CI)",
    x = "",
    fill = "NTL Dataset",
    color = "Difference\n(Years)") +
  guides(color = guide_legend(reverse=T),
         fill = guide_legend(reverse=T)) +
  scale_color_manual(values = wes_palette("Zissou1", N_colors, type = "continuous")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_flip() +
  facet_wrap(~title,
             nrow = 1) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold"))

ggsave(p, filename = file.path(figures_file_path, "diff_cor_across.png"), height = 6, width=14)

