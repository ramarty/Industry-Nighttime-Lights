# Correlation Figure

STIP_TEXT_SIZE = 12
AXIS_TEXT_Y_SIZE = 14
PLOT_TITLE_SIZE = 16
AXIS_TITLE_X_SIZE = 14
AXIS_TEXT_X_SIZE = 14

# COR. LEVELS ==================================================================
## Load Data
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

## Rename
df_out_all$ntl_var <- df_out_all$ntl_var %>% as.character()
df_out_all$ntl_var[df_out_all$ntl_var %in% "dmspolsharmon_sum"] <- "DMSP"
df_out_all$ntl_var[df_out_all$ntl_var %in% "viirs_sum"] <- "VIIRS"

df_out_all$transform <- df_out_all$transform %>% as.character()
df_out_all$transform[df_out_all$transform %in% "level"] <- "Levels"

## Subset/Prep Variables
df_out_all <- df_out_all %>%
  filter(difference %in% "level",
         ntl_var %in% c("DMSP", "VIIRS"),
         transform %in% c("log"), # Levels, log
         !(year %in% "All")) %>%
  mutate(year = year %>% as.character() %>% as.numeric() %>% as.factor()) %>%
  mutate(unit = unit %>% factor(levels = c("City",
                                           "Grid in Cities",
                                           "5km Grid",
                                           "10km Grid",
                                           "25km Grid",
                                           "50km Grid",
                                           "100km Grid")))

df_out_all <- df_out_all %>%
  dplyr::filter(!(firm_var == "employment_sum_all" & country == "Mexico")) %>%
  dplyr::mutate(firm_var =
                  case_when(firm_var %in% "N_firms_sum_all" ~ "N Firms",
                            firm_var %in% "employment_sum_all" ~ "Employment"))

df_out_all <- df_out_all %>%
  dplyr::filter(!is.na(b))

df_out_all$country <- df_out_all$country %>% fct_rev()

df_out_all <- df_out_all %>%
  dplyr::mutate(year = year %>% as.character %>% as.numeric)
N_colors <- df_out_all$year %>% unique() %>% length()

N_colors <- (max(df_out_all$year) - min(df_out_all$year)) + 1

##### ** Figure #####
p_level <- df_out_all %>%
  ggplot(aes(y = b,
             x = unit,
             color = year,
             group = year,
             fill = ntl_var,
             ymin = ci_low,
             ymax = ci_high)) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_linerange(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             pch = 21) +
  ylim(c(0,1)) +
  scale_fill_manual(values = c("white", "black")) +
  labs(y = "Correlation Coefficient (+/- 95% CI)",
       x = NULL,
       title = "(A) Correlation of levels of NTL and firm outcomes",
       fill = "NTL Dataset",
       color = "Year") +
  guides(#color = guide_legend(reverse=T),
    fill = guide_legend(reverse=T)) +
  scale_color_gradientn(colours = wes_palette("Zissou1", N_colors, type = "continuous"),
                        limits = c(2000, 2020)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = PLOT_TITLE_SIZE),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = STIP_TEXT_SIZE),
        axis.title.x = element_text(size = AXIS_TITLE_X_SIZE),
        axis.text.x = element_text(size = AXIS_TEXT_X_SIZE),
        axis.text.y = element_text(face = "bold", color = "black", size = AXIS_TEXT_Y_SIZE)) +
  facet_wrap(~country+firm_var, 
             #scales = "free_x",
             nrow=1) +
  coord_flip() 

# COR. CHANGES ACCROSS =========================================================

# Load/Prep Across Unit Correlation 
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
         ntl_var = case_when(ntl_var %in% "dmspolsharmon_sum" ~ "DMSP",
                             ntl_var %in% "viirs_sum"         ~ "VIIRS"),
         firm_var = case_when(firm_var %in% "N_firms_sum_all"    ~ "N Firms",
                              firm_var %in% "employment_sum_all" ~ "Employment")) %>%
  mutate(diffyear   = paste(difference, year),
         difference = difference %>% str_replace_all("diff", "") %>% as.numeric,
         title      = paste0(country, "\n", ntl_var, " & ", firm_var)) %>%
  mutate(difference = case_when(country == "Mexico" & ntl_var == "VIIRS"    ~ 1 * difference,
                                country == "Mexico" & ntl_var == "DMSP" ~ 5 * difference,
                                country == "Canada"                         ~ 2 * difference) %>%
           as.factor()) %>%
  mutate(unit = unit %>% factor(levels = c("City",
                                           "Grid in Cities",
                                           "5km Grid",
                                           "10km Grid",
                                           "25km Grid",
                                           "50km Grid",
                                           "100km Grid")))

# Make Figure 
N_colors <- df_out_all$difference %>% unique() %>% length()

##### ** Figure #####
p_accross <- df_out_all %>%
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
    x = NULL,
    title = "(B) Correlation of changes in NTL and firm outcomes across units",
    fill = "NTL Dataset",
    color = "Difference\n(Years)") +
  guides(color = guide_legend(reverse=T),
         fill = guide_legend(reverse=T)) +
  scale_color_manual(values = wes_palette("Zissou1", N_colors, type = "continuous")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, face = "bold", size = PLOT_TITLE_SIZE),
        axis.text.y = element_text(size = AXIS_TEXT_Y_SIZE, face = "bold", color = "black"),
        axis.title.x = element_text(size = AXIS_TITLE_X_SIZE),
        axis.text.x = element_text(size = AXIS_TEXT_X_SIZE),
        strip.text = element_text(face = "bold", size = STIP_TEXT_SIZE)) +
  coord_flip() +
  facet_wrap(~title,
             nrow = 1) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold"))

# COR. CHANGES WITHIN ==========================================================

# Load/Prep Within Unit Correlation 
cor_within <- readRDS(file.path(data_file_path, "Results", "correlation_within_unit.Rds"))
cor_within <- cor_within %>%
  dplyr::filter(!is.na(cor),
                transform == "log",
                ntl_var %in% c("dmspolsharmon_mean", "viirs_mean")) %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolsharmon_mean" ~ "DMSP",
                                    ntl_var %in% "viirs_mean"        ~ "VIIRS"),
                firm_var = case_when(firm_var %in% "N_firms_sum_all"    ~ "N Firms",
                                     firm_var %in% "employment_sum_all" ~ "Employment")) %>%
  dplyr::mutate(unit = unit %>% factor(levels = rev(c("City",
                                                      "Grid in Cities",
                                                      "5km Grid",
                                                      "10km Grid",
                                                      "25km Grid",
                                                      "50km Grid",
                                                      "100km Grid"))))

cor_within <- cor_within %>%
  dplyr::filter(
    (country == "Canada" & ntl_var == "DMSP" & firm_var == "Employment") |
      (country == "Canada" & ntl_var == "DMSP" & firm_var == "N Firms") |
      (country == "Mexico" & ntl_var == "DMSP" & firm_var == "N Firms") |
      (country == "Mexico" & ntl_var == "VIIRS" & firm_var == "N Firms")
  ) %>%
  mutate(ntl_and_firm_var = paste0(ntl_var, " & ", firm_var))

##### ** Make figure #####
cor_within$cor_pos <- ifelse(cor_within$cor >= 0, "pos", "neg")


cor_within <- cor_within %>%
  mutate(cor_round = round(cor*10)/10) %>%
  group_by(cor_round,cor_pos, unit, country, ntl_var, ntl_and_firm_var) %>%
  dplyr::summarise(N = n()) %>%
  mutate(cor_pos = ifelse(cor_round > 0, "pos", "neg"),
         cor_round = as.factor(cor_round))

cor_within <- cor_within %>%
  mutate(year_info = case_when(
    country == "Canada" & ntl_var == "DMSP" ~ "2001-2013 / 7 Obs.",
    country == "Mexico" & ntl_var == "DMSP" ~ "2004-2013 / 3 Obs.",
    country == "Mexico" & ntl_var == "VIIRS" ~ "2014-2020 / 5 Obs."
  ))

p_within <- cor_within %>%
  ggplot(aes(x = cor_round,
             y = N)) +
  #geom_histogram(fill = "dodgerblue2",
  #               color = "black",
  #               color = "dodgerblue4",
  #               alpha = 0.8,
  #               bins = 21) +
  geom_col(aes(fill = cor_pos),
           color = NA,
           #color = "black",
           #color = "dodgerblue4",
           alpha = 0.8) +
  scale_fill_manual(values = c("dodgerblue", "orange")) +
  #scale_color_manual(values = c("dodgerblue4", "orange3")) +
  scale_x_discrete(labels = c("-1", "", "", "", "", "-.5", "", "", "", "", 
                              "0",
                              "", "", "", "", ".5", "", "", "", "", "1")) +
  labs(x = "Correlation Coefficient",
       title = "(C) Distribution of correlation of NTL and firm outcomes within units") +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_text(size = AXIS_TITLE_X_SIZE),
        axis.text.x = element_text(size = AXIS_TEXT_X_SIZE),
        strip.text.x = element_text(size = STIP_TEXT_SIZE, face = "bold"),
        strip.text.y.left = element_text(angle=0, size = STIP_TEXT_SIZE, face = "bold"),
        legend.position = "none",
        plot.title = element_text(hjust = 0, face = "bold", size = PLOT_TITLE_SIZE)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  facet_grid(unit~country+ntl_and_firm_var+year_info,
             scale = "free_y",
             switch="y") 

# FIGURE ARRANGE ===============================================================
p_change_all <- ggarrange(p_level,
                          p_accross,
                          p_within,
                          ncol = 1,
                          heights = c(0.31, 0.32, 0.37))
ggsave(p_change_all, filename = file.path(figures_file_path, "cor_fig_all.png"), height = 16, width=12)





