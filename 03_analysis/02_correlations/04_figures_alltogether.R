# Correlation Figure


# COR. LEVELS ==================================================================
## Load Data
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

## Rename
df_out_all$ntl_var <- df_out_all$ntl_var %>% as.character()
df_out_all$ntl_var[df_out_all$ntl_var %in% "dmspolsharmon_sum"] <- "DMSP-OLS"
df_out_all$ntl_var[df_out_all$ntl_var %in% "viirs_sum"] <- "VIIRS"

df_out_all$transform <- df_out_all$transform %>% as.character()
df_out_all$transform[df_out_all$transform %in% "level"] <- "Levels"

## Subset/Prep Variables
df_out_all <- df_out_all %>%
  filter(difference %in% "level",
         ntl_var %in% c("DMSP-OLS", "VIIRS"),
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

# Make Figure Function 
make_figure <- function(country, df_out_all){
  
  df_out_all <- df_out_all[df_out_all$country %in% country,]
  
  df_out_all <- df_out_all %>%
    filter(!is.na(b)) 
  N_colors <- df_out_all$year %>% unique() %>% length()
  
  title <- country
  
  df_out_all %>%
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
    labs(title = title,
         y = "Correlation Coefficient (+/- 95% CI)",
         x = "",
         fill = "NTL Dataset",
         color = "Year") +
    guides(color = guide_legend(reverse=T),
           fill = guide_legend(reverse=T)) +
    #theme_minimal() +
    scale_color_manual(values = wes_palette("Zissou1", N_colors, type = "continuous")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~firm_var, 
               #scales = "free_x",
               nrow=1) +
    coord_flip() 
  
}

# Make and Append Figures 
p_mex <- make_figure("Mexico", df_out_all)
p_can <- make_figure("Canada", df_out_all)

p_level <- ggarrange(p_can,
                     p_mex,
                     widths = c(0.6, 0.4)) 

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

# Make Figure 
N_colors <- df_out_all$difference %>% unique() %>% length()

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



# COR. CHANGES WITHIN ==========================================================

# Load/Prep Within Unit Correlation --------------------------------------------
cor_within <- readRDS(file.path(data_file_path, "Results", "correlation_within_unit.Rds"))
cor_within <- cor_within %>%
  dplyr::filter(!is.na(cor),
                transform == "log",
                ntl_var %in% c("dmspolsharmon_sum", "viirs_sum")) %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolsharmon_sum" ~ "DMSP-OLS",
                                    ntl_var %in% "viirs_sum"        ~ "VIIRS"),
                firm_var = case_when(firm_var %in% "N_firms_sum_all"    ~ "N Firms",
                                     firm_var %in% "employment_sum_all" ~ "Employment")) %>%
  dplyr::mutate(unit = unit %>% factor(levels = rev(c("City",
                                                      "Grid in Cities",
                                                      "5km Grid",
                                                      "10km Grid",
                                                      "25km Grid",
                                                      "50km Grid",
                                                      "100km Grid"))))

# Function to Make Figure ------------------------------------------------------
make_figure <- function(country_name, ntl_var_name, firm_var_name){
  
  if(country_name %in% "Canada"){
    fill_color = "dodgerblue"
  } else{
    fill_color = "darkorange1"
  }
  
  title = paste0(country_name, "\n", ntl_var_name, " & ", firm_var_name)
  
  cor_within %>%
    dplyr::filter(country %in% country_name,
                  firm_var %in% firm_var_name,
                  ntl_var %in% ntl_var_name) %>%
    ggplot(aes(x = cor)) +
    geom_histogram(fill = fill_color,
                   color = "black") +
    labs(x = "Correlation",
         title = title) +
    theme(strip.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          strip.text = element_text(angle=0, size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~unit, 
               scale = "free_y",
               strip.position = "left",
               ncol = 1)
  
}

# Make Figures -----------------------------------------------------------------
can_dmsp_employ <- make_figure("Canada", "DMSP-OLS", "Employment")
can_dmsp_firms  <- make_figure("Canada", "DMSP-OLS", "N Firms")

mex_dmsp_firms  <- make_figure("Mexico", "DMSP-OLS", "N Firms")
mex_viirs_firms <- make_figure("Mexico", "VIIRS",    "N Firms")

# Append / Arrange Figures -----------------------------------------------------
p_within <- ggarrange(can_dmsp_employ,
                      can_dmsp_firms,
                      mex_dmsp_firms,
                      mex_viirs_firms,
                      nrow = 1) 

# FIGURE ARRANGE ===============================================================
p_level_t <- p_level %>% 
  annotate_figure(top = text_grob("(A) Correlation of NTL and Firm Outcomes Levels", 
                                  color = "black", face = "bold", size = 20))

p_accross_t <- p_accross %>% #+ 
  #ggtitle("(B) Correlation of NTL and Firm Outcomes Levels") +
  #theme(plot.title = element_text(hjust = 0, face = "bold", size = 16))
  annotate_figure(top = text_grob("(B) Correlation of Changes in NTL and Firm Outcomes Across Units", 
                                  color = "black", face = "bold", size = 20))

p_within_t <- p_within %>% 
  annotate_figure(top = text_grob("(C) Distribution of correlation of Changes in NTL and Firm Outcomes Within Units", 
                                 color = "black", face = "bold", size = 20))

p_change_all <- ggarrange(p_level_t,
                          p_accross_t,
                          p_within_t,
                          ncol = 1,
                          heights = c(0.3, 0.3, 0.4))
ggsave(p_change_all, filename = file.path(figures_file_path, "cor_fig_all.png"), height = 20, width=15)






