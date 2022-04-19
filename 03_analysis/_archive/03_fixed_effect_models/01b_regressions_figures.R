# Analysis

# Load Data --------------------------------------------------------------------
df_fe <- readRDS(file.path(data_file_path, "Results", "regression_results_yrid.Rds"))
df_longdiff <- readRDS(file.path(data_file_path, "Results", "regression_results_longdiff.Rds"))

df <- bind_rows(df_fe,
                df_longdiff)

df$FE[df$long_diff %in% T] <- "long-diff"

df <- df %>%
  filter(FE %in% c("id", "yr", "long-diff"),
         ntl_var %in% c("viirs_sum_log", "dmspolsharmon_sum_log"),
         dv_type %in% "ntl", # industry, ntl
         var != "(Intercept)",
         splag %in% F) %>%
  mutate(unit = unit %>% factor(levels = c("City", "Grid in Cities", "5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")),
         industry_var = case_when(industry_var == "employment_sum_all_log" ~ "Employment",
                                  industry_var == "N_firms_sum_all_log"    ~ "N Firms"),
         ntl_var = case_when(ntl_var == "viirs_sum_log" ~ "VIIRS",
                             ntl_var == "dmspolsharmon_sum_log" ~ "DMSP-OLS"),
         country = country %>% tools::toTitleCase(),
         title = paste0(country, "\n", ntl_var, " & ", industry_var))

## Remove Within Unit Different for Canada, VIIRS
df <- df[!(df$country %in% "Canada" & df$ntl_var %in% "VIIRS" & df$FE %in% "id"),]

## Prep Titles
subset_tf <- df$long_diff %in% T & df$country %in% "Canada" & df$ntl_var %in% "DMSP-OLS"
df$title[subset_tf] <- paste0(df$title[subset_tf], "\n[12 Years]")

subset_tf <- df$long_diff %in% T & df$country %in% "Canada" & df$ntl_var %in% "VIIRS"
df$title[subset_tf] <- paste0(df$title[subset_tf], "\n[2 Years]")

subset_tf <- df$long_diff %in% T & df$country %in% "Mexico" & df$ntl_var %in% "DMSP-OLS"
df$title[subset_tf] <- paste0(df$title[subset_tf], "\n[10 Years]")

subset_tf <- df$long_diff %in% T & df$country %in% "Mexico" & df$ntl_var %in% "VIIRS"
df$title[subset_tf] <- paste0(df$title[subset_tf], "\n[6 Years]")

#df <- df[grepl("km", df$unit),]

# Figure Function --------------------------------------------------------------
make_figure <- function(df, FE_i, title_main){
  
  df %>%
    filter(FE %in% FE_i) %>%
    ggplot(aes(y = coef,
               ymin = ci2_5,
               ymax = ci97_5,
               x = unit)) +
    # color = var
    geom_hline(yintercept=0,size=.2,color="red") +
    geom_point(position = position_dodge(width = 1)) +
    geom_linerange(position = position_dodge(width = 1)) +
    labs(x = NULL,
         y = "Coefficient (+/- 95% CI)",
         title = title_main) +
    coord_flip() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11)) +
    facet_wrap(~title,
               scales = "free_x",
               nrow = 1) +
    theme(strip.background = element_blank(),
          strip.text = element_text(face = "bold"))
}

# Make Figure ------------------------------------------------------------------
p_yr <- make_figure(df, "yr", "Panel, Year Fixed Effects - Association Across Levels")
p_id <- make_figure(df, "id", "Panel, Unit Fixed Effects - Association Across Time")
p_ld <- make_figure(df, "long-diff", "Long Difference")

p <- ggarrange(p_yr,
               ggarrange(NA, p_id, NA, nrow = 1, widths = c(0.15, 0.7, 0.15)),
               p_ld,
               ncol = 1)

ggsave(p, filename = file.path(figures_file_path, "fe_elasticity_fig.png"), 
       height = 9, width=12)
