# File to check if we can plot the residuals from each of the models

# Object for residuals distribution: -------------------------------------------
all_sites_lm

library(cowplot)
library(ggridges)

all_sites_lm  %>% 
  # mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
  select(index, augmented) %>% 
  unnest(cols = c(augmented)) %>% 
  select(index, gpp_dt_vut_ref, .fitted, .resid) %>% 
  ggplot(aes(x = .resid, fill = index)) +
  geom_density(alpha = .7) + 
  scale_fill_viridis_d() +
  scale_y_continuous(expand = c(0, 0)) +
  theme_half_open(12)   


all_sites_lm  %>% 
  # mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
  select(index, augmented) %>% 
  unnest(cols = c(augmented)) %>% 
  select(index, gpp_dt_vut_ref, .fitted, .resid) %>%
  rename("residuals" = ".resid") %>% 
  ggplot(aes(x = residuals, y = index, fill = index)) +
  geom_density_ridges() +
  geom_vline(xintercept = 0, colour = "#FF5500", 
             linewidth = 0.7, linetype = "dashed") +
  scale_fill_viridis_d() +
  theme_ridges() + 
  theme(legend.position = "none")

# all_sites_lm %>% 
#   unnest(augmented) %>% glimpse()

all_sites_lm  %>% 
  # mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
  select(index, augmented) %>% 
  unnest(cols = c(augmented)) %>% 
  ggplot(aes(x = .fitted, y = gpp_dt_vut_ref, colour = index)) +
  geom_point(size = 4, alpha = .8) +
  geom_abline(lty = 1,  color = "#E20D6A", linewidth = 2) +
  scale_color_viridis_d() +
  theme_ridges()

# Barplot monthly --------------------------------------------------------------
response_vars <- c("r.squared", "mae", "rmse")

# Create a function to generate the plot
create_plot <- function(y_var, ylim_range) {
  vis_site_glance_montly %>% 
    select(site, index, {{ y_var }}) %>%
    bind_rows(all_sites_glance_monthly,
              all_sites_all_vis_glance_monthly,
              all_vis_glance_monthly) %>% 
    ggplot(aes(x = site, y = .data[[y_var]], fill = index)) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = ylim_range) +
    scale_fill_viridis_d() +
    theme_minimal_hgrid()
}

# Map over the response variables and create the plots
plots <- map2(response_vars, 
              list(c(0.5, 1), c(0.5, 2.5), c(0.8, 3)),
              create_plot)

# Grid the plots as should go in the chapter
monthly_metrics_plot <- plot_grid(plots[[1]], plots[[2]], plots[[3]],
          nrow = 3)

# Weekly
create_plot <- function(y_var, ylim_range) {
  vis_site_glance_weekly %>% 
    select(site, index, {{ y_var }}) %>%
    bind_rows(all_sites_glance_weekly,
              all_sites_all_vis_glance_weekly,
              all_vis_glance_weekly) %>% 
    ggplot(aes(x = site, y = .data[[y_var]], fill = index)) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = ylim_range) +
    scale_fill_viridis_d() +
    theme_minimal_hgrid()
}

# Map over the response variables and create the plots
plots <- map2(response_vars, 
              list(c(0.3, 0.9), c(0.8, 3.2), c(1.4, 3.6)),
              create_plot)

# Grid the plots as should go in the chapter
weekly_metrics_plot <- plot_grid(plots[[1]], plots[[2]], plots[[3]],
          nrow = 3)

# Weekly
create_plot <- function(y_var, ylim_range) {
  vis_site_glance_weekly %>% 
    select(site, index, {{ y_var }}) %>%
    bind_rows(all_sites_glance_daily,
              all_sites_all_vis_glance_daily,
              all_vis_glance_daily) %>% 
    ggplot(aes(x = site, y = .data[[y_var]], fill = index)) +
    geom_col(position = "dodge") +
    coord_cartesian(ylim = ylim_range) +
    scale_fill_viridis_d() +
    theme_minimal_hgrid()
}

# Map over the response variables and create the plots
plots <- map2(response_vars, 
              list(c(0.3, 0.9), c(0.8, 3.2), c(1.4, 3.6)),
              create_plot)

# Grid the plots as should go in the chapter
daily_metrics_plot <- plot_grid(plots[[1]], plots[[2]], plots[[3]],
          nrow = 3)


# Si tengo tiempo, revisar este proceso
# library(tidymodels)
# tidymodels_prefer()
# 
# 
# all_sites <- 
# 
# 
# 
# basic_rec <- 
#   recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
#            Latitude + Longitude, data = ames_train) %>%
#   step_log(Gr_Liv_Area, base = 10) %>% 
#   step_other(Neighborhood, threshold = 0.01) %>% 
#   step_dummy(all_nominal_predictors())
# 
# interaction_rec <- 
#   basic_rec %>% 
#   step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 
# 
# spline_rec <- 
#   interaction_rec %>% 
#   step_ns(Latitude, Longitude, deg_free = 50)
# 
# preproc <- 
#   list(basic = basic_rec, 
#        interact = interaction_rec, 
#        splines = spline_rec
#   )
# 
# lm_models <- workflow_set(preproc, list(lm = linear_reg()), cross = FALSE)
# lm_models
# 

