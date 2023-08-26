# File to check if we can plot the residuals from each of the models

# Object for residuals distribution: -------------------------------------------
all_sites_lm

library(cowplot)
library(ggridges)

# all_sites_lm  %>% 
#   # mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
#   select(index, augmented) %>% 
#   unnest(cols = c(augmented)) %>% 
#   select(index, gpp_dt_vut_ref, .fitted, .resid) %>% 
#   ggplot(aes(x = .resid, fill = index)) +
#   geom_density(alpha = .7) + 
#   scale_fill_viridis_d() +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme_half_open(12)   

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

all_sites_lm  %>% 
  # mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
  select(index, augmented) %>% 
  unnest(cols = c(augmented)) %>% 
  ggplot(aes(x = .fitted, y = gpp_dt_vut_ref, colour = index)) +
  geom_point(size = 4, alpha = .8) +
  geom_abline(lty = 1,  color = "#E20D6A", linewidth = 2) +
  scale_color_viridis_d() +
  theme_ridges()

evi_lm %>% 
  # mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
  select(site, augmented) %>% 
  unnest(cols = c(augmented)) %>% 
  select(site, gpp_dt_vut_ref, .fitted, .resid) %>%
  rename("residuals" = ".resid") %>% 
  ggplot(aes(x = residuals, y = site, fill = site)) +
  geom_density_ridges() +
  geom_vline(xintercept = 0, colour = "#FF5500", 
             linewidth = 0.7, linetype = "dashed") +
  scale_fill_viridis_d() +
  theme_ridges() + 
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# Probando la union de todos los datos:
# vis_site_augmented_monthly %>% 
#   bind_rows(all_sites_augmented_monthly,
#             all_sites_all_vis_augmented_monthly,
#             all_vis_augmented_monthly) %>% 
#   mutate(index = case_when(
#     index == "evi_mean" ~ "EVI",
#     index == "ndvi_mean" ~ "NDVI",
#     index == "nirv_mean" ~ "NIRv",
#     index == "cci_mean" ~ "CCI",
#     .default = index
#   )) %>% 
#   rename("residuals" = ".resid") %>% 
#   group_by(site, index) %>% 
#   nest() %>% 
#   arrange(site)



  
# Function to create residuals plot
create_residuals_plot <- function(data, site) {
  data %>% 
    filter(site == {{site}}) %>% 
    ggplot(aes(x = residuals, y = index, fill = index)) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, colour = "#FF5500", 
               linewidth = 0.7, linetype = "dashed") +
    scale_fill_viridis_d() +
    # scale_x_continuous(limits = c(-30, 20), n.breaks = 10) +
    theme_ridges() + 
    theme(legend.position = "none")
}

# Vector with sites for looping in the plots
sites <- c("Bartlett", "Borden", "Michigan", "All")

# Monthly
monthly_residuals_data <- vis_site_augmented_monthly %>% 
  bind_rows(all_sites_augmented_monthly,
            all_sites_all_vis_augmented_monthly,
            all_vis_augmented_monthly) %>% 
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  )) %>% 
  rename("residuals" = ".resid") 
  
# Loop over all the sites
monthly_residuals_plots <- 
  map(sites, ~ create_residuals_plot(monthly_residuals_data, .x))

# Weekly
weekly_residuals_data <- vis_site_augmented_weekly %>% 
  bind_rows(all_sites_augmented_weekly,
            all_sites_all_vis_augmented_weekly,
            all_vis_augmented_weekly) %>% 
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  )) %>% 
  rename("residuals" = ".resid") 

# Loop over all the sites
weekly_residuals_plots <- 
  map(sites, ~ create_residuals_plot(weekly_residuals_data, .x))

# Daily
daily_residuals_data <- vis_site_augmented_daily %>% 
  bind_rows(all_sites_augmented_daily,
            all_sites_all_vis_augmented_daily,
            all_vis_augmented_daily) %>% 
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  )) %>% 
  rename("residuals" = ".resid") 

# Loop over all the sites
daily_residuals_plots <- 
  map(sites, ~ create_residuals_plot(daily_residuals_data, .x))

plot_grid(monthly_residuals_plots[[1]] + labs(title = "monthly 1"),
          monthly_residuals_plots[[2]],
          monthly_residuals_plots[[3]],
          monthly_residuals_plots[[4]] + labs(title = "monthly 4"),
          weekly_residuals_plots[[1]],
          weekly_residuals_plots[[2]],
          weekly_residuals_plots[[3]],
          weekly_residuals_plots[[4]],
          daily_residuals_plots[[1]],
          daily_residuals_plots[[2]],
          daily_residuals_plots[[3]],
          daily_residuals_plots[[4]],
          nrow = 3, 
          ncol = 4)







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

