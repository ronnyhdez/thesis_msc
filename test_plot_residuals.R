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

