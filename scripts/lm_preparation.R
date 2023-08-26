# File to prepare the lm models
#
# Objective of this file is to have big portions of the code here and call the 
# created objects in the chapter file, without clumping the chapter file with
# many code lines, and leave it mainly for text.
#
# There is a source call to a function file.

# Libraries
library(dplyr)
library(broom)
library(tidymodels)
library(purrr)

source("scripts/models_data_preparation.R")

# MONTHLY LMS ------------------------------------------------------------------
# Prepare data with all sites
ind_bar <- bartlett_monthly_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean,
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Bartlett")

ind_bor <- borden_monthly_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Borden")

ind_mich <- michigan_monthly_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Michigan")

ind_sites <- bind_rows(ind_bar, ind_bor, ind_mich)

# Function to select variables for residuals plots
select_augmented <- function(data, variable) {
  data %>% 
    select(variable, augmented) %>% 
    unnest(cols = c(augmented)) %>% 
    select(variable, gpp_dt_vut_ref, .resid)
}

# Linear model for all sites [A | Diff VIs]
all_sites_lm <- ind_sites %>% 
  select(-kndvi_mean) %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index", values_to = "value") %>% 
  nest(data = c(-index)) %>% 
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ value, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

all_sites_augmented_monthly <- select_augmented(all_sites_lm, "index") %>% 
  mutate(site = "All")

all_fit <- all_sites_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(site = "All")

all_sites_glance_monthly <- all_sites_lm  %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>% 
  select(-data, -fit, -tidied) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(site = "All") %>% 
  select(site, index, r.squared, mae, rmse) %>% 
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    # index == "kndvi_mean" ~ "kNDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  ))

# Linear model for all sites EVI [B | Diff VIS + site]
evi_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ evi_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  ) 

evi_augmented_monthly <- select_augmented(evi_lm, "site") %>% 
  mutate(index = "EVI")

evi_fit <- evi_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "EVI")

evi_glance_monthly <- evi_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "EVI")

# Linear model for all sites NDVI [B | Diff VIS + site]
ndvi_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ ndvi_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

ndvi_augmented_monthly <- select_augmented(ndvi_lm, "site") %>% 
  mutate(index = "NDVI")

ndvi_fit <- ndvi_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "NDVI")

ndvi_glance_monthly <- ndvi_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "NDVI")

# Linear model for all sites NIRv [B | Diff VIS + site]
nirv_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ nirv_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

nirv_augmented_monthly <- select_augmented(nirv_lm, "site") %>% 
  mutate(index = "NIRv")

nirv_fit <- nirv_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "NIRv")

nirv_glance_monthly <- nirv_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "NIRv") 

# Linear model for all sites CCI [B | Diff VIS + site]
cci_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ cci_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

cci_augmented_monthly <- select_augmented(cci_lm, "site") %>% 
  mutate(index = "CCI")

cci_fit <- cci_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "CCI")

cci_glance_monthly <- cci_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "CCI")

vis_site_glance_monthly <- bind_rows(evi_glance_monthly,
                                    ndvi_glance_monthly,
                                    nirv_glance_monthly,
                                    cci_glance_monthly) 

vis_site_augmented_monthly <- bind_rows(evi_augmented_monthly,
                                        ndvi_augmented_monthly,
                                        nirv_augmented_monthly,
                                        cci_augmented_monthly) 

# # Linear model for all sites kNDVI
# kndvi_lm <- ind_sites %>%
#   nest(data = c(-site)) %>%
#   mutate(
#     fit = map(data, ~ lm(gpp_dt_vut_ref ~ kndvi_mean, data = .x)),
#     tidied = map(fit, tidy),
#     glanced = map(fit, glance),
#     augmented = map(fit, augment)
#   )
# 
# kndvi_fit <- kndvi_lm %>% 
#   unnest(tidied) %>%
#   select(-data, -fit, -glanced) %>% 
#   mutate(index = "kNDVI")
# 
# kndvi_glance <- kndvi_lm %>% 
#   mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2)))) %>% 
#   unnest(glanced) %>%
#   select(-data, -fit, -tidied) %>% 
#   arrange(desc(r.squared)) %>% 
#   mutate(index = "kNDVI")


# Linear model for all VI's [C | All VIs]
all_vis_lm <- ind_sites %>%
  select(-kndvi_mean) %>%
  # pivot_longer(cols = c(ends_with("mean")), names_to = "index", values_to = "value") %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ evi_mean +
                           ndvi_mean + nirv_mean +
                           cci_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

all_vis_augmented_monthly <- select_augmented(all_vis_lm, "site") %>% 
  mutate(index = "All")

all_fit <- all_vis_lm %>%
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>%
  mutate(site = "All")

all_vis_glance_monthly <- all_vis_lm  %>%
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied) %>%
  arrange(desc(r.squared)) %>%
  mutate(index = "All") %>%
  select(site, index, r.squared, mae, rmse) %>%
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    # index == "kndvi_mean" ~ "kNDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  ))

# Linear model for all sites and all indices (covariates) [D | All VIs + site]
all_sites_all_indices <- lm(gpp_dt_vut_ref ~ evi_mean +
                              ndvi_mean + nirv_mean +
                              cci_mean, data = ind_sites)

# summary(all_sites_all_indices)
metrics <- augment(all_sites_all_indices) %>% 
  select(gpp_dt_vut_ref, .resid) 

all_sites_all_vis_augmented_monthly <- metrics %>% 
  mutate(index = "All", site = "All")

rmse <- sqrt(mean((metrics$.resid)^2))
mae <- mean(abs(metrics$.resid))

all_sites_all_vis_glance_monthly <- glance(all_sites_all_indices) %>% 
  mutate(rmse = rmse,
         mae = mae,
         site = "All",
         index = "All") %>% 
  select(site, index, r.squared, mae, rmse)

# WEEKLY LMS ------------------------------------------------------------------
# Prepare data with all sites
ind_bar <- bartlett_weekly_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean,
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Bartlett")

ind_bor <- borden_weekly_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Borden")

ind_mich <- michigan_weekly_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Michigan")

ind_sites <- bind_rows(ind_bar, ind_bor, ind_mich)

# Linear model for all sites [A | Diff VIs]

all_sites_lm <- ind_sites %>% 
  select(-kndvi_mean) %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index", values_to = "value") %>% 
  nest(data = c(-index)) %>% 
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ value, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

all_sites_augmented_weekly <- select_augmented(all_sites_lm, "index") %>% 
  mutate(site = "All")

all_fit <- all_sites_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(site = "All")

all_sites_glance_weekly <- all_sites_lm  %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>% 
  select(-data, -fit, -tidied) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(site = "All") %>% 
  select(site, index, r.squared, mae, rmse) %>% 
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    # index == "kndvi_mean" ~ "kNDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  ))

# Linear model for all sites EVI [B | Diff VIS + site]
evi_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ evi_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  ) 

evi_augmented_weekly <- select_augmented(evi_lm, "site") %>% 
  mutate(index = "EVI")

evi_fit <- evi_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "EVI")

evi_glance_weekly <- evi_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "EVI")

# Linear model for all sites NDVI [B | Diff VIS + site]
ndvi_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ ndvi_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

ndvi_augmented_weekly <- select_augmented(ndvi_lm, "site") %>% 
  mutate(index = "NDVI")

ndvi_fit <- ndvi_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "NDVI")

ndvi_glance_weekly <- ndvi_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "NDVI")

# Linear model for all sites NIRv [B | Diff VIS + site]
nirv_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ nirv_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

nirv_augmented_weekly <- select_augmented(nirv_lm, "site") %>% 
  mutate(index = "NIRv")

nirv_fit <- nirv_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "NIRv")

nirv_glance_weekly <- nirv_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "NIRv") 

# Linear model for all sites CCI [B | Diff VIS + site]
cci_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ cci_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

cci_augmented_weekly <- select_augmented(cci_lm, "site") %>% 
  mutate(index = "CCI")

cci_fit <- cci_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "CCI")

cci_glance_weekly <- cci_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "CCI")

vis_site_glance_weekly <- bind_rows(evi_glance_weekly,
                                    ndvi_glance_weekly,
                                    nirv_glance_weekly,
                                    cci_glance_weekly) 

vis_site_augmented_weekly <- bind_rows(evi_augmented_weekly,
                                        ndvi_augmented_weekly,
                                        nirv_augmented_weekly,
                                        cci_augmented_weekly) 

# Linear model for all VI's [C | All VIs]
all_vis_lm <- ind_sites %>%
  select(-kndvi_mean) %>%
  # pivot_longer(cols = c(ends_with("mean")), names_to = "index", values_to = "value") %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ evi_mean +
                           ndvi_mean + nirv_mean +
                           cci_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

all_vis_augmented_weekly <- select_augmented(all_vis_lm, "site") %>% 
  mutate(index = "All")

all_fit <- all_vis_lm %>%
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>%
  mutate(site = "All")

all_vis_glance_weekly <- all_vis_lm  %>%
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied) %>%
  arrange(desc(r.squared)) %>%
  mutate(index = "All") %>%
  select(site, index, r.squared, mae, rmse) %>%
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    # index == "kndvi_mean" ~ "kNDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  ))

# Linear model for all sites and all indices (covariates) [D | All VIs + site]
all_sites_all_indices <- lm(gpp_dt_vut_ref ~ evi_mean +
                              ndvi_mean + nirv_mean +
                              cci_mean, data = ind_sites)

# summary(all_sites_all_indices)
metrics <- augment(all_sites_all_indices) %>% 
  select(gpp_dt_vut_ref, .resid)

all_sites_all_vis_augmented_weekly <- metrics %>% 
  mutate(index = "All", site = "All")

rmse <- sqrt(mean((metrics$.resid)^2))
mae <-  mean(abs(metrics$.resid))

all_sites_all_vis_glance_weekly <- glance(all_sites_all_indices) %>% 
  mutate(rmse = rmse,
         mae = mae,
         site = "All",
         index = "All") %>% 
  select(site, index, r.squared, mae, rmse)


# DAILY LMS ------------------------------------------------------------------
# Prepare data with all sites
ind_bar <- bartlett_daily_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean,
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Bartlett")

ind_bor <- borden_daily_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Borden")

ind_mich <- michigan_daily_500 %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, kndvi_mean, cci_mean) %>% 
  mutate(site = "Michigan")

ind_sites <- bind_rows(ind_bar, ind_bor, ind_mich)

# Linear model for all sites [A | Diff VIs]
all_sites_lm <- ind_sites %>% 
  select(-kndvi_mean) %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index", values_to = "value") %>% 
  nest(data = c(-index)) %>% 
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ value, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

all_fit <- all_sites_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(site = "All")

all_sites_augmented_daily <- select_augmented(all_sites_lm, "index") %>% 
  mutate(site = "All")

all_sites_glance_daily <- all_sites_lm  %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>% 
  select(-data, -fit, -tidied) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(site = "All") %>% 
  select(site, index, r.squared, mae, rmse) %>% 
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    # index == "kndvi_mean" ~ "kNDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  ))

# Linear model for all sites EVI [B | Diff VIS + site]
evi_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ evi_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  ) 

evi_augmented_daily <- select_augmented(evi_lm, "site") %>% 
  mutate(index = "EVI")

evi_fit <- evi_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "EVI")

evi_glance_daily <- evi_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "EVI")

# Linear model for all sites NDVI [B | Diff VIS + site]
ndvi_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ ndvi_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

ndvi_augmented_daily <- select_augmented(ndvi_lm, "site") %>% 
  mutate(index = "NDVI")

ndvi_fit <- ndvi_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "NDVI")

ndvi_glance_daily <- ndvi_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "NDVI")

# Linear model for all sites NIRv [B | Diff VIS + site]
nirv_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ nirv_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

nirv_augmented_daily <- select_augmented(nirv_lm, "site") %>% 
  mutate(index = "NIRv")

nirv_fit <- nirv_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "NIRv")

nirv_glance_daily <- nirv_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied, -augmented) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "NIRv") 

# Linear model for all sites CCI [B | Diff VIS + site]
cci_lm <- ind_sites %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ cci_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

cci_augmented_daily <- select_augmented(cci_lm, "site") %>% 
  mutate(index = "CCI")

cci_fit <- cci_lm %>% 
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>% 
  mutate(index = "CCI")

cci_glance_daily <- cci_lm %>% 
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>%  
  unnest(glanced) %>%
  select(-data, -fit, -tidied) %>% 
  arrange(desc(r.squared)) %>% 
  mutate(index = "CCI")

vis_site_glance_daily <- bind_rows(evi_glance_daily,
                                   ndvi_glance_daily,
                                   nirv_glance_daily,
                                   cci_glance_daily) 

vis_site_augmented_daily <- bind_rows(evi_augmented_daily,
                                        ndvi_augmented_daily,
                                        nirv_augmented_daily,
                                        cci_augmented_daily) 

# Linear model for all VI's [C | All VIs]
all_vis_lm <- ind_sites %>%
  select(-kndvi_mean) %>%
  # pivot_longer(cols = c(ends_with("mean")), names_to = "index", values_to = "value") %>%
  nest(data = c(-site)) %>%
  mutate(
    fit = map(data, ~ lm(gpp_dt_vut_ref ~ evi_mean +
                           ndvi_mean + nirv_mean +
                           cci_mean, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

all_vis_augmented_daily <- select_augmented(all_vis_lm, "site") %>% 
  mutate(index = "All")

all_fit <- all_vis_lm %>%
  unnest(tidied) %>%
  select(-data, -fit, -glanced) %>%
  mutate(site = "All")

all_vis_glance_daily <- all_vis_lm  %>%
  mutate(rmse = map_dbl(augmented, ~sqrt(mean((.x$.resid)^2))),
         mae = map_dbl(augmented, ~mean(abs(.x$.resid)))) %>% 
  unnest(glanced) %>%
  select(-data, -fit, -tidied) %>%
  arrange(desc(r.squared)) %>%
  mutate(index = "All") %>%
  select(site, index, r.squared, mae, rmse) %>%
  mutate(index = case_when(
    index == "evi_mean" ~ "EVI",
    index == "ndvi_mean" ~ "NDVI",
    # index == "kndvi_mean" ~ "kNDVI",
    index == "nirv_mean" ~ "NIRv",
    index == "cci_mean" ~ "CCI",
    .default = index
  ))

# Linear model for all sites and all indices (covariates) [D | All VIs + site]
all_sites_all_indices <- lm(gpp_dt_vut_ref ~ evi_mean +
                              ndvi_mean + nirv_mean +
                              cci_mean, data = ind_sites)

# summary(all_sites_all_indices)
metrics <- augment(all_sites_all_indices) %>% 
  select(gpp_dt_vut_ref, .resid)

all_sites_all_vis_augmented_daily <- metrics %>% 
  mutate(index = "All", site = "All")

rmse <- sqrt(mean((metrics$.resid)^2))
mae <-  mean(abs(metrics$.resid))

all_sites_all_vis_glance_daily <- glance(all_sites_all_indices) %>% 
  mutate(rmse = rmse,
         mae = mae,
         site = "All",
         index = "All") %>% 
  select(site, index, r.squared, mae, rmse)

