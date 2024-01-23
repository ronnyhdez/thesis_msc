# File to prepare the RF models
#
# Objective of this file is to have big portions of the code here and call the 
# created objects in the chapter file, without clumping the chapter file with
# many code lines, and leave it mainly for text.
#
# There is a source call to a function file.

# Libraries
library(ggplot2)
library(cowplot)
library(lubridate)
library(purrr)
library(broom)
library(gt)
library(tidymodels)
library(broom)
library(usemodels)
library(vip)
library(h2o)
library(stringr)
library(DALEX)
library(DALEXtra)
library(forcats)

# Source files
# Source the objects created for the complete GPP trends.
# This file will source the code and load objects to memory.
source("scripts/trend_plots.R")

# Source the objects created for the complete GPP trends
source("scripts/models_data_preparation.R")

# Source file with functions to plot rf predictions
source("R/plot_exploratory.R")

# ```{r data_preparation_rf}
#| echo: false
#| message: false
#| warning: false

# 500
# Dataset to use: daily_500 for all sites
bor <- borden_daily_500 %>% 
  select(ends_with(c("_mean")),
         gpp_dt_vut_ref, total_obs) %>% 
  mutate(site = "borden")

bar <- bartlett_daily_500 %>% 
  select(ends_with(c("_mean")),
         gpp_dt_vut_ref, total_obs) %>% 
  mutate(site = "bartlett")

mich <- michigan_daily_500 %>% 
  select(ends_with(c("_mean")),
         gpp_dt_vut_ref, total_obs) %>% 
  mutate(site = "michigan")

daily_500_rf <- bind_rows(bor, bar, mich) %>% 
  select(-kndvi_mean)

# Dataset to use: weekly_500 for all sites
bor <- borden_weekly_500 %>% 
  select(ends_with(c("_mean")),
         gpp_dt_vut_ref, total_obs) %>% 
  mutate(site = "borden")

variables <- names(bor)

bar <- bartlett_weekly_500 %>% 
  mutate(site = "bartlett") %>% 
  select(all_of(variables))

mich <- michigan_weekly_500 %>% 
  mutate(site = "michigan") %>% 
  select(all_of(variables))

weekly_500_rf <- bind_rows(bor, bar, mich) %>% 
  select(-kndvi_mean)

## Dataset to use: monthly_500 for all sites
bor <- borden_monthly_500 %>% 
  select(ends_with(c("_mean")),
         gpp_dt_vut_ref, total_obs) %>% 
  mutate(site = "borden")

variables <- names(bor)

bar <- bartlett_monthly_500 %>% 
  mutate(site = "bartlett") %>% 
  select(all_of(variables))

mich <- michigan_monthly_500 %>% 
  mutate(site = "michigan") %>% 
  select(all_of(variables))

monthly_500_rf <- bind_rows(bor, bar, mich) %>% 
  select(-kndvi_mean)
# ```


# <!-- #### Daily 500 -->
  
  # ```{r xgboost_daily}
#| echo: false
#| message: false
#| warning: false
# set.seed(123)
# daily_500_split <- initial_split(daily_500_rf, strata = site)
# daily_500_train <- training(daily_500_split)
# daily_500_test <- testing(daily_500_split)
# 
# set.seed(234)
# # daily_500_folds 
# daily_500_folds <- bootstraps(daily_500_train,
#                               times = 100,
#                               strata = gpp_dt_vut_ref)
# 
# bb_recipe <- 
#   recipe(formula = gpp_dt_vut_ref ~ ., data = daily_500_train) %>% 
#   step_select(-site, -total_obs)
# 
# xgb_spec <-
#   boost_tree(
#     trees = tune(),
#     min_n = tune(),
#     mtry = tune(),
#     learn_rate = 0.01
#   ) %>%
#   set_engine("xgboost") %>%
#   set_mode("regression")
# 
# xgb_wf <- workflow(bb_recipe, xgb_spec)
# 
# set.seed(3156)
# 
# library(finetune)
# doParallel::registerDoParallel()
# 
# set.seed(345)
# xgb_rs <- tune_race_anova(
#   xgb_wf,
#   resamples = daily_500_folds,
#   grid = 15,
#   # metrics = metric_set(mn_log_loss),
#   control = control_race(verbose_elim = TRUE)
# )
# 
# plot_race(xgb_rs)
# 
# show_best(xgb_rs)
# 
# xgb_last <- xgb_wf %>%
#   finalize_workflow(select_best(xgb_rs)) %>%
#   last_fit(daily_500_split)
# 
# xgb_last
# metrics <- collect_metrics(xgb_last)
# 
# plot_predictions_rf(xgb_last, metrics, 4, 4, 23.5, 22) 
# ```

# ```{r daily_500_rf}
#| echo: false
#| message: false
#| warning: false

## Make sure that the source of the file "R/plot_exploratory.R" was succesful

set.seed(752)
daily_500_split <- initial_split(daily_500_rf, strata = site)
daily_500_train <- training(daily_500_split)
daily_500_test <- testing(daily_500_split)

set.seed(234)
# daily_500_folds 
daily_500_folds <- bootstraps(daily_500_train,
                              times = 100,
                              strata = gpp_dt_vut_ref)

ranger_recipe <- 
  recipe(formula = gpp_dt_vut_ref ~ ., data = daily_500_train) %>% 
  step_select(-site, -total_obs, skip = TRUE)

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

# Conditional to re-run model if no artifac was saved before.
if (fs::file_exists("models/daily_500_fit_site.rds") & 
    fs::file_exists("models/daily_500_site_ranger_tune.rds")) {
  
  daily_500_fit <- readRDS("models/daily_500_fit_site.rds")
  ranger_tune <- readRDS("models/daily_500_site_ranger_tune.rds")
  
} else {  
  doParallel::registerDoParallel()
  set.seed(6578)
  ranger_tune <-
    tune_grid(ranger_workflow, 
              resamples = daily_500_folds, 
              grid = 12)
  
  # Final fit
  final_rf <- ranger_workflow %>% 
    finalize_workflow(select_best(ranger_tune))
  
  daily_500_fit <- last_fit(final_rf, daily_500_split)
  
  ## last_fit is saved if no model has been trained and saved.
  #saveRDS(ranger_tune, "models/daily_500_site_ranger_tune.rds")
  #saveRDS(daily_500_fit, "models/daily_500_fit_site.rds")
}
# ```

# ```{r predictions_plot_daily_500_rf}
#| label: fig-daily_500_rf
#| fig-cap: GPP observed and predicted values from the Random Forest model for all the sites at a daily basis. The red line represents a 1:1 relation. Metrics units are gC m⁻² d⁻¹
#| fig-width: 6
#| fig-height: 4
#| echo: false
#| message: false
#| warning: false

# Explore RF results
## Check the metrics
daily_metrics <- collect_metrics(daily_500_fit) 

## Collect predictions
rf_pred_daily <- plot_predictions_rf(daily_500_fit, daily_metrics, 4, 4, 23.5, 22) 
# ```

# ```{r vip_plot_daily_500_rf}
#| label: fig-vip_daily_500_rf
#| fig-cap: "Variable of importance derived from the Random forest model for the daily values at 500 m spatial resolution model."
#| echo: false
#| message: false
#| warning: false

## Feature importance
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune)) %>%
  set_engine("ranger", importance = "permutation")

rf_vip_daily <- workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(imp_spec) %>%
  fit(daily_500_train) %>%
  extract_fit_parsnip() %>% 
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue")) +
  theme_light(base_size = 12) +
  scale_x_discrete(labels = c("ndvi_mean" = "NDVI",
                              "nirv_mean" = "NIRv",
                              "evi_mean" = "EVI",
                              "cci_mean" = "CCI",
                              "sur_refl_b01_mean" = "B01",
                              "sur_refl_b02_mean" = "B02",
                              "sur_refl_b03_mean" = "B03",
                              "sur_refl_b04_mean" = "B04",
                              "sur_refl_b05_mean" = "B05",
                              "sur_refl_b06_mean" = "B06",
                              "sur_refl_b07_mean" = "B07"))
# ```

# ```{r shapley_values_daily}
#| label: fig-shap_daily_500_rf
#| fig-cap: "Shapley values derived from the Random forest model for the daily values at 500 m spatial resolution model. Predicted value for the low GPP value is 0.59 (A) and 12.7 for the selected high GPP value (B)."
#| echo: false
#| message: false
#| warning: false

# Extract workflow to obtain shapley values (or run predict)
daily_gpp_model <- extract_workflow(daily_500_fit)

# Create an explainer for a regression model
explainer_rf <- explain_tidymodels(
  daily_gpp_model,
  data = daily_500_train,
  y = daily_500_train$gpp_dt_vut_ref,
  label = "rf",
  verbose = FALSE
)

# Take a low gpp value
low_gpp <- daily_500_train[71, ] 
low_gpp_pred <- predict(daily_gpp_model, low_gpp)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = low_gpp,
                              type = "shap")
# rf_breakdown
low_value <- rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>% 
  mutate(variable = case_when(
    str_detect(variable, "ndvi_mean") ~ str_replace(variable, "ndvi_mean", "NDVI"),
    str_detect(variable, "nirv_mean") ~ str_replace(variable, "nirv_mean", "NIRv"),
    str_detect(variable, "evi_mean" ) ~ str_replace(variable, "evi_mean", "EVI" ),
    str_detect(variable, "cci_mean" ) ~ str_replace(variable, "cci_mean", "CCI" ),
    str_detect(variable, "sur_refl_b01_mean") ~ str_replace(variable, "sur_refl_b01_mean", "B01"),
    str_detect(variable, "sur_refl_b02_mean") ~ str_replace(variable, "sur_refl_b02_mean", "B02"),
    str_detect(variable, "sur_refl_b03_mean") ~ str_replace(variable, "sur_refl_b03_mean", "B03"),
    str_detect(variable, "sur_refl_b04_mean") ~ str_replace(variable, "sur_refl_b04_mean", "B04"),
    str_detect(variable, "sur_refl_b05_mean") ~ str_replace(variable, "sur_refl_b05_mean", "B05"),
    str_detect(variable, "sur_refl_b06_mean") ~ str_replace(variable, "sur_refl_b06_mean", "B06"),
    str_detect(variable, "sur_refl_b07_mean") ~ str_replace(variable, "sur_refl_b07_mean", "B07"),
    str_detect(variable, "total_obs") ~ str_replace(variable, "total_obs", "Total obs."),
    str_detect(variable, "site") ~ str_replace(variable, "site", "Site"),
    str_detect(variable, "gpp_dt_vut_ref") ~ str_replace(variable, "gpp_dt_vut_ref", "GPP"),
    .default = variable
  )) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

# Take a high gpp value
high_gpp <- daily_500_train[578, ] 
high_gpp_pred <- predict(daily_gpp_model, high_gpp)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = high_gpp,
                              type = "shap")
# rf_breakdown
high_value <- rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = case_when(
    str_detect(variable, "ndvi_mean") ~ str_replace(variable, "ndvi_mean", "NDVI"),
    str_detect(variable, "nirv_mean") ~ str_replace(variable, "nirv_mean", "NIRv"),
    str_detect(variable, "evi_mean" ) ~ str_replace(variable, "evi_mean", "EVI" ),
    str_detect(variable, "cci_mean" ) ~ str_replace(variable, "cci_mean", "CCI" ),
    str_detect(variable, "sur_refl_b01_mean") ~ str_replace(variable, "sur_refl_b01_mean", "B01"),
    str_detect(variable, "sur_refl_b02_mean") ~ str_replace(variable, "sur_refl_b02_mean", "B02"),
    str_detect(variable, "sur_refl_b03_mean") ~ str_replace(variable, "sur_refl_b03_mean", "B03"),
    str_detect(variable, "sur_refl_b04_mean") ~ str_replace(variable, "sur_refl_b04_mean", "B04"),
    str_detect(variable, "sur_refl_b05_mean") ~ str_replace(variable, "sur_refl_b05_mean", "B05"),
    str_detect(variable, "sur_refl_b06_mean") ~ str_replace(variable, "sur_refl_b06_mean", "B06"),
    str_detect(variable, "sur_refl_b07_mean") ~ str_replace(variable, "sur_refl_b07_mean", "B07"),
    str_detect(variable, "total_obs") ~ str_replace(variable, "total_obs", "Total obs."),
    str_detect(variable, "site") ~ str_replace(variable, "site", "Site"),
    str_detect(variable, "gpp_dt_vut_ref") ~ str_replace(variable, "gpp_dt_vut_ref", "GPP"),
    .default = variable
  )) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

rf_shap_daily <- plot_grid(low_value,
          high_value,
          nrow = 1,
          labels = c('A', 'B'),
          vjust = 1)
# ```

# <!-- #### Weekly 500 -->
  
  # ```{r weekly_500_rf}
#| echo: false
#| message: false
#| warning: false
set.seed(125)

weekly_500_split <- initial_split(weekly_500_rf, strata = site)
weekly_500_train <- training(weekly_500_split)
weekly_500_test <- testing(weekly_500_split)

set.seed(2389)
weekly_500_folds <- bootstraps(weekly_500_train,
                               times = 100,
                               strata = gpp_dt_vut_ref)
ranger_recipe <- 
  recipe(formula = gpp_dt_vut_ref ~ ., data = weekly_500_train) %>% 
  step_select(-site, -total_obs, skip = TRUE)

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

# Conditional to re-run model if no artifac was saved before.
if (fs::file_exists("models/weekly_500_fit_site.rds")) {
  weekly_500_fit <- readRDS("models/weekly_500_fit_site.rds")
  ranger_tune <- readRDS("models/weekly_500_site_ranger_tune.rds")
} else {
  doParallel::registerDoParallel()
  set.seed(1297)
  ranger_tune <-
    tune_grid(ranger_workflow, 
              resamples = weekly_500_folds, 
              grid = 12)
  
  # Final fit
  final_rf <- ranger_workflow %>% 
    finalize_workflow(select_best(ranger_tune))
  
  weekly_500_fit <- last_fit(final_rf, weekly_500_split)
  
  ## last_fit is saved if no model has been trained and saved.
  #saveRDS(ranger_tune, "models/weekly_500_site_ranger_tune.rds")
  #saveRDS(weekly_500_fit, "models/weekly_500_fit_site.rds")
}
# ```

# ```{r predictions_plot_weekly_500_rf}
#| label: fig-weekly_500_rf
#| fig-cap: "GPP observed and predicted values from the Random Forest for all the sites at a weekly basis. The red line represents a 1:1 relation. Metrics units are gC m⁻² d⁻¹"
#| fig-width: 6
#| fig-height: 4
#| echo: false
#| message: false
#| warning: false

# Explore RF results
## Check the metrics
weekly_metrics <- collect_metrics(weekly_500_fit) 

## Collect predictions
rf_pred_weekly <- plot_predictions_rf(weekly_500_fit, weekly_metrics, 5, 5, 18, 19)
# ```

# ```{r vip_plot_weekly_500_rf}
#| label: fig-vip_weekly_500_rf
#| fig-cap: "Variable of importance derived from the Random forest model for the weekly values at 500 m spatial resolution model."
#| echo: false
#| message: false
#| warning: false

## Feature importance
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune)) %>%
  set_engine("ranger", importance = "permutation")

rf_vip_weekly <- workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(imp_spec) %>%
  fit(weekly_500_train) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue")) +
  theme_classic(base_size = 12) +
  scale_x_discrete(labels = c("ndvi_mean" = "NDVI",
                              "nirv_mean" = "NIRv",
                              "evi_mean" = "EVI",
                              "cci_mean" = "CCI",
                              "sur_refl_b01_mean" = "B01",
                              "sur_refl_b02_mean" = "B02",
                              "sur_refl_b03_mean" = "B03",
                              "sur_refl_b04_mean" = "B04",
                              "sur_refl_b05_mean" = "B05",
                              "sur_refl_b06_mean" = "B06",
                              "sur_refl_b07_mean" = "B07"))
# ```

# ```{r shapley_values_weekly}
#| label: fig-shap_weekly_500_rf
#| fig-cap: "Shapley values derived from the Random forest model for the weekly values at 500 m spatial resolution model. Predicted value for the low GPP value is 1.59 (A) and 11.0 for the selected high GPP value (B)."
#| echo: false
#| message: false
#| warning: false

# Extract workflow to obtain shapley values (or run predict)
weekly_gpp_model <- extract_workflow(weekly_500_fit)

# Create an explainer for a regression model
explainer_rf <- explain_tidymodels(
  weekly_gpp_model,
  data = weekly_500_train,
  y = weekly_500_train$gpp_dt_vut_ref,
  label = "rf",
  verbose = FALSE
)

# Take a low gpp value
low_gpp <- weekly_500_train[16, ] 
low_gpp_pred <- predict(weekly_gpp_model, low_gpp)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = low_gpp,
                              type = "shap")
# rf_breakdown
low_value <- rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = case_when(
    str_detect(variable, "ndvi_mean") ~ str_replace(variable, "ndvi_mean", "NDVI"),
    str_detect(variable, "nirv_mean") ~ str_replace(variable, "nirv_mean", "NIRv"),
    str_detect(variable, "evi_mean" ) ~ str_replace(variable, "evi_mean", "EVI" ),
    str_detect(variable, "cci_mean" ) ~ str_replace(variable, "cci_mean", "CCI" ),
    str_detect(variable, "sur_refl_b01_mean") ~ str_replace(variable, "sur_refl_b01_mean", "B01"),
    str_detect(variable, "sur_refl_b02_mean") ~ str_replace(variable, "sur_refl_b02_mean", "B02"),
    str_detect(variable, "sur_refl_b03_mean") ~ str_replace(variable, "sur_refl_b03_mean", "B03"),
    str_detect(variable, "sur_refl_b04_mean") ~ str_replace(variable, "sur_refl_b04_mean", "B04"),
    str_detect(variable, "sur_refl_b05_mean") ~ str_replace(variable, "sur_refl_b05_mean", "B05"),
    str_detect(variable, "sur_refl_b06_mean") ~ str_replace(variable, "sur_refl_b06_mean", "B06"),
    str_detect(variable, "sur_refl_b07_mean") ~ str_replace(variable, "sur_refl_b07_mean", "B07"),
    str_detect(variable, "total_obs") ~ str_replace(variable, "total_obs", "Total obs."),
    str_detect(variable, "site") ~ str_replace(variable, "site", "Site"),
    str_detect(variable, "gpp_dt_vut_ref") ~ str_replace(variable, "gpp_dt_vut_ref", "GPP"),
    .default = variable
  )) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

# Take a high gpp value
high_gpp <- weekly_500_train[167, ] 
high_gpp_pred <- predict(weekly_gpp_model, high_gpp)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = high_gpp,
                              type = "shap")
# rf_breakdown
high_value <- rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = case_when(
    str_detect(variable, "ndvi_mean") ~ str_replace(variable, "ndvi_mean", "NDVI"),
    str_detect(variable, "nirv_mean") ~ str_replace(variable, "nirv_mean", "NIRv"),
    str_detect(variable, "evi_mean" ) ~ str_replace(variable, "evi_mean", "EVI" ),
    str_detect(variable, "cci_mean" ) ~ str_replace(variable, "cci_mean", "CCI" ),
    str_detect(variable, "sur_refl_b01_mean") ~ str_replace(variable, "sur_refl_b01_mean", "B01"),
    str_detect(variable, "sur_refl_b02_mean") ~ str_replace(variable, "sur_refl_b02_mean", "B02"),
    str_detect(variable, "sur_refl_b03_mean") ~ str_replace(variable, "sur_refl_b03_mean", "B03"),
    str_detect(variable, "sur_refl_b04_mean") ~ str_replace(variable, "sur_refl_b04_mean", "B04"),
    str_detect(variable, "sur_refl_b05_mean") ~ str_replace(variable, "sur_refl_b05_mean", "B05"),
    str_detect(variable, "sur_refl_b06_mean") ~ str_replace(variable, "sur_refl_b06_mean", "B06"),
    str_detect(variable, "sur_refl_b07_mean") ~ str_replace(variable, "sur_refl_b07_mean", "B07"),
    str_detect(variable, "total_obs") ~ str_replace(variable, "total_obs", "Total obs."),
    str_detect(variable, "site") ~ str_replace(variable, "site", "Site"),
    str_detect(variable, "gpp_dt_vut_ref") ~ str_replace(variable, "gpp_dt_vut_ref", "GPP"),
    .default = variable
  )) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

rf_shap_weekly <- plot_grid(low_value,
          high_value,
          nrow = 1,
          labels = c('A', 'B'),
          vjust = 1)
# ```

# <!-- #### Monthly -->
  
  # ```{r monthly_500_rf}
#| echo: false
#| message: false
#| warning: false
set.seed(973)

monthly_500_split <- initial_split(monthly_500_rf, strata = site)
monthly_500_train <- training(monthly_500_split)
monthly_500_test <- testing(monthly_500_split)

# monthly_500_folds 
set.seed(365)
monthly_500_folds <- bootstraps(monthly_500_train,
                                times = 100,
                                strata = gpp_dt_vut_ref)

ranger_recipe <- 
  recipe(formula = gpp_dt_vut_ref ~ ., data = monthly_500_train) %>% 
  step_select(-site, -total_obs, skip = TRUE)

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

# Conditional to re-run model if no artifac was saved before.
if (fs::file_exists("models/monthly_500_fit_site.rds")) {
  monthly_500_fit <- readRDS("models/monthly_500_fit_site.rds")
  ranger_tune <- readRDS("models/monthly_500_site_ranger_tune.rds")
} else {
  set.seed(3159)
  
  doParallel::registerDoParallel()
  ranger_tune <-
    tune_grid(ranger_workflow, 
              resamples = monthly_500_folds, 
              grid = 12)
  
  # Final fit
  final_rf <- ranger_workflow %>% 
    finalize_workflow(select_best(ranger_tune))
  
  monthly_500_fit <- last_fit(final_rf, monthly_500_split)
  
  ## last_fit is saved if no model has been trained and saved.
  #saveRDS(ranger_tune, "models/monthly_500_site_ranger_tune.rds")
  #saveRDS(monthly_500_fit, "models/monthly_500_fit_site.rds")
}
# ```

# ```{r predictions_plot_monthly_500_rf}
#| label: fig-monthly_500_rf
#| fig-cap: "GPP observed and predicted values from the Random Forest for all the sites at a monthly basis. The red line represents a 1:1 relation. Metrics units are gC m⁻² d⁻¹"
#| fig-width: 6
#| fig-height: 4
#| echo: false
#| message: false
#| warning: false

# Explore RF results
## Check the metrics
monthly_metrics <- collect_metrics(monthly_500_fit) 

## Collect predictions
rf_pred_monthly <- plot_predictions_rf(monthly_500_fit, monthly_metrics, 5, 5, 13, 14)
# ```

# ```{r vip_plot_monthly_500_rf}
#| label: fig-vip_monthly_500_rf
#| fig-cap: "Variable of importance derived from the Random forest model for the monthly values at 500 m spatial resolution model."
#| echo: false
#| message: false
#| warning: false

## Feature importance
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune)) %>%
  set_engine("ranger", importance = "permutation")

rf_vip_monthly <- workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(imp_spec) %>%
  fit(monthly_500_train) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue")) +
  theme_light(base_size = 12) +
  scale_x_discrete(labels = c("ndvi_mean" = "NDVI",
                              "nirv_mean" = "NIRv",
                              "evi_mean" = "EVI",
                              "cci_mean" = "CCI",
                              "sur_refl_b01_mean" = "B01",
                              "sur_refl_b02_mean" = "B02",
                              "sur_refl_b03_mean" = "B03",
                              "sur_refl_b04_mean" = "B04",
                              "sur_refl_b05_mean" = "B05",
                              "sur_refl_b06_mean" = "B06",
                              "sur_refl_b07_mean" = "B07"))
# ```

# ```{r shapley_values_monthly}
#| label: fig-shap_monthly_500_rf
#| fig-cap: "Shapley values derived from the Random forest model for the monthly values at 500 m spatial resolution model. Predicted value for the low GPP value is 3.31 (A) and 10.6 for the selected high GPP value (B)."
#| echo: false
#| message: false
#| warning: false

# Extract workflow to obtain shapley values (or run predict)
monthly_gpp_model <- extract_workflow(monthly_500_fit)

# Create an explainer for a regression model
explainer_rf <- explain_tidymodels(
  monthly_gpp_model,
  data = monthly_500_train,
  y = monthly_500_train$gpp_dt_vut_ref,
  label = "rf",
  verbose = FALSE
)

# Take a low gpp value
low_gpp <- monthly_500_train[45, ] 
low_gpp_pred <- predict(monthly_gpp_model, low_gpp)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = low_gpp,
                              type = "shap")
# rf_breakdown
low_value <- rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = case_when(
    str_detect(variable, "ndvi_mean") ~ str_replace(variable, "ndvi_mean", "NDVI"),
    str_detect(variable, "nirv_mean") ~ str_replace(variable, "nirv_mean", "NIRv"),
    str_detect(variable, "evi_mean" ) ~ str_replace(variable, "evi_mean", "EVI" ),
    str_detect(variable, "cci_mean" ) ~ str_replace(variable, "cci_mean", "CCI" ),
    str_detect(variable, "sur_refl_b01_mean") ~ str_replace(variable, "sur_refl_b01_mean", "B01"),
    str_detect(variable, "sur_refl_b02_mean") ~ str_replace(variable, "sur_refl_b02_mean", "B02"),
    str_detect(variable, "sur_refl_b03_mean") ~ str_replace(variable, "sur_refl_b03_mean", "B03"),
    str_detect(variable, "sur_refl_b04_mean") ~ str_replace(variable, "sur_refl_b04_mean", "B04"),
    str_detect(variable, "sur_refl_b05_mean") ~ str_replace(variable, "sur_refl_b05_mean", "B05"),
    str_detect(variable, "sur_refl_b06_mean") ~ str_replace(variable, "sur_refl_b06_mean", "B06"),
    str_detect(variable, "sur_refl_b07_mean") ~ str_replace(variable, "sur_refl_b07_mean", "B07"),
    str_detect(variable, "total_obs") ~ str_replace(variable, "total_obs", "Total obs."),
    str_detect(variable, "site") ~ str_replace(variable, "site", "Site"),
    str_detect(variable, "gpp_dt_vut_ref") ~ str_replace(variable, "gpp_dt_vut_ref", "GPP"),
    .default = variable
  )) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

# Take a high gpp value
high_gpp <- monthly_500_train[6, ] 
high_gpp_pred <- predict(monthly_gpp_model, high_gpp)

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = high_gpp,
                              type = "shap")
# rf_breakdown
high_value <- rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = case_when(
    str_detect(variable, "ndvi_mean") ~ str_replace(variable, "ndvi_mean", "NDVI"),
    str_detect(variable, "nirv_mean") ~ str_replace(variable, "nirv_mean", "NIRv"),
    str_detect(variable, "evi_mean" ) ~ str_replace(variable, "evi_mean", "EVI" ),
    str_detect(variable, "cci_mean" ) ~ str_replace(variable, "cci_mean", "CCI" ),
    str_detect(variable, "sur_refl_b01_mean") ~ str_replace(variable, "sur_refl_b01_mean", "B01"),
    str_detect(variable, "sur_refl_b02_mean") ~ str_replace(variable, "sur_refl_b02_mean", "B02"),
    str_detect(variable, "sur_refl_b03_mean") ~ str_replace(variable, "sur_refl_b03_mean", "B03"),
    str_detect(variable, "sur_refl_b04_mean") ~ str_replace(variable, "sur_refl_b04_mean", "B04"),
    str_detect(variable, "sur_refl_b05_mean") ~ str_replace(variable, "sur_refl_b05_mean", "B05"),
    str_detect(variable, "sur_refl_b06_mean") ~ str_replace(variable, "sur_refl_b06_mean", "B06"),
    str_detect(variable, "sur_refl_b07_mean") ~ str_replace(variable, "sur_refl_b07_mean", "B07"),
    str_detect(variable, "total_obs") ~ str_replace(variable, "total_obs", "Total obs."),
    str_detect(variable, "site") ~ str_replace(variable, "site", "Site"),
    str_detect(variable, "gpp_dt_vut_ref") ~ str_replace(variable, "gpp_dt_vut_ref", "GPP"),
    .default = variable
  )) %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

rf_shap_monthly <- plot_grid(low_value,
          high_value,
          nrow = 1,
          labels = c('A', 'B'),
          vjust = 1)
# ```
