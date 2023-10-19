set.seed(123)
daily_500_split <- initial_split(daily_500_rf, prop = 0.70, strata = site)
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
  # add_formula(gpp_dt_vut_ref ~ .) %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(ranger_workflow, 
            resamples = daily_500_folds, 
            grid = 12)

# Final fit
final_rf <- ranger_workflow %>% 
  finalize_workflow(select_best(ranger_tune))

# daily_500_fit <- fit(final_rf, daily_500_train)

daily_500_fit <- last_fit(final_rf, daily_500_split)
daily_gpp_model <- extract_workflow(daily_500_fit)

# test <- extract_workflow(daily_500_fit)
# daily_500_last_fit <- daily_500_fit

# predict(test, new_data = daily_500_test)

predict(daily_gpp_model, new_data = daily_500_test)


# SHA values ----
library(DALEX)
library(DALEXtra)

# Create an explainer for a regression model
explainer_rf <- explain_tidymodels(
  daily_gpp_model,
  # daily_500_fit,
  data = daily_500_train,
  y = daily_500_train$gpp_dt_vut_ref,
  label = "rf",
  verbose = FALSE
)

## Local explanations

# Take a high gpp value
high_gpp <- daily_500_train[16, ] 

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = high_gpp,
                              type = "shap")

rf_breakdown

library(forcats)
rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)

# normal value
high_gpp <- daily_500_train[43, ] 

rf_breakdown <- predict_parts(explainer = explainer_rf, 
                              new_observation = high_gpp,
                              type = "shap")
rf_breakdown

library(forcats)
rf_breakdown %>%
  group_by(variable) %>%
  mutate(mean_val = mean(contribution)) %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, abs(mean_val))) %>%
  ggplot(aes(contribution, variable, fill = mean_val > 0)) +
  geom_col(data = ~distinct(., variable, mean_val), 
           aes(mean_val, variable), 
           alpha = 0.5) +
  geom_boxplot(width = 0.5) +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(y = NULL)


