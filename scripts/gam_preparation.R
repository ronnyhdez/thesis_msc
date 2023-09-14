# File to prepare the GAM models
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
library(mgcv) 

source("scripts/models_data_preparation.R")

# DAILY GAMS -------------------------------------------------------------------
# Prepare data with all sites
daily_gam <- daily_plot_500 %>% 
  pivot_wider(names_from = index, values_from = value) %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, cci_mean, site) %>% 
  mutate(site = as.factor(site))

# Create functions to extract metrics
rsq_fun <- function(mod) {
  summary(mod)[["r.sq"]]
}

rmse_fun <- function(mod) {
  sqrt(mean((mod[["residuals"]])^2))
}

mae_fun <- function(mod) {
  mean(abs(mod[["residuals"]]))
}

# GAM model for all sites [E | Diff VIs] ----
group_site <- daily_gam %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index",
               values_to = "value") %>% 
  nest(data = c(-index)) 

mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ s(value, k = 10), 
      data = df, 
      method = 'REML')
}

models <- group_site %>% 
  mutate(model = map(data, mod_fun))

all_sites_gam_daily <- models %>% 
  transmute(index,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  mutate(site = "All") %>% 
  select(site, index, rsq, mae, rmse)

# Tabla metricas completas
all_sites_gam_daily_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  # result_p_table <- summary(models[[3]][[i]])[["p.table"]] %>%
  #   as.data.frame() %>%
  #   mutate(index = models$index[i]) %>%
  #   rownames_to_column("intercept") %>%
  #   janitor::clean_names() %>%
  #   select(-intercept)  
  
  result_s_table <- summary(models[[3]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  aic_table <- models[[3]][[i]][["aic"]] %>% 
    as.data.frame(nm = "AIC") %>% 
    mutate(index = models$index[i])
  
  result <-  result_s_table %>% 
    left_join(aic_table, by = "index") %>% 
    mutate(site = "All") %>% 
    select(site, index, edf, f, p_value, AIC) %>% 
    arrange(desc("AIC"))
  
  return(result)
})

# GAM model for all sites diff VIs [F | Diff VIS + site] ---- 
group_site <- daily_gam %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index",
               values_to = "value") %>% 
  nest(data = c(-index, -site)) 

mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ s(value, k = 10), 
      data = df, 
      method = 'REML')
}

models <- group_site %>% 
  mutate(model = map(data, mod_fun))

vis_sites_gam_daily <- models %>% 
  transmute(index, site,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  arrange(desc(rsq))

# Tabla metricas completas
vis_sites_gam_daily_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  # result_p_table <- summary(models[[4]][[i]])[["p.table"]] %>%
  #   as.data.frame() %>%
  #   mutate(index = models$index[i],
  #          site = models$site[i]) %>%
  #   rownames_to_column("intercept") %>%
  #   janitor::clean_names() %>%
  #   select(-intercept)  
  
  result_s_table <- summary(models[[4]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  aic_table <- models[[4]][[i]][["aic"]] %>% 
    as.data.frame(nm = "AIC") %>% 
    mutate(index = models$index[i],
           site = models$site[i])
  
  result <-  result_s_table %>% 
    left_join(aic_table, by = c("index", "site")) %>% 
    select(site, index, edf, f, p_value, AIC) %>% 
    arrange(desc("AIC"))
  
  return(result)
})

# GAM model for all VI's [G | All VIs] ---- 
mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ ndvi_mean +
        # kndvi_mean +
        s(evi_mean) +
        s(nirv_mean) +
        s(cci_mean),
      data = df, 
      method = 'REML')
}

group_site <- daily_gam %>% 
  nest(data = c(-site)) 

models <- group_site %>% 
  mutate(model = map(data, mod_fun),
         index = "All")

all_vis_gam_daily <- models %>% 
  transmute(index, site,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  arrange(desc(rsq))

# Tabla metricas completas
all_vis_gam_daily_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[3]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)
  
  result_s_table <- summary(models[[3]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)
  
  result <-  result_p_table %>% 
    left_join(result_s_table, by = "site")
  
  return(result)
})


# GAM model for all sites and all indices (covariates) [H | All VIs + site] ----
single_vis_daily <- gam(gpp_dt_vut_ref ~ ndvi_mean +
                          # kndvi_mean +
                          s(evi_mean) +
                          s(nirv_mean) +
                          s(cci_mean),
                        data = daily_gam, 
                        method = 'REML')

# **Daily models outputs**
all_sites_all_vis_gam_daily <- tribble(
  ~index, ~rsq, ~rmse, ~mae,
  "All", summary(single_vis_daily)[["r.sq"]],
  sqrt(mean((single_vis_daily[["residuals"]])^2)),
  mean(abs(single_vis_daily[["residuals"]]))
) %>% 
  mutate(site = "All") %>% 
  select(index, site, rsq, rmse, mae)

# Tabla metricas completas
result_p_table <- summary(single_vis_daily)[["p.table"]] %>%
  as.data.frame() %>%
  # mutate(index = models$index[i],
  #        site = models$site[i]) %>%
  rownames_to_column("intercept") %>%
  janitor::clean_names() #%>%
  # select(-intercept)

result_s_table <- summary(single_vis_daily)[["s.table"]] %>%
  as.data.frame() %>%
  # mutate(index = models$index[i],
  #        site = models$site[i]) %>%
  rownames_to_column("intercept") %>%
  janitor::clean_names() #%>%
  # select(-intercept)  
  
  # result <-  result_p_table %>% 
  # left_join(result_s_table, by = "site")

# WEEKLY GAMS ------------------------------------------------------------------
# Prepare data with all sites
weekly_gam <- weekly_plot_500 %>% 
  pivot_wider(names_from = index, values_from = value) %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, cci_mean, site) %>% 
  mutate(site = as.factor(site))

# Create functions to extract metrics
rsq_fun <- function(mod) {
  summary(mod)[["r.sq"]]
}

rmse_fun <- function(mod) {
  sqrt(mean((mod[["residuals"]])^2))
}

mae_fun <- function(mod) {
  mean(abs(mod[["residuals"]]))
}

# GAM model for all sites [E | Diff VIs] ----
group_site <- weekly_gam %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index",
               values_to = "value") %>% 
  nest(data = c(-index)) 

mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ s(value, k = 10), 
      data = df, 
      method = 'REML')
}

models <- group_site %>% 
  mutate(model = map(data, mod_fun))

all_sites_gam_weekly <- models %>% 
  transmute(index,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  mutate(site = "All") %>% 
  select(site, index, rsq, mae, rmse)

# Tabla metricas completas
all_sites_gam_weekly_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[3]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result_s_table <- summary(models[[3]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result <-  result_p_table %>% 
    left_join(result_s_table, by = "index")
  
  return(result)
})


# GAM model for all sites diff VIs [F | Diff VIS + site] ----
group_site <- weekly_gam %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index",
               values_to = "value") %>% 
  nest(data = c(-index, -site)) 

mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ s(value, k = 10), 
      data = df, 
      method = 'REML')
}

models <- group_site %>% 
  mutate(model = map(data, mod_fun))

vis_sites_gam_weekly <- models %>% 
  transmute(index, site,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  arrange(desc(rsq))

# Tabla metricas completas
vis_sites_gam_weekly_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[4]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result_s_table <- summary(models[[4]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result <-  result_p_table %>% 
    left_join(result_s_table, by = c("index", "site"))
  
  return(result)
})

# GAM model for all VI's [G | All VIs] ----
mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ ndvi_mean +
        # kndvi_mean +
        s(evi_mean) +
        s(nirv_mean) +
        s(cci_mean),
      data = df, 
      method = 'REML')
}

group_site <- weekly_gam %>% 
  nest(data = c(-site)) 

models <- group_site %>% 
  mutate(model = map(data, mod_fun),
         index = "All")

all_vis_gam_weekly <- models %>% 
  transmute(index, site,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  arrange(desc(rsq))

# Tabla metricas completas
all_vis_gam_weekly_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[3]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)
  
  result_s_table <- summary(models[[3]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)
    
    result <-  result_p_table %>% 
    left_join(result_s_table, by = "site")
  
  return(result)
})

# GAM model for all sites and all indices (covariates) [H | All VIs + site] ----
single_vis_weekly <- gam(gpp_dt_vut_ref ~ ndvi_mean +
                          # kndvi_mean +
                          s(evi_mean) +
                          s(nirv_mean) +
                          s(cci_mean),
                        data = weekly_gam, 
                        method = 'REML')

# **Daily models outputs**
all_sites_all_vis_gam_weekly <- tribble(
  ~index, ~rsq, ~rmse, ~mae,
  "All", summary(single_vis_daily)[["r.sq"]],
  sqrt(mean((single_vis_daily[["residuals"]])^2)),
  mean(abs(single_vis_daily[["residuals"]]))
) %>% 
  mutate(site = "All") %>% 
  select(index, site, rsq, rmse, mae)

# Tabla metricas completas
result_p_table <- summary(single_vis_weekly)[["p.table"]] %>%
  as.data.frame() %>%
  # mutate(index = models$index[i],
  #        site = models$site[i]) %>%
  rownames_to_column("intercept") %>%
  janitor::clean_names() #%>%
# select(-intercept)

result_s_table <- summary(single_vis_weekly)[["s.table"]] %>%
  as.data.frame() %>%
  # mutate(index = models$index[i],
  #        site = models$site[i]) %>%
  rownames_to_column("intercept") %>%
  janitor::clean_names() #%>%
# select(-intercept)  

# result <-  result_p_table %>% 
# left_join(result_s_table, by = "site")

# MONTHLY GAMS ------------------------------------------------------------------
# Prepare data with all sites
monthly_gam <- monthly_plot_500 %>% 
  pivot_wider(names_from = index, values_from = value) %>% 
  select(gpp_dt_vut_ref, evi_mean, ndvi_mean, 
         nirv_mean, cci_mean, site) %>% 
  mutate(site = as.factor(site))

# Create functions to extract metrics
rsq_fun <- function(mod) {
  summary(mod)[["r.sq"]]
}

rmse_fun <- function(mod) {
  sqrt(mean((mod[["residuals"]])^2))
}

mae_fun <- function(mod) {
  mean(abs(mod[["residuals"]]))
}

# GAM model for all sites [E | Diff VIs] ---- 
group_site <- monthly_gam %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index",
               values_to = "value") %>% 
  nest(data = c(-index)) 

mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ s(value, k = 10), 
      data = df, 
      method = 'REML')
}

models <- group_site %>% 
  mutate(model = map(data, mod_fun))

all_sites_gam_monthly <- models %>% 
  transmute(index,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  mutate(site = "All") %>% 
  select(site, index, rsq, mae, rmse)

# Tabla metricas completas
all_sites_gam_monthly_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[3]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result_s_table <- summary(models[[3]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result <-  result_p_table %>% 
    left_join(result_s_table, by = "index")
  
  return(result)
})

# GAM model for all sites diff VIs [F | Diff VIS + site] ---- 
group_site <- monthly_gam %>% 
  pivot_longer(cols = c(ends_with("mean")), names_to = "index",
               values_to = "value") %>% 
  nest(data = c(-index, -site)) 

mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ s(value, k = 10), 
      data = df, 
      method = 'REML')
}

models <- group_site %>% 
  mutate(model = map(data, mod_fun))

vis_sites_gam_monthly <- models %>% 
  transmute(index, site,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  arrange(desc(rsq))

# Tabla metricas completas
vis_sites_gam_monthly_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[4]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result_s_table <- summary(models[[4]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)  
  
  result <-  result_p_table %>% 
    left_join(result_s_table, by = c("index", "site"))
  
  return(result)
})

# GAM model for all VI's [G | All VIs] ----
mod_fun <- function(df) {
  gam(gpp_dt_vut_ref ~ ndvi_mean +
        # kndvi_mean +
        s(evi_mean) +
        s(nirv_mean) +
        s(cci_mean),
      data = df, 
      method = 'REML')
}

group_site <- monthly_gam %>% 
  nest(data = c(-site)) 

models <- group_site %>% 
  # Michigan & Bartlett have few data points.
  filter(site == "Borden") %>% 
  mutate(model = map(data, mod_fun),
         index = "All")

all_vis_gam_monthly <- models %>% 
  transmute(index, site,
            rsq = map_dbl(model, rsq_fun),
            rmse = map_dbl(model, rmse_fun),
            mae = map_dbl(model, mae_fun)) %>% 
  arrange(desc(rsq)) %>%
  # Insert NA values for Bartlett & Michigan which does not have
  # enough obs. for this kind of model
  bind_rows(
    tibble(
      index = rep("All", 2),
      site = c("Bartlett", "Michigan"),
      rsq = rep(NA, 2),
      rmse = rep(NA, 2),
      mae = rep(NA, 2)
    )
  )

# Tabla metricas completas
all_vis_gam_monthly_complete <- map_dfr(1:nrow(models), function(i) {
  print(i)
  result_p_table <- summary(models[[3]][[i]])[["p.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)
  
  result_s_table <- summary(models[[3]][[i]])[["s.table"]] %>%
    as.data.frame() %>%
    mutate(index = models$index[i],
           site = models$site[i]) %>%
    rownames_to_column("intercept") %>%
    janitor::clean_names() %>%
    select(-intercept)
  
  result <-  result_p_table %>% 
    left_join(result_s_table, by = "site")
  
  return(result)
})

# GAM model for all sites and all indices (covariates) [H | All VIs + site] ----
single_vis_monthly <- gam(gpp_dt_vut_ref ~ ndvi_mean +
                           # kndvi_mean +
                           s(evi_mean) +
                           s(nirv_mean) +
                           s(cci_mean),
                         data = weekly_gam, 
                         method = 'REML')

# **Monthly models outputs**
all_sites_all_vis_gam_monthly <- tribble(
  ~index, ~rsq, ~rmse, ~mae,
  "All", summary(single_vis_daily)[["r.sq"]],
  sqrt(mean((single_vis_daily[["residuals"]])^2)),
  mean(abs(single_vis_daily[["residuals"]]))
) %>% 
  mutate(site = "All") %>% 
  select(index, site, rsq, rmse, mae)

# Tabla metricas completas
result_p_table <- summary(single_vis_monthly)[["p.table"]] %>%
  as.data.frame() %>%
  # mutate(index = models$index[i],
  #        site = models$site[i]) %>%
  rownames_to_column("intercept") %>%
  janitor::clean_names() #%>%
# select(-intercept)

result_s_table <- summary(single_vis_monthly)[["s.table"]] %>%
  as.data.frame() %>%
  # mutate(index = models$index[i],
  #        site = models$site[i]) %>%
  rownames_to_column("intercept") %>%
  janitor::clean_names() #%>%
# select(-intercept)  

# result <-  result_p_table %>% 
# left_join(result_s_table, by = "site")





