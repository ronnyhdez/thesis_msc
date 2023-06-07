# File to read the analysis ready datasets and create time scales data frames
#
# Objective of this file is to have big portions of the code here and call the 
# created objects in the chapter file, without clumping the chapter file with
# many code lines, and leave it mainly for text.
#
# There is a source call to a function file.

# Libraries
library(dplyr)
library(tidyr)


# Read ARD datasets files

## Michigan
michigan_daily_250 <-  readRDS("data_ard/michigan_daily_250.rds")
michigan_daily_500 <-  readRDS("data_ard/michigan_daily_500.rds")
michigan_weekly_250 <-  readRDS("data_ard/michigan_weekly_250.rds")
michigan_weekly_500 <-  readRDS("data_ard/michigan_weekly_500.rds")
michigan_monthly_250 <- readRDS("data_ard/michigan_montly_250.rds") 
michigan_monthly_500 <- readRDS("data_ard/michigan_montly_500.rds") 

## Borden
borden_daily_250 <-  readRDS("data_ard/borden_daily_250.rds")
borden_daily_500 <-  readRDS("data_ard/borden_daily_500.rds")
borden_weekly_250 <-  readRDS("data_ard/borden_weekly_250.rds")
borden_weekly_500 <-  readRDS("data_ard/borden_weekly_500.rds")
borden_monthly_250 <- readRDS("data_ard/borden_montly_250.rds") 
borden_monthly_500 <- readRDS("data_ard/borden_montly_500.rds")

## Bartlett
bartlett_daily_250 <-  readRDS("data_ard/bartlett_daily_250.rds")
bartlett_daily_500 <-  readRDS("data_ard/bartlett_daily_500.rds")
bartlett_weekly_250 <-  readRDS("data_ard/bartlett_weekly_250.rds")
bartlett_weekly_500 <-  readRDS("data_ard/bartlett_weekly_500.rds")
bartlett_monthly_250 <- readRDS("data_ard/bartlett_montly_250.rds") 
bartlett_monthly_500 <- readRDS("data_ard/bartlett_montly_500.rds")


# Prepare datasets for figures

## 500m resolution data preparation

# 500

## Daily -----------------------------------------------------------------------
michigan_plot_daily <- michigan_daily_500 %>% 
  select(date, gpp_dt_vut_ref, ends_with("mean"), total_obs) %>% 
  select(-starts_with("sur")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Michigan")

borden_plot_daily <- borden_daily_500 %>% 
  select(date, gpp_dt_vut_ref, ends_with("mean"), total_obs) %>% 
  select(-starts_with("sur")) %>% 
  # filter(ndvi_mean > 3 & gpp_dt_vut_ref < 20) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Borden") %>% 
  filter(!(index == "ndvi_mean" & value < 0.5 & gpp_dt_vut_ref > 8))

bartlett_plot_daily <- bartlett_daily_500 %>% 
  select(date, gpp_dt_vut_ref, ends_with("mean"), total_obs) %>% 
  select(-starts_with("sur")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Bartlett")

daily_plot_500 <- borden_plot_daily %>% 
  bind_rows(michigan_plot_daily) %>% 
  bind_rows(bartlett_plot_daily)

## Weekly ----------------------------------------------------------------------
michigan_plot_weekly <- michigan_weekly_500 %>% 
  select(date_start, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date_start, gpp_dt_vut_ref, total_obs), 
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Michigan")

borden_plot_weekly <- borden_weekly_500 %>% 
  select(date_start, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date_start, gpp_dt_vut_ref, total_obs), 
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Borden")

bartlett_plot_weekly <- bartlett_weekly_500 %>% 
  select(date_start, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date_start, gpp_dt_vut_ref, total_obs), 
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Bartlett")

weekly_plot_500 <- borden_plot_weekly %>% 
  bind_rows(michigan_plot_weekly) %>% 
  bind_rows(bartlett_plot_weekly)

# Monthly ----------------------------------------------------------------------
michigan_plot_monthly <- michigan_monthly_500 %>% 
  select(date, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Michigan")

borden_plot_monthly <- borden_monthly_500 %>% 
  select(date, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Borden")

bartlett_plot_monthly <- bartlett_monthly_500 %>% 
  select(date, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Bartlett")

monthly_plot_500 <- borden_plot_monthly %>% 
  bind_rows(michigan_plot_monthly) %>% 
  bind_rows(bartlett_plot_monthly)

# 250

### 250m resolution data preparation

## Daily -----------------------------------------------------------------------
michigan_plot_daily <- michigan_daily_250 %>% 
  select(date, gpp_dt_vut_ref, ends_with("mean"), total_obs) %>% 
  select(-starts_with("sur")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Michigan")

borden_plot_daily <- borden_daily_250 %>% 
  select(date, gpp_dt_vut_ref, ends_with("mean"), total_obs) %>% 
  select(-starts_with("sur")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Borden")

bartlett_plot_daily <- bartlett_daily_250 %>% 
  select(date, gpp_dt_vut_ref, ends_with("mean"), total_obs) %>% 
  select(-starts_with("sur")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Bartlett")

daily_plot_250 <- borden_plot_daily %>% 
  bind_rows(michigan_plot_daily) %>% 
  bind_rows(bartlett_plot_daily)

## Weekly ----------------------------------------------------------------------
michigan_plot_weekly <- michigan_weekly_250 %>% 
  select(date_start, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date_start, gpp_dt_vut_ref, total_obs), 
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Michigan")

borden_plot_weekly <- borden_weekly_250 %>% 
  select(date_start, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date_start, gpp_dt_vut_ref, total_obs), 
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Borden")

bartlett_plot_weekly <- bartlett_weekly_250 %>% 
  select(date_start, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date_start, gpp_dt_vut_ref, total_obs), 
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Bartlett")

weekly_plot_250 <- borden_plot_weekly %>% 
  bind_rows(michigan_plot_weekly) %>% 
  bind_rows(bartlett_plot_weekly)

# Monthly ----------------------------------------------------------------------
michigan_plot_monthly <- michigan_monthly_250 %>% 
  select(date, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Michigan")

borden_plot_monthly <- borden_monthly_250 %>% 
  select(date, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Borden")

bartlett_plot_monthly <- bartlett_monthly_250 %>% 
  select(date, gpp_dt_vut_ref, total_obs,
         starts_with(c("ndvi", "evi", "nirv")) & ends_with("mean")) %>% 
  pivot_longer(-c(date, gpp_dt_vut_ref, total_obs),  
               names_to = "index", values_to = "value") %>% 
  mutate(site = "Bartlett")

monthly_plot_250 <- borden_plot_monthly %>% 
  bind_rows(michigan_plot_monthly) %>% 
  bind_rows(bartlett_plot_monthly)

