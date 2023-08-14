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
library(h2o)
library(mgcv) 

source("scripts/models_data_preparation.R")

# DAILY LMS --------------------------------------------------------------------
daily_gam <- daily_plot_500 %>% 
  pivot_wider(names_from = index, values_from = value) %>% 
  mutate(site = as.factor(site))

# Daily ndvi 
model_ndvi_daily <- gam(gpp_dt_vut_ref ~ s(ndvi_mean, k = 10), 
                        data = daily_gam, 
                        method = 'REML')

# Daily evi 
model_evi_daily <- gam(gpp_dt_vut_ref ~ s(evi_mean, k = 10), 
                       data = daily_gam, 
                       method = 'REML')

# Daily nirv 
model_nirv_daily <- gam(gpp_dt_vut_ref ~ s(nirv_mean, k = 10), 
                        data = daily_gam, 
                        method = 'REML')

# Daily kndvi
model_kndvi_daily <- gam(gpp_dt_vut_ref ~ s(kndvi_mean, k = 10), 
                         data = daily_gam, 
                         method = 'REML')

# Daily cci
model_cci_daily <- gam(gpp_dt_vut_ref ~ s(cci_mean, k = 10), 
                       data = daily_gam, 
                       method = 'REML')

# **Daily models outputs**
summ_ndvi_daily <- summary(model_ndvi_daily)
summ_evi_daily <- summary(model_evi_daily)
summ_nirv_daily <- summary(model_nirv_daily)
summ_kndvi_daily <- summary(model_kndvi_daily)
summ_cci_daily <- summary(model_cci_daily)

# Tables for comparison (this can be a function)
s_table_evi_daily <- summ_evi_daily[["s.table"]] %>% as.data.frame()
s_table_ndvi_daily <- summ_ndvi_daily[["s.table"]] %>% as.data.frame()
s_table_nirv_daily <- summ_nirv_daily[["s.table"]] %>% as.data.frame()
s_table_kndvi_daily <- summ_kndvi_daily[["s.table"]] %>% as.data.frame()
s_table_cci_daily <- summ_cci_daily[["s.table"]] %>% as.data.frame()

p_table_evi_daily <- summ_evi_daily[["p.table"]] %>% as.data.frame()
p_table_ndvi_daily <- summ_ndvi_daily[["p.table"]] %>% as.data.frame()
p_table_nirv_daily <- summ_nirv_daily[["p.table"]] %>% as.data.frame()
p_table_kndvi_daily <- summ_kndvi_daily[["p.table"]] %>% as.data.frame()
p_table_cci_daily <- summ_cci_daily[["p.table"]] %>% as.data.frame()