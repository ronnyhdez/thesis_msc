# File to read ONEFlux datasets and create trends plots
#
# Objective of this file is to have big portions of the code here and call the 
# created objects in the chapter file, without clumping the chapter file with
# many code lines, and leave it mainly for text.
#
# There is a source call to a function file.

# Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(cowplot)
library(lubridate)
# Personal functions
source("R/import_oneflux_data.R")

# MICHIGAN ------

## Read files ----

### Daily fluxes ----
file <-
  "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/FLX_US-UMB_FLUXNET2015_FULLSET_DD_2007-2017_beta-4.csv"

one_flux_michigan_daily <- read_flux_file(file = file, timeframe = "daily")

### Weekly fluxes ----
file <- 
  "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/FLX_US-UMB_FLUXNET2015_FULLSET_WW_2007-2017_beta-4.csv"
one_flux_michigan_weekly <- read_flux_file(file = file, timeframe = "weekly")

### Monthly fluxes ----
file <-
  "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/FLX_US-UMB_FLUXNET2015_FULLSET_MM_2007-2017_beta-4.csv"
one_flux_michigan_monthly <- read_flux_file(file = file, timeframe = "monthly")

# BORDEN ------

## Read files ----

### Daily fluxes ----
file <- "data/flux_borden_oneflux/AMF_CA-Cbo_FLUXNET_SUBSET_1994-2020_3-5/AMF_CA-Cbo_FLUXNET_SUBSET_DD_1994-2020_3-5.csv"
one_flux_borden_daily <- read_flux_file(file = file, timeframe = "daily")

### Weekly fluxes ----
file <- "data/flux_borden_oneflux/AMF_CA-Cbo_FLUXNET_SUBSET_1994-2020_3-5/AMF_CA-Cbo_FLUXNET_SUBSET_WW_1994-2020_3-5.csv"
one_flux_borden_weekly <- read_flux_file(file = file, timeframe = "weekly")

### Monthly fluxes ----
file <- "data/flux_borden_oneflux/AMF_CA-Cbo_FLUXNET_SUBSET_1994-2020_3-5/AMF_CA-Cbo_FLUXNET_SUBSET_MM_1994-2020_3-5.csv"
one_flux_borden_monthly <- read_flux_file(file = file, timeframe = "monthly")

# BARTLETT ------

## Read files ----

### Daily fluxes ----
file <-
  "data/FLX_US-Bar_FLUXNET2015_FULLSET_2005-2017_beta-3/FLX_US-Bar_FLUXNET2015_FULLSET_DD_2005-2017_beta-3.csv"
one_flux_bartlett_daily <- read_flux_file(file = file, timeframe = "daily")

### Weekly fluxes ----
file <-
  "data/FLX_US-Bar_FLUXNET2015_FULLSET_2005-2017_beta-3/FLX_US-Bar_FLUXNET2015_FULLSET_WW_2005-2017_beta-3.csv"
one_flux_bartlett_weekly <- read_flux_file(file = file, timeframe = "weekly")

### Monthly fluxes ----
file <-
  "data/FLX_US-Bar_FLUXNET2015_FULLSET_2005-2017_beta-3/FLX_US-Bar_FLUXNET2015_FULLSET_MM_2005-2017_beta-3.csv"
one_flux_bartlett_monthly <- read_flux_file(file = file, timeframe = "monthly")

## Create plots ----

### Daily plot ----

## Daily plot with just gpp_dt_vut_ref from all the sites together
daily_gpp_michigan <- one_flux_michigan_daily %>% 
  select(date, gpp_dt_vut_ref) %>% 
  mutate(Site = "Michigan")

daily_gpp_bartlett <- one_flux_bartlett_daily %>% 
  select(date, gpp_dt_vut_ref) %>% 
  mutate(Site = "Bartlett")

daily_gpp_borden <- one_flux_borden_daily %>% 
  select(date, gpp_dt_vut_ref) %>% 
  mutate(Site = "Borden")

daily_gpp_trend <- bind_rows(daily_gpp_bartlett, 
                            daily_gpp_borden,
                            daily_gpp_michigan) %>% 
  ggplot(aes(x = date, y = gpp_dt_vut_ref, color = Site)) +
  geom_jitter(alpha = 0.6) +
  geom_line(alpha = 0.6) +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
  scale_y_continuous(breaks = seq(0, 60, by = 2)) +
  labs(x = "Date", 
       y = expression(GPP~(gC~m^{"-2"}~d^-1))) +
  # guides(color = "none") +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, h = 1))
  

# daily_gpp_trend <-
#   one_flux_michigan_daily %>%
#   select(date, starts_with("gpp")) %>%
#   pivot_longer(-date, names_to = "gpp_method", values_to = "gpp_value") %>% glimpse()
#   filter(gpp_method %in% c("gpp_dt_vut_25",
#                            "gpp_dt_vut_50",
#                            "gpp_dt_vut_75",
#                            "gpp_dt_vut_ref",
#                            "gpp_nt_vut_25",
#                            "gpp_nt_vut_50",
#                            "gpp_nt_vut_75",
#                            "gpp_nt_vut_ref"
#   )) %>% 
#   ggplot(aes(x = date, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 60, by = 2)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux daily GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))

### Weekly plot ----
## Weekly plot with just gpp_dt_vut_ref from all the sites together
weekly_gpp_michigan <- one_flux_michigan_weekly %>% 
  select(date_start, gpp_dt_vut_ref) %>% 
  mutate(Site = "Michigan")

weekly_gpp_bartlett <- one_flux_bartlett_weekly %>% 
  select(date_start, gpp_dt_vut_ref) %>% 
  mutate(Site = "Bartlett")

weekly_gpp_borden <- one_flux_borden_weekly %>% 
  select(date_start, gpp_dt_vut_ref) %>% 
  mutate(Site = "Borden")

weekly_gpp_trend <- bind_rows(weekly_gpp_bartlett, 
                              weekly_gpp_borden,
                              weekly_gpp_michigan) %>% 
  ggplot(aes(x = date_start, y = gpp_dt_vut_ref, color = Site)) +
  geom_jitter(alpha = 0.6) +
  geom_line(alpha = 0.6) +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
  scale_y_continuous(breaks = seq(0, 60, by = 2)) +
  labs(x = "Date", 
       y = expression(GPP~(gC~m^{"-2"}~d^-1))) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, h = 1))

# weekly_gpp_trend  <- one_flux_michigan_weekly %>%
#   select(date_start, starts_with("gpp")) %>%
#   pivot_longer(-date_start, names_to = "gpp_method", values_to = "gpp_value") %>%
#   filter(gpp_method %in% c("gpp_dt_vut_25",
#                            "gpp_dt_vut_50",
#                            "gpp_dt_vut_75",
#                            "gpp_dt_vut_ref",
#                            "gpp_nt_vut_25",
#                            "gpp_nt_vut_50",
#                            "gpp_nt_vut_75",
#                            "gpp_nt_vut_ref"
#   )) %>% 
#   ggplot(aes(x = date_start, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 32, by = 2)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux weekly GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))

### Monthly plot ----
## Weekly plot with just gpp_dt_vut_ref from all the sites together
monthly_gpp_michigan <- one_flux_michigan_monthly %>% 
  select(date, gpp_dt_vut_ref) %>% 
  mutate(Site = "Michigan")

monthly_gpp_bartlett <- one_flux_bartlett_monthly %>% 
  select(date, gpp_dt_vut_ref) %>% 
  mutate(Site = "Bartlett")

monthly_gpp_borden <- one_flux_borden_monthly %>% 
  select(date, gpp_dt_vut_ref) %>% 
  mutate(Site = "Borden")

monthly_gpp_trend <- bind_rows(monthly_gpp_bartlett, 
                               monthly_gpp_borden,
                               monthly_gpp_michigan) %>% 
  mutate(date = zoo::as.Date(date)) %>%
  ggplot(aes(x = date, y = gpp_dt_vut_ref, color = Site)) +
  geom_jitter(alpha = 0.6) +
  geom_line(alpha = 0.6) +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
  scale_y_continuous(breaks = seq(0, 60, by = 2)) +
  labs(x = "Date",
       y = expression(GPP~(gC~m^{"-2"}~d^-1))) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, h = 1))

# monthly_gpp_trend  <- one_flux_michigan_monthly %>%
#   select(date, starts_with("gpp")) %>%
#   pivot_longer(-date, names_to = "gpp_method", values_to = "gpp_value") %>%
#   filter(gpp_method %in% c("gpp_dt_vut_25",
#                            "gpp_dt_vut_50",
#                            "gpp_dt_vut_75",
#                            "gpp_dt_vut_ref",
#                            "gpp_nt_vut_25",
#                            "gpp_nt_vut_50",
#                            "gpp_nt_vut_75",
#                            "gpp_nt_vut_ref"
#   )) %>% 
#   mutate(date = zoo::as.Date(date)) %>%
#   ggplot(aes(x = date, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 28, by = 2)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux monthly GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))

## Export cowplot ----
gpp_trends <- plot_grid(daily_gpp_trend + theme(legend.position = "none"),
                        weekly_gpp_trend + theme(legend.position = "none"),
                        monthly_gpp_trend + theme(legend.position = "none"),
                        nrow = 3,
                        labels = c("A", "B", "C"),
                        hjust = -1,
                        vjust = 1)

plot_legend <- get_legend(
  daily_gpp_trend + 
    guides(color = guide_legend(nrow = 3)) 
)

gpp_trends <- plot_grid(gpp_trends, plot_legend, ncol = 2, rel_widths = c(1, .1))
rm(plot_legend)



# michigan_gpp_trends <- plot_grid(daily_gpp_trend,
#                                  weekly_gpp_trend,
#                                  monthly_gpp_trend,
#                                  nrow = 3)


# # BORDEN ------
# 
# ## Read files ----
# 
# ### Daily fluxes ----
# file <- "data/flux_borden_oneflux/AMF_CA-Cbo_FLUXNET_SUBSET_1994-2020_3-5/AMF_CA-Cbo_FLUXNET_SUBSET_DD_1994-2020_3-5.csv"
# one_flux_borden_daily <- read_flux_file(file = file, timeframe = "daily")
# 
# ### Weekly fluxes ----
# file <- "data/flux_borden_oneflux/AMF_CA-Cbo_FLUXNET_SUBSET_1994-2020_3-5/AMF_CA-Cbo_FLUXNET_SUBSET_WW_1994-2020_3-5.csv"
# one_flux_borden_weekly <- read_flux_file(file = file, timeframe = "weekly")
# 
# ### Monthly fluxes ----
# file <- "data/flux_borden_oneflux/AMF_CA-Cbo_FLUXNET_SUBSET_1994-2020_3-5/AMF_CA-Cbo_FLUXNET_SUBSET_MM_1994-2020_3-5.csv"
# one_flux_borden_monthly <- read_flux_file(file = file, timeframe = "monthly")
# 
# ## Create plots ----
# 
# ### Daily plot ----
# daily_gpp_trend <- one_flux_borden_daily %>%
#   select(date, starts_with("gpp")) %>%
#   pivot_longer(-date, names_to = "gpp_method", values_to = "gpp_value") %>%
#   filter(gpp_value > 0 & gpp_value < 34) %>% 
#   ggplot(aes(x = date, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(-19, 60, by = 4)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux daily Borden GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))
# 
# ### Weekly plot ----
# weekly_gpp_trend  <- one_flux_borden_weekly %>%
#   select(date_start, starts_with("gpp")) %>%
#   pivot_longer(-date_start, names_to = "gpp_method", values_to = "gpp_value") %>%
#   ggplot(aes(x = date_start, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 32, by = 4)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux wekly Borden GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))
# 
# ### Monthly plot ----
# monthly_gpp_trend  <- one_flux_borden_monthly %>%
#   select(date, starts_with("gpp")) %>%
#   pivot_longer(-date, names_to = "gpp_method", values_to = "gpp_value") %>%
#   mutate(date = zoo::as.Date(date)) %>%
#   ggplot(aes(x = date, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 28, by = 4)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux monthly Borden GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))
# 
# ## Export cowplot ----
# borden_gpp_trends <- plot_grid(daily_gpp_trend,
#                                  weekly_gpp_trend,
#                                  monthly_gpp_trend,
#                                  nrow = 3)
# # BARTLETT ------
# 
# ## Read files ----
# 
# ### Daily fluxes ----
# file <-
#   "data/FLX_US-Bar_FLUXNET2015_FULLSET_2005-2017_beta-3/FLX_US-Bar_FLUXNET2015_FULLSET_DD_2005-2017_beta-3.csv"
# one_flux_bartlett_daily <- read_flux_file(file = file, timeframe = "daily")
# 
# ### Weekly fluxes ----
# file <- 
#   "data/FLX_US-Bar_FLUXNET2015_FULLSET_2005-2017_beta-3/FLX_US-Bar_FLUXNET2015_FULLSET_WW_2005-2017_beta-3.csv"
# one_flux_bartlett_weekly <- read_flux_file(file = file, timeframe = "weekly")
# 
# ### Monthly fluxes ----
# file <-
#   "data/FLX_US-Bar_FLUXNET2015_FULLSET_2005-2017_beta-3/FLX_US-Bar_FLUXNET2015_FULLSET_MM_2005-2017_beta-3.csv"
# one_flux_bartlett_monthly <- read_flux_file(file = file, timeframe = "monthly")
# 
# ## Create plots ----
# 
# ### Daily plot ----
# daily_gpp_trend <- one_flux_bartlett_daily %>%
#   select(date, starts_with("gpp")) %>%
#   pivot_longer(-date, names_to = "gpp_method", values_to = "gpp_value") %>%
#   filter(gpp_method %in% c("gpp_dt_vut_25",
#                            "gpp_dt_vut_50",
#                            "gpp_dt_vut_75",
#                            "gpp_dt_vut_ref",
#                            "gpp_nt_vut_25",
#                            "gpp_nt_vut_50",
#                            "gpp_nt_vut_75",
#                            "gpp_nt_vut_ref"
#   )) %>% 
#   ggplot(aes(x = date, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 60, by = 2)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux daily Bartlett GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))
# 
# ### Weekly plot ----
# weekly_gpp_trend  <- one_flux_bartlett_weekly %>%
#   select(date_start, starts_with("gpp")) %>%
#   pivot_longer(-date_start, names_to = "gpp_method", values_to = "gpp_value") %>%
#   filter(gpp_method %in% c("gpp_dt_vut_25",
#                            "gpp_dt_vut_50",
#                            "gpp_dt_vut_75",
#                            "gpp_dt_vut_ref",
#                            "gpp_nt_vut_25",
#                            "gpp_nt_vut_50",
#                            "gpp_nt_vut_75",
#                            "gpp_nt_vut_ref"
#   )) %>% 
#   ggplot(aes(x = date_start, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 32, by = 2)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux wekly Bartlett GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))
# 
# monthly_gpp_trend  <- one_flux_bartlett_monthly %>%
#   select(date, starts_with("gpp")) %>%
#   pivot_longer(-date, names_to = "gpp_method", values_to = "gpp_value") %>%
#   filter(gpp_method %in% c("gpp_dt_vut_25",
#                            "gpp_dt_vut_50",
#                            "gpp_dt_vut_75",
#                            "gpp_dt_vut_ref",
#                            "gpp_nt_vut_25",
#                            "gpp_nt_vut_50",
#                            "gpp_nt_vut_75",
#                            "gpp_nt_vut_ref"
#   )) %>% 
#   mutate(date = zoo::as.Date(date)) %>%
#   ggplot(aes(x = date, y = gpp_value, color = gpp_method)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%b%Y", breaks = "2 months") +
#   scale_y_continuous(breaks = seq(0, 28, by = 2)) +
#   labs(x = "Date", y = expression(GPP~(gC~m^{"-2"}~d^-1)), color = "Index",
#        title = "Oneflux monthly Bartlett GPP") +
#   theme_classic(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 90, h = 1))
# 
# ## Export cowplot ----
# bartlett_gpp_trends <- plot_grid(daily_gpp_trend,
#                                  weekly_gpp_trend,
#                                  monthly_gpp_trend,
#                                  nrow = 3)
