# Create Analysis Ready Data for each of the sites.
## This file takes the MODIS data and flux data to create one clean dataset
## per each of the sites.

# Libraries
library(dplyr)
library(readr)
library(janitor)
library(tidyr)
library(stringr)
library(fs)
library(purrr)
library(lubridate)
library(stringr)

# Local scripts
source("R/scale_reflectance_bands.R")
source("R/calculate_indices.R")
source("R/create_bit_string.R")
source("R/filter_quality_pixels.R")
source("R/summarized_indices.R")

## Importing the data

# Three files are read in this document:
#   
#   -   reflectance_500
#   -   one_flux_daily
#   -   one_flux_weekly
#   -   one_flux_monthly


# Satellite data ----
reflectance_500 <- 
  readRDS("data_satellite_processed/michigan_modis_reflectance_500_clean.rds")

# Flux data ----
## AmeriFluxBASE dataset to approximate fAPAR with sw_in and sw_out
file <- "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/AMF_US-UMB_BASE_HH_18-5.csv"
base_flux_hh <- read_csv(file, skip = 2) %>% 
  clean_names() %>% 
  # All NA values are written as -9999
  mutate_all(~na_if(., -9999)) %>% 
  mutate(date = ymd_hm(timestamp_start)) %>% 
  select(-timestamp_start, -timestamp_end) %>% 
  filter(year(date) >= 2015)

## Daily fluxes ----
file <-
  "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/FLX_US-UMB_FLUXNET2015_FULLSET_DD_2007-2017_beta-4.csv"

# Read the file
one_flux_daily <- read_csv(file) %>% 
  clean_names() %>% 
  # All NA values are written as -9999
  mutate_all(~na_if(., -9999)) %>% 
  mutate(date = ymd(timestamp)) %>% 
  select(-timestamp) %>% 
  filter(year(date) >= 2015)

## Weekly fluxes ----
file <- 
  "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/FLX_US-UMB_FLUXNET2015_FULLSET_WW_2007-2017_beta-4.csv"

one_flux_weekly <- read_csv(file) %>% 
  clean_names() %>% 
  # All NA values are written as -9999
  mutate_all(~na_if(., -9999)) %>% 
  mutate(date_start = ymd(timestamp_start),
         date_end = ymd(timestamp_end)) %>% 
  select(-timestamp_start, -timestamp_end) %>%
  filter(year(date_start) >= 2015)

## Monthly fluxes ----
file <-
  "data/FLX_US-UMB_FLUXNET2015_FULLSET_2007-2017_beta-4/FLX_US-UMB_FLUXNET2015_FULLSET_MM_2007-2017_beta-4.csv"

one_flux_monthly <- read_csv(file) %>% 
  clean_names() %>% 
  # All NA values are written as -9999
  mutate_all(~na_if(., -9999)) %>% 
  mutate(day = "01") %>% 
  unite(date, c("timestamp", "day"), sep = "") %>% 
  mutate(date = ymd(date)) %>% 
  mutate(date = zoo::as.yearmon(date)) %>% 
  filter(year(date) >= 2015)

# Data ingestion ----

## Identifying quality pixels ----

## Obtain bitstrings
qc_500_bitstring <- obtain_bit_qc_df(data = reflectance_500, 
                                     variable = "qc_500m",
                                     bits = 32)

state_1km_bitstring <- obtain_bit_qc_df(data = reflectance_500,
                                        variable = "state_1km", 
                                        bits = 16)

## Obtain quality descriptions
qc_500_description <- obtain_qc_bit_description(qc_500_bitstring)
state_1km_description <-  obtain_state_1km_description(state_1km_bitstring)

## Preparing MODIS satellite data ----

## Filter by highest quality categories
reflectance_500 <- filter_modis_pixels_500(data = reflectance_500,
                                           state_1km = state_1km_description,
                                           qc_500 = qc_500_description,
                                           quality = "highest") %>% 
  # Scaling band values
  scale_modis_reflectance_bands() %>% 
  # Indices calculation
  calculate_indices(nir = "sur_refl_b02",
                    red = "sur_refl_b01",
                    blue = "sur_refl_b03",
                    green = "sur_refl_b04")

## Matching dates from satellite and flux towers ----

### Daily ----

# Prepare oneflux daily dataset

## fapar values with NA means that fpar in or out did not recorded 
## anything that date
prepared_one_flux_daily <- one_flux_daily %>% 
  # mutate(fapar = ppfd_in - ppfd_out) %>%
  select(date, gpp_dt_vut_ref, gpp_nt_vut_ref) %>% 
  filter(gpp_dt_vut_ref > params$gpp)

# Prepare base dataset to obtain fapar
prepared_base_flux_daily <- base_flux_hh %>% 
  select(date, sw_in, sw_out) %>% 
  mutate(date = ymd(as.Date(date))) %>% 
  group_by(date) %>% 
  summarise(
    sw_in_mean = mean(sw_in, na.rm = TRUE),
    sw_out_mean = mean(sw_out, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(fapar = 1 - (sw_out_mean / sw_in_mean)) %>% 
  select(date, fapar)

# Prepare reflectance datasets

## 500m summary
daily_reflectance_500 <- summarise_indices(reflectance_dataset = reflectance_500, 
                                           summary_date = "daily",
                                           resolution = 500,
                                           sensor = "MODIS")

# Join reflectance dataset with oneflux

## 500m join
daily_500 <- prepared_one_flux_daily %>% 
  inner_join(daily_reflectance_500, by =  "date") %>% 
  # Include the new base dataset with fapar calculated.
  left_join(prepared_base_flux_daily, by = "date")

# Remove objects
rm(prepared_one_flux_daily, 
   prepared_base_flux_daily,
   daily_reflectance_500)

### Weekly ----

# Prepare oneflux daily dataset

## Check weeks in one flux weekly
prepared_one_flux_weekly <- one_flux_weekly %>% 
  # mutate(fapar = ppfd_in - ppfd_out) %>% 
  filter(gpp_dt_vut_ref > params$gpp) %>% 
  mutate(week_start = week(date_start),
         week_end = week(date_end),
         year = year(date_start)) %>% 
  mutate(same_week_test = week_start - week_end)

## Test to check that all the dates (start and end) correspond to the same week.
test <- prepared_one_flux_weekly %>% 
  filter(same_week_test > 0)

stopifnot(nrow(test) == 0)
rm(test)

# Prepare base dataset to obtain fapar
prepared_base_flux_weekly <- base_flux_hh %>% 
  select(date, sw_in, sw_out) %>% 
  mutate(date = ymd(as.Date(date)),
         week_start = week(date),
         year = year(date)) %>% 
  group_by(week_start, year) %>%
  summarise(
    sw_in_mean = mean(sw_in, na.rm = TRUE),
    sw_out_mean = mean(sw_out, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(fapar = 1 - (sw_out_mean / sw_in_mean)) %>% 
  select(week_start, year, fapar)

# Prepare reflectance datasets

## 500m summary
weekly_reflectance_500 <- summarise_indices(reflectance_dataset = reflectance_500, 
                                            summary_date = "weekly",
                                            resolution = 500,
                                            sensor = "MODIS")

# Join reflectance dataset with oneflux

## 500m join
weekly_500 <- prepared_one_flux_weekly %>% 
  inner_join(weekly_reflectance_500, by = c("week_start" = "week", "year")) %>% 
  left_join(prepared_base_flux_weekly, by = (c("week_start", "year")))

# Remove objects
rm(weekly_reflectance_500,
   prepared_one_flux_weekly,
   prepared_base_flux_weekly)

### Monthly ----

# Prepare oneflux daily dataset
prepared_one_flux_monthly <- one_flux_monthly %>% 
  # mutate(fapar = ppfd_in - ppfd_out) %>% 
  filter(gpp_dt_vut_ref > params$gpp) 

# Prepare base dataset to obtain fapar
prepared_base_flux_monthly <- base_flux_hh %>% 
  select(date, sw_in, sw_out) %>% 
  mutate(date = ymd(as.Date(date)),
         date = zoo::as.yearmon(date)) %>% 
  group_by(date) %>% 
  summarise(
    sw_in_mean = mean(sw_in, na.rm = TRUE),
    sw_out_mean = mean(sw_out, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(fapar = 1 - (sw_out_mean / sw_in_mean)) %>% 
  select(date, fapar)

# Prepare reflectance datasets

## 500m summary
monthly_reflectance_500 <- summarise_indices(reflectance_dataset = reflectance_500, 
                                             summary_date = "monthly",
                                             resolution = 500,
                                             sensor = "MODIS")

# Join reflectance dataset with oneflux

## 500m join
monthly_500 <- prepared_one_flux_monthly %>% 
  inner_join(monthly_reflectance_500, by = "date") %>% 
  left_join(prepared_base_flux_monthly, by = "date")

# Remove objects
rm(prepared_one_flux_monthly,
   monthly_reflectance_500)

## Export analysis ready datasets ----

# Export prepared datasets for poster plotting
if (Sys.getenv("ENV") == "DEV") {
  monthly_500 %>% 
    mutate(date = as.Date(date)) %>% 
    saveRDS("data_ard/michigan_montly_500.rds")

  saveRDS(weekly_500, "data_ard/michigan_weekly_500.rds")
  saveRDS(daily_500, "data_ard/michigan_daily_500.rds")
}
