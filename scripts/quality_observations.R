# Quality pixels filtering
## Script to create plots with quality label in each of the observations

# Libraries and 
library(dplyr)
library(tidyr)
library(ggplot2)
source("R/scale_reflectance_bands.R")
source("R/create_bit_string.R")
source("R/calculate_indices.R")
source("R/filter_quality_pixels.R")

# Bartlett
reflectance_500 <- 
  readRDS("data_satellite_processed/bartlett_modis_reflectance_500_clean.rds") %>% 
  mutate(id = row_number())

## Identifying quality pixels
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

## Preparing MODIS satellite data
## Filter by highest quality categories
reflectance_500_filtered <- filter_modis_pixels_500(data = reflectance_500,
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

## Join Bartlett observations
rf <- reflectance_500_filtered %>% 
  select(id) %>% 
  mutate(quality = "high")

all_bartlett <- reflectance_500 %>% 
  select(id, starts_with("sur"), date) %>% 
  left_join(rf, by = "id") %>% 
  mutate(quality = replace_na(quality, "other"),
         site = "Bartlett")

# Borden ----
reflectance_500 <- 
  readRDS("data_satellite_processed/borden_modis_reflectance_500_clean.rds") %>% 
  mutate(id = row_number())

## Identifying quality pixels
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

## Preparing MODIS satellite data
## Filter by highest quality categories
reflectance_500_filtered <- filter_modis_pixels_500(data = reflectance_500,
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

## Join Borden observations
rf <- reflectance_500_filtered %>% 
  select(id) %>% 
  mutate(quality = "high")

all_borden <- reflectance_500 %>% 
  select(id, starts_with("sur"), date) %>% 
  left_join(rf, by = "id") %>% 
  mutate(quality = replace_na(quality, "other"),
         site = "Borden")

# Michigan ----
reflectance_500 <- 
  readRDS("data_satellite_processed/michigan_modis_reflectance_500_clean.rds") %>% 
  mutate(id = row_number())

## Identifying quality pixels
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


## Preparing MODIS satellite data
## Filter by highest quality categories
reflectance_500_filtered <- filter_modis_pixels_500(data = reflectance_500,
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
## Join Michigan observations
rf <- reflectance_500_filtered %>% 
  select(id) %>% 
  mutate(quality = "high")

all_michigan <- reflectance_500 %>% 
  select(id, starts_with("sur"), date) %>% 
  left_join(rf, by = "id") %>% 
  mutate(quality = replace_na(quality, "other"),
         site = "Michigan")

# Plot
## Join all 3 sites datasets with observations classified according to quality ----
all <- bind_rows(all_bartlett, all_borden, all_michigan)

## Remove all unnecesary objects
rm(all_bartlett, all_borden, all_michigan,
   qc_500_bitstring, qc_500_description,
   reflectance_500, reflectance_500_filtered,
   rf, state_1km_bitstring, state_1km_description)

## Stacked bar plot for the 3 sites
all %>% 
  ggplot(aes(x = site, fill = quality)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d(begin = 0.34, end = 0.8) +
  labs(x = "Site", 
       y = "Total observations (pixels)",
       fill = "Quality") +
  theme_bw(base_size = 12)

## Number of observations per month per site
all %>% 
  filter(quality == "high") %>% 
  mutate(year_mon = zoo::as.yearmon(date)) %>% 
  ggplot(aes(x = year_mon, fill = site)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(x = "Date", 
       y = "Total observations (pixels)",
       fill = "Site") +
  theme_bw(base_size = 12)



