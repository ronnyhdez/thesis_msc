# 0. LOAD PACKAGES ----
library(data.table)
library(dplyr)
library(tictoc)
library(readr)
library(fs)
library(purrr)
library(lubridate)
library(stringr)
library(janitor)

# Internal function
# source("R/import_drive.R")
source("R/ingest_modis_data.R")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%% Michigan %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# For this one I just have the reflectance_500
ingest_modis_reflectance(raw_files_path = "data/gee_michigan_modis_reflectance_500/",
              output_path = "data_satellite_processed", 
              output_file_name = "michigan_modis_reflectance_500_clean.rds",
              ignore_file = "metadata")

ingest_modis_reflectance(raw_files_path = "data/gee_michigan_modis_reflectance_250/",
                         output_path = "data_satellite_processed", 
                         output_file_name = "michigan_modis_reflectance_250_clean.rds",
                         ignore_file = "metadata")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%% Santa Rosa %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ingest_modis_reflectance(raw_files_path = "data/gee_santa_rosa_modis_reflectance_500/",
             output_path = "data_satellite_processed",
             output_file_name = "santa_rosa_modis_reflectance_500_clean.rds",
             ignore_file = "metadata")

ingest_modis_reflectance(raw_files_path = "data/gee_santa_rosa_modis_reflectance_250/",
             output_path = "data_satellite_processed",
             output_file_name = "santa_rosa_modis_reflectance_250_clean.rds",
             ignore_file = "metadata")

ingest_modis_lai(raw_files_path = "data/gee_santa_rosa_modis_lai_500/",
                 output_path = "data_satellite_processed",
                 output_file_name = "santa_rosa_modis_lai_500_clean.rds",
                 ignore_file = "metadata")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%% Borden %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ingest_modis_reflectance(raw_files_path = "data/gee_borden_modis_reflectance_500/",
                         output_path = "data_satellite_processed",
                         output_file_name = "borden_modis_reflectance_500_clean.rds",
                         ignore_file = "metadata")

ingest_modis_reflectance(raw_files_path = "data/gee_borden_modis_reflectance_250",
                         output_path = "data_satellite_processed",
                         output_file_name = "borden_modis_reflectance_250_clean.rds",
                         ignore_file = "metadata")

ingest_modis_lai(raw_files_path = "data/gee_borden_modis_lai_500/",
                 output_path = "data_satellite_processed",
                 output_file_name = "borden_modis_lai_500_clean.rds",
                 ignore_file = "metadata")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%% Bartlett %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ingest_modis_reflectance(raw_files_path = "data/gee_bartlett_modis_reflectance_500/",
                         output_path = "data_satellite_processed",
                         output_file_name = "bartlett_modis_reflectance_500_clean.rds",
                         ignore_file = "metadata")

ingest_modis_reflectance(raw_files_path = "data/gee_bartlett_modis_reflectance_250",
                         output_path = "data_satellite_processed",
                         output_file_name = "bartlett_modis_reflectance_250_clean.rds",
                         ignore_file = "metadata")

# ingest_modis_lai(raw_files_path = "data/gee_borden_modis_lai_500/",
#                  output_path = "data_satellite_processed",
#                  output_file_name = "borden_modis_lai_500_clean.rds",
#                  ignore_file = "metadata")

