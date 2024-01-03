#' @title Scale modis reflectance bands
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the reflectance bands from the MODIS
#' datasets and scale them.
#' 
#' @details Dataset should have the reflectance bands names starting with the
#' "sur" word. According to the documentation:
#' 
#'  - Values should be between the ranges described in MODIS documentation: 
#'    100 & 16000
#'  - Scaling value is 0.0001
#'  - After scaling, values should be between 0 and 1. 
#'  - Values outside that range are considered fill values. 
#' 
#' @param data MODIS dataset to apply the scaling.
#' 
#' @example 
#' \dontrun{
#' scale_modis_reflectance_bands(reflectance_500) 
#'}
#'
scale_modis_reflectance_bands <- function(data) {
  scaled_data <- data %>%
    mutate_at(vars(starts_with("sur")), ~ . * 0.0001)
  
  return(scaled_data)
}

#' @title Scale Harmonized Sentinel-2 MSI reflectance bands
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the reflectance bands from the 
#' Harmonized Sentinel-2 MSI datasets and scale them.
#' 
#' @details Dataset should have the reflectance bands names starting with the
#' "b" letter. According to the [documentation](https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_SR_HARMONIZED#bands):
#' 
#'  - Scaling value is 0.0001
#'  - After scaling, values should be between 0 and 1. 
#'  - Values outside that range are considered fill values. 
#' 
#' @param data Harmonized Sentinel-2 MSI dataset to apply the scaling.
#' 
#' @example 
#' \dontrun{
#' scale_sentinel_reflectance_bands(reflectance_500) 
#'}
#'
scale_sentinel_reflectance_bands <- function(data) {
  scaled_data <- data %>%
    mutate_at(vars(starts_with("b")), ~ . * 0.0001)
  
  return(scaled_data)
}

