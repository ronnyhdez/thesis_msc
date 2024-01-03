
#' @title  Filter the MODIS pixels 500m by the highest quality pixels
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the reflectance 500m dataset and the
#' bitstring categories to filter out bad quality pixels
#' 
#' @details Dataset should have the reflectance dataset and the bitstring
#' categories datasets
#' 
#' @param data Dataset with the reflectance values with 500m resolution.
#' @param state_1km The `state_1km` variable with the quality descriptions.
#' @param qc_500 The `qc_500` variable with the quality descriptions.
#' @param quality If we want to filter out by the highest quality categories.
#' 
#' @example 
#' \dontrun{
#' filter_modis_pixels_500(data = reflectance_500,
#'                         state_1km = state_1km_description,
#'                         qc_500 = qc_500_description,
#'                         quality = "highest")
#'}
#'
filter_modis_pixels_500 <- function(data, state_1km, qc_500, quality = "highest") {
  
  if (quality == "highest") {
    # quality values from state_1km
    # bit 6-7 and bit 13 can be anything as regarded by richard
    state_1km_highest_quality <- state_1km %>%
      dplyr::filter(cloud_state_qa == "clear") %>% 
      dplyr::filter(cloud_shadow_qa == "no") %>%
      dplyr::filter(land_water_qa == "land") %>% 
      dplyr::filter(cirrus_detected_qa == "none") %>% 
      dplyr::filter(bit_10 == "0") %>% 
      dplyr::filter(fire_flag_qa == "no fire") %>% 
      dplyr::filter(snow_ice_flag_qa == "no") %>% 
      dplyr::filter(bit_14 == "0") %>% 
      dplyr::filter(bit_15 == "0") %>% 
      dplyr::select(qc_ints) %>% 
      dplyr::pull()
    
    # quality values from qc_500
    # All should be 0 for all of the bits, except for 
    qc_500_highest_quality <- qc_500 %>%
      dplyr::filter(modland_qa == "ideal quality - all bands") %>%
      dplyr::filter(band1_qa == "highest_quality") %>%
      dplyr::filter(band2_qa == "highest_quality") %>%
      dplyr::filter(band3_qa == "highest_quality") %>%
      dplyr::filter(band4_qa == "highest_quality") %>%
      dplyr::filter(band5_qa == "highest_quality") %>%
      dplyr::filter(band6_qa == "highest_quality") %>%
      dplyr::filter(band7_qa == "highest_quality") %>%
      dplyr::filter(atmospheric_correction == "yes") %>%
      # dplyr::filter(adjacency_correction == "yes") %>% 
      dplyr::select(qc_ints) %>% 
      dplyr::pull()
  }
  
  data_filtered <- data %>% 
    dplyr::filter(state_1km %in% state_1km_highest_quality) %>% 
    dplyr::filter(qc_500m %in% qc_500_highest_quality)
}

#' @title  Filter the MODIS pixels 250m by the highest quality pixels
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the reflectance 250m dataset and the
#' bitstring categories to filter out bad quality pixels
#' 
#' @details Dataset should have the reflectance dataset and the bitstring
#' categories datasets
#' 
#' @param data Dataset with the reflectance values with 500m resolution.
#' @param qc_250 The `qc_250` variable with the quality descriptions.
#' @param quality If we want to filter out by the highest quality categories.
#' 
#' @example 
#' \dontrun{
#' filter_modis_pixels_500(data = reflectance_250,
#'                         qc_250 = qc_250_description,
#'                         quality = "highest")
#'}
#'
filter_modis_pixels_250 <- function(data, qc_250, quality = "highest") {
  
  if (quality == "highest") {
    # quality values from qc_250
    qc_250_highest_quality <- qc_250 %>%
      dplyr::filter(modland_qa == "ideal_quality") %>% 
      dplyr::filter(band1_qa == "highest_quality") %>%
      dplyr::filter(band2_qa == "highest_quality") %>% 
      dplyr::filter(atmospheric_correction == "yes") %>% 
      dplyr::select(qc_ints) %>% 
      dplyr::pull()
    
    data_filtered <- data %>% 
      dplyr::filter(qc_250m %in% qc_250_highest_quality)
  }
}


