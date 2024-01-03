#' @import dplyr
NULL

#' @title Transform bit strings
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description Takes a variable and the number of bits necessary to proced
#' with the creationg of the bit string for MODIS data.
#' 
#' @details It works if the the reflectance_500 data object is loaded in the
#' session.
#' 
#' @param variable variable from the dataset to obtain the bit string.
#' @param bits Number of bits according to MODIS documentation for the
#' variable of interest.
#' 
#' @example 
#' \dontrun{
#' obtain_bit_qc_df(data = "reflectance_500", variable = "state_1km", bits = 16)
#'}
#'
#' @example 
#' \dontrun{
#'obtain_bit_qc_df(data = reflectance_500_bits, 
#'                 variable = "qc_500m",
#'                 bits = 32)
#'}
#'
obtain_bit_qc_df <- function(data, variable, bits) {
  qc_data <- data %>% 
    select(variable) %>% 
    distinct() %>% 
    rename(qc_ints = .data[[variable]]) %>% 
    bind_rows(tibble(qc_ints = 1131675649)) %>% 
    as.data.frame()
  
  ## Create empty data frame. This case is 32 given that the variable
  ## `qc_500m` have 32 bits
  total_bits = bits - 1
  for (i in c(total_bits:0)) {
    qc_data[, paste0("bit_", i)] <- NA
  }
  
  # Loop for obtaining bit string from unique values in the variable
  bit_col <- bits + 1
  z <- 1
  for (i in qc_data$qc_ints) {
    # print(paste(i))
    transformed <- as.integer(intToBits(i)[1:bits])
    qc_data[z, 2:bit_col] <- transformed[bits:1]
    z <- z + 1
  }
  
  ## Create test according to value in video
  final_bit <- paste0("bit_", total_bits)
  
  process_test <- qc_data %>% 
    filter(qc_ints == 1131675649) %>%
    select(-qc_ints) %>%
    tidyr::unite(col = "new", everything(), sep = "") %>% 
    pull() 
  
  test_object <- stringr::str_sub("01000011011101000000000000000001",
                                  start = -bits, end = -1)
  
  stopifnot(process_test == test_object) 
  
  return(qc_data)
}

#' @title Obtain the qc_500m data description
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will take the bit string conversion dataset (with the
#' bits variables) and include the quality categories according to the MODIS
#' documentation.
#' 
#' @details It needs the data from the bit string conversion for the variable
#' qc_500 that comes from the reflectance MODIS dataset with a resolution of
#' 500m
#' 
#' @param data variable from the dataset to obtain the bit string.
#' 
#' @example 
#' \dontrun{
#' qc_500_description <- obtain_qc_bit_description(qc_data)
#'}
#'
obtain_qc_bit_description <- function(data) {
  qc_500_description <- data %>% 
    tidyr::unite(col = "modland", c("bit_1", "bit_0"), sep = "") %>% 
    tidyr::unite(col = "band_1", 
          c("bit_5", "bit_4", "bit_3", "bit_2"), sep = "") %>% 
    tidyr::unite(col = "band_2", 
          c("bit_9", "bit_8", "bit_7", "bit_6"), sep = "") %>% 
    tidyr::unite(col = "band_3", 
          c("bit_13", "bit_12", "bit_11", "bit_10"), sep = "") %>% 
    tidyr::unite(col = "band_4", 
          c("bit_17", "bit_16", "bit_15", "bit_14"), sep = "") %>% 
    tidyr::unite(col = "band_5", 
          c("bit_21", "bit_20", "bit_19", "bit_18"), sep = "") %>% 
    tidyr::unite(col = "band_6", 
          c("bit_25", "bit_24", "bit_23", "bit_22"), sep = "") %>%
    tidyr::unite(col = "band_7", 
          c("bit_29", "bit_28", "bit_27", "bit_26"), sep = "") %>% 
    dplyr::mutate(modland_qa = dplyr::case_when(
      modland == "00" ~ "ideal quality - all bands",
      modland == "01" ~ "less than ideal quality - some or all bands",
      modland == "10" ~ "product not produced due to cloud effects",
      modland == "11" ~ "product not produced for other reasons",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band1_qa = dplyr::case_when(
      band_1 == "0000" ~ "highest_quality",
      band_1 == "0111" ~ "noisy detector",
      band_1 == "1000" ~ "dead detector, data interpolated in L1B",
      band_1 == "1001" ~ "solar zenith >= 86 degrees",
      band_1 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_1 == "1011" ~ "missing input",
      band_1 == "1100" ~ "internal constant used",
      band_1 == "1101" ~ "correction out of bounds",
      band_1 == "1110" ~ "L1B data faulty",
      band_1 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band2_qa = dplyr::case_when(
      band_2 == "0000" ~ "highest_quality",
      band_2 == "0111" ~ "noisy detector",
      band_2 == "1000" ~ "dead detector, data interpolated in L1B",
      band_2 == "1001" ~ "solar zenith >= 86 degrees",
      band_2 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_2 == "1011" ~ "missing input",
      band_2 == "1100" ~ "internal constant used",
      band_2 == "1101" ~ "correction out of bounds",
      band_2 == "1110" ~ "L1B data faulty",
      band_2 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band3_qa = dplyr::case_when(
      band_3 == "0000" ~ "highest_quality",
      band_3 == "0111" ~ "noisy detector",
      band_3 == "1000" ~ "dead detector, data interpolated in L1B",
      band_3 == "1001" ~ "solar zenith >= 86 degrees",
      band_3 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_3 == "1011" ~ "missing input",
      band_3 == "1100" ~ "internal constant used",
      band_3 == "1101" ~ "correction out of bounds",
      band_3 == "1110" ~ "L1B data faulty",
      band_3 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band4_qa = dplyr::case_when(
      band_4 == "0000" ~ "highest_quality",
      band_4 == "0111" ~ "noisy detector",
      band_4 == "1000" ~ "dead detector, data interpolated in L1B",
      band_4 == "1001" ~ "solar zenith >= 86 degrees",
      band_4 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_4 == "1011" ~ "missing input",
      band_4 == "1100" ~ "internal constant used",
      band_4 == "1101" ~ "correction out of bounds",
      band_4 == "1110" ~ "L1B data faulty",
      band_4 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band5_qa = dplyr::case_when(
      band_5 == "0000" ~ "highest_quality",
      band_5 == "0111" ~ "noisy detector",
      band_5 == "1000" ~ "dead detector, data interpolated in L1B",
      band_5 == "1001" ~ "solar zenith >= 86 degrees",
      band_5 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_5 == "1011" ~ "missing input",
      band_5 == "1100" ~ "internal constant used",
      band_5 == "1101" ~ "correction out of bounds",
      band_5 == "1110" ~ "L1B data faulty",
      band_5 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band6_qa = dplyr::case_when(
      band_6 == "0000" ~ "highest_quality",
      band_6 == "0111" ~ "noisy detector",
      band_6 == "1000" ~ "dead detector, data interpolated in L1B",
      band_6 == "1001" ~ "solar zenith >= 86 degrees",
      band_6 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_6 == "1011" ~ "missing input",
      band_6 == "1100" ~ "internal constant used",
      band_6 == "1101" ~ "correction out of bounds",
      band_6 == "1110" ~ "L1B data faulty",
      band_6 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band7_qa = dplyr::case_when(
      band_7 == "0000" ~ "highest_quality",
      band_7 == "0111" ~ "noisy detector",
      band_7 == "1000" ~ "dead detector, data interpolated in L1B",
      band_7 == "1001" ~ "solar zenith >= 86 degrees",
      band_7 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_7 == "1011" ~ "missing input",
      band_7 == "1100" ~ "internal constant used",
      band_7 == "1101" ~ "correction out of bounds",
      band_7 == "1110" ~ "L1B data faulty",
      band_7 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(atmospheric_correction = ifelse(bit_30 == 0, "no", "yes"),
           adjacency_correction = ifelse(bit_31 == 0, "no", "yes"))
  
  return(qc_500_description)
}

#' @title Obtain the state_1km data description
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will take the bit string conversion dataset (with the
#' bits variables) and include the quality categories according to the MODIS
#' documentation.
#' 
#' @details It needs the data from the bit string conversion for the variable
#' state_1km that comes from the reflectance MODIS dataset with a resolution of
#' 500m
#' 
#' @param data variable from the dataset to obtain the bit string.
#' 
#' @example 
#' \dontrun{
#' state_1km_description <- obtain_state_1km_description(qc_data)
#'}
#'
obtain_state_1km_description <- function(data) {
  state_1km_description <- data %>% 
    tidyr::unite(col = "cloud_state", c("bit_1", "bit_0"), sep = "") %>% 
    tidyr::unite(col = "land_water_flag",
          c("bit_5", "bit_4", "bit_3"), sep = "") %>%
    tidyr::unite(col = "aerosol_quantity",
          c("bit_7", "bit_6"), sep = "") %>%
    tidyr::unite(col = "cirrus_detected",
          c("bit_9", "bit_8"), sep = "") %>%
    dplyr::mutate(cloud_state_qa = dplyr::case_when(
      cloud_state == "00" ~ "clear",
      cloud_state == "01" ~ "cloudy",
      cloud_state == "10" ~ "mixed",
      cloud_state == "11" ~ "not set, assumed clear",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(cloud_shadow_qa = ifelse(bit_2 == 1, "yes", "no")) %>% 
    dplyr::mutate(land_water_qa = dplyr::case_when(
      land_water_flag == "000" ~ "shallow ocean",
      land_water_flag == "001" ~ "land",
      land_water_flag == "010" ~ "ocean coastlines and lake shorelines",
      land_water_flag == "011" ~ "shallow inland water",
      land_water_flag == "100" ~ "ephemeral water",
      land_water_flag == "101" ~ "deep inland water",
      land_water_flag == "110" ~ "continental/moderate ocean",
      land_water_flag == "111" ~ "deep ocean",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(aerosol_quantity_qa = dplyr::case_when(
      aerosol_quantity == "00" ~ "climatology",
      aerosol_quantity == "01" ~ "low",
      aerosol_quantity == "10" ~ "average",
      aerosol_quantity == "11" ~ "high",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(cirrus_detected_qa = dplyr::case_when(
      cirrus_detected == "00" ~ "none",
      cirrus_detected == "01" ~ "small",
      cirrus_detected == "10" ~ "average",
      cirrus_detected == "11" ~ "high",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(cloud_flag_qa = ifelse(bit_10 == 1, "cloud", "no cloud"),
           fire_flag_qa = ifelse(bit_11 == 1, "fire", "no fire"),
           snow_ice_flag_qa = ifelse(bit_12 == 1, "yes", "no"),
           pixel_adjacent_cloud_qa = ifelse(bit_13 == 1, "yes", "no"),
           salt_pan_qa = ifelse(bit_14 == 1, "yes", "no"),
           snow_mask_qa = ifelse(bit_15 == 1, "yes", "no"))
  
  return(state_1km_description)
}


#' @title Obtain the q_scan data description
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will take the bit string conversion dataset (with the
#' bits variables) and include the quality categories according to the MODIS
#' documentation.
#' 
#' @details It needs the data from the bit string conversion for the variable
#' q_scan that comes from the reflectance MODIS dataset with a resolution of
#' 500m
#' 
#' @param data variable from the dataset to obtain the bit string.
#' 
#' @example 
#' \dontrun{
#' q_scan_description <- obtain_q_scan_description(qc_data)
#'}
#'
obtain_q_scan_description <- function(data){
  q_scan_description <- data %>% 
    dplyr::mutate(scan_quadrant_1 = ifelse(bit_0 == 1, "yes", "no"),
           scan_quadrant_2 = ifelse(bit_0 == 1, "yes", "no"),
           scan_quadrant_3 = ifelse(bit_0 == 1, "yes", "no"),
           scan_quadrant_4 = ifelse(bit_0 == 1, "yes", "no"),
           missing_obs_1 = ifelse(bit_1 == 1, "same", "different"),
           missing_obs_2 = ifelse(bit_1 == 1, "same", "different"),
           missing_obs_3 = ifelse(bit_1 == 1, "same", "different"),
           missing_obs_4 = ifelse(bit_1 == 1, "same", "different"))
  
  return(q_scan_description)
}


#' @title Obtain the qc_250 data description
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This will take the bit string conversion dataset (with the
#' bits variables) and include the quality categories according to the MODIS
#' documentation.
#' 
#' @details It needs the data from the bit string conversion for the variable
#' qc_250 that comes from the reflectance MODIS dataset with a resolution of
#' 250m
#' 
#' @param data variable from the dataset to obtain the bit string.
#' 
#' @example 
#' \dontrun{
#' qc_250_description <- obtain_qc_250_description(qc_data)
#'}
#'
obtain_qc_250_description <- function(data) {
  qc_250_description <- data %>% 
    tidyr::unite(col = "moodland", 
          c("bit_1", "bit_0"), sep = "") %>% 
    tidyr::unite(col = "band_1",
          c("bit_7", "bit_6", "bit_5", "bit_4"), sep = "") %>%
    tidyr::unite(col = "band_2",
          c("bit_11", "bit_10", "bit_9", "bit_8"), sep = "") %>%
    mutate(modland_qa = dplyr::case_when(
      moodland == "00" ~ "ideal_quality",
      moodland == "01" ~ "less_than_ideal_quality",
      moodland == "10" ~ "cloud_effects",
      moodland == "11" ~ "fill_value",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band1_qa = dplyr::case_when(
      band_1 == "0000" ~ "highest_quality",
      band_1 == "0111" ~ "noisy detector",
      band_1 == "1000" ~ "dead detector, data interpolated in L1B",
      band_1 == "1001" ~ "solar zenith >= 86 degrees",
      band_1 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_1 == "1011" ~ "missing input",
      band_1 == "1100" ~ "internal constant used",
      band_1 == "1101" ~ "correction out of bounds",
      band_1 == "1110" ~ "L1B data faulty",
      band_1 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(band2_qa = dplyr::case_when(
      band_2 == "0000" ~ "highest_quality",
      band_2 == "0111" ~ "noisy detector",
      band_2 == "1000" ~ "dead detector, data interpolated in L1B",
      band_2 == "1001" ~ "solar zenith >= 86 degrees",
      band_2 == "1010" ~ "solar zenith >= 85 and < 86 degrees",
      band_2 == "1011" ~ "missing input",
      band_2 == "1100" ~ "internal constant used",
      band_2 == "1101" ~ "correction out of bounds",
      band_2 == "1110" ~ "L1B data faulty",
      band_2 == "1111" ~ "not processed due to deep ocean or clouds",
      TRUE ~ "No info, please validate bit conversion"
    )) %>% 
    dplyr::mutate(atmospheric_correction = ifelse(bit_12 == 0, "no", "yes"),
           adjacency_correction = ifelse(bit_13 == 0, "no", "yes"))
  
  return(qc_250_description)
}