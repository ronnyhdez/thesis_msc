
#' @title Calculate indices 
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the reflectance bands from the MODIS
#' datasets and calculate the NDVI, EVI and NIRv.
#' 
#' @details Dataset should have the reflectance bands scaled 
#' 
#' MODIS bands are as follows:
#'    - nir = sur_refl_b02
#'    - red = sur_refl_b01
#'    - blue = sur_refl_b03
#' 
#' 
#' @param data Dataset with the reflectance bands to calculate the indices.
#' @param nir Near Infrared band.
#' @param red Red band.
#' @param blue Blue band.
#' 
#' @example 
#' \dontrun{
#' calculate_indices(reflectance_500, 
#'                  nir = "sur_refl_b02",
#'                  red = "sur_refl_b01",
#'                  blue = "sur_refl_b03",
#'                  green = "sur_refl_b04")
#'}
#'
calculate_indices <- function(data, nir, red, 
                              green = "none", blue = "none") {
  
  if (!nir %in% names(data)) {
    stop("NIR band is not in the dataset")
  }
  
  if (!red %in% names(data)) {
    stop("RED band is not in the dataset")
  }
  
  data_with_index <- data %>%
    dplyr::mutate(ndvi = (.data[[nir]] - .data[[red]]) / (.data[[nir]] + .data[[red]])) %>%
    dplyr::mutate(nirv = .data[[nir]] * (ndvi)) %>% 
    # dplyr::mutate(sigma_kndvi = 0.5*(.data[[nir]] + .data[[red]])) %>% 
    # dplyr::mutate(kndvi = tanh(((.data[[nir]] - .data[[red]]) / (2*sigma_kndvi))^2)) %>% 
    dplyr::mutate(kndvi = tanh((ndvi^2))) 
  
  if (blue %in% names(data)) {
    data_with_index <- data_with_index  %>%
      dplyr::mutate(
        evi = 2.5 * ((.data[[nir]] - .data[[red]]) /
                       (.data[[nir]] + 5 * .data[[red]] - 7.5 * .data[[blue]] + 1))
      )
  } else if (!blue %in% names(data)) {
    print("Blue band is not in the dataset. EVI was not calculated")
  }
  
  if (green %in% names(data)) {
    data_with_index <- data_with_index  %>%
      dplyr::mutate(cci = (.data[[green]] - .data[[red]]) / (.data[[green]] + .data[[red]]))
  } else if (!green %in% names(data)) {
    print("Green band is not in the dataset. CCI was not calculated")
  }
  
  return(data_with_index)
}

#' @title Calculate Sentinel Harmonized indices 
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description The function will take the reflectance bands from the Sentinel
#' Harmonized datasets and calculate the NDVI, EVI and NIRv.
#' 
#' @details Dataset should have the reflectance bands scaled 
#' 
#' Harmonized Sentinel-2 MSI bands are as follows:
#'    - nir = b8
#'    - red = b4
#'    - blue = b2
#' 
#' 
#' @param data Dataset with the reflectance bands to calculate the indices.
#' @param nir Near Infrared band.
#' @param red Red band.
#' @param blue Blue band.
#' 
#' @example 
#' \dontrun{
#' calculate_sentinel_indices(reflectance_500, 
#'                   nir = "b8",
#'                   red = "b4",
#'                   blue = "b2")
#'}
#'
calculate_sentinel_indices <- function(data, nir, red, blue = "none") {
  
  if (!nir %in% names(data)) {
    stop("NIR band is not in the dataset")
  }
  
  if (!red %in% names(data)) {
    stop("RED band is not in the dataset")
  }
  
  data_with_index <- data %>%
    dplyr::mutate(ndvi = (.data[[nir]] - .data[[red]]) / (.data[[nir]] + .data[[red]])) %>%
    dplyr::mutate(nirv = .data[[nir]] * (ndvi))
  
  # EVI (Sentinel 2) = 2.5 * ((B8 – B4) / (B8 + 6 * B4 – 7.5 * B2 + 1)).
  
  if (blue %in% names(data)) {
    data_with_index <- data_with_index  %>%
      dplyr::mutate(
        evi = 2.5 * ((.data[[nir]] - .data[[red]]) /
                       (.data[[nir]] + 6 * .data[[red]] - 7.5 * .data[[blue]] + 1))
      )
  } else if (!blue %in% names(data)) {
    print("Blue band is not in the dataset. EVI was not calculated")
  }
  
  return(data_with_index)
}

