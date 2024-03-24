#' @import dplyr
#' @import data.table
NULL

#' @title Ingest MODIS reflectance data 
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description It takes the folder containing the csv files downloaded from
#' Google Drive (exported from GEE) and parse into one RDS object all the files.
#' 
#' @details This is designed to parsed, clean variable names, write the NAs
#' into one RDS object that will be exported for the reflectance datasets with
#' an spatial resolution of 250m and 500m
#' 
#' @param raw_files_path Folder containing the `.csv` files with the MODIS info.
#' @param output_path Folder where the processed data should be save. Do not 
#' include the trailing slash `/`.
#' @param output_file_name The name you want to give to the final `.rds` file
#' @param ignore_file Optional. If there is a metadata file that should be 
#' ignored when creating the final file, this should be indicated here.
#'
#' @example 
#' \dontrun{
#' ingest_modis_reflectance(raw_files_path = "data_test_temporal/more_folders/", 
#'              output_path = "test", 
#'              output_file_name = "test.rds",
#'              ignore_file = "metadata")
#'}
#'
ingest_modis_reflectance <- function(raw_files_path, output_path, output_file_name,
						 ignore_file = "metadata") {
	
  # Paths validation
  ## Existing path
  if (fs::dir_exists(raw_files_path) == FALSE) {
    stop(paste(raw_files_path, "is not a valid path"))
  }
  
  ## Output path
  if (!fs::dir_exists(output_path)) {
    fs::dir_create(output_path)
  }
  
  # Create file names pattern to be read
  glob_pattern <- paste0(raw_files_path, "*")

	files <- fs::dir_ls(path = raw_files_path,
	                    glob = glob_pattern)
	
	# Ignore files to be read
	if (ignore_file == "metadata") {
	  file_to_ignore  <- files[stringr::str_detect(files, "metadata")]
	  
	  # Validate if file to ignore exists
	  if (!rlang::is_empty(file_to_ignore) == TRUE) {
	    files <- files[files != file_to_ignore]
	    print(paste("The file to be ignore when reading is:", file_to_ignore))
	  }
	  
	} else if (ignore_file != "metadata") {
	  file_to_ignore <- files[stringr::str_detect(files, ignore_file)]
	  
	  # Validate if file to ignore exists
	  if (!rlang::is_empty(file_to_ignore) == TRUE) {
	    files <- files[files != file_to_ignore]
	    print(paste("The file to be ignore when reading is:", file_to_ignore))
	  }
	}
	
	pixels_sr <- files %>%
	  purrr::map_dfr(readr::read_csv, .id = "file_id") %>%
	  janitor::clean_names()
	
	### 2. CLEAN DATASET ----
	data.table::setDT(pixels_sr)
	pixels_sr[, c("folder_1", "folder_2", "file_id") := tstrsplit(file_id, "/", fixed = TRUE)]
	pixels_sr[ , ":="(file_id = stringr::str_remove(file_id, ".csv"))]
	pixels_sr[ , ":="(location = stringr::str_extract(geo, "\\[(.*?)\\]"))]
	pixels_sr[,  c("lat", "long") := tstrsplit(location, ",", fixed = TRUE)]
	pixels_sr[ , ":="(lat = stringr::str_extract(lat, "-?[0-9.]+"),
	                  long = stringr::str_extract(long, "-?[0-9.]+"))]
	pixels_sr[, ":="(date = lubridate::ymd(file_id))]
	pixels_sr[, c("file_id", "system_index", 
	              "folder_1", "folder_2",
	              "geo", "location") := NULL]
	
	### 3. EXPORT OBJECT ----
	output_path <- paste0(output_path, "/", output_file_name)
	saveRDS(object = pixels_sr, file = output_path)
	print(paste("Your processed data was saved in", output_path))
}

#' @title Ingest MODIS lai data 
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description It takes the folder containing the csv files downloaded from
#' Google Drive (exported from GEE) and parse into one RDS object all the files.
#' 
#' @details This is designed to parsed, clean variable names, write the NAs
#' into one RDS object that will be exported for the lai datasets with
#' an spatial resolution of 500m
#' 
#' @param raw_files_path Folder containing the `.csv` files with the MODIS lai info.
#' @param output_path Folder where the processed data should be save. Do not 
#' include the trailing slash `/`.
#' @param output_file_name The name you want to give to the final `.rds` file
#' @param ignore_file Optional. If there is a metadata file that should be 
#' ignored when creating the final file, this should be indicated here.
#'
#' @example 
#' \dontrun{
#' ingest_modis_lai(raw_files_path = "data_test_temporal/more_folders/", 
#'              output_path = "test", 
#'              output_file_name = "test.rds",
#'              ignore_file = "metadata")
#'}
#'
ingest_modis_lai <- function(raw_files_path, output_path, output_file_name,
                             ignore_file = "metadata") {
  
  # Paths validation
  ## Existing path
  if (fs::dir_exists(raw_files_path) == FALSE) {
    stop(paste(raw_files_path, "is not a valid path"))
  }
  
  ## Output path
  if (!fs::dir_exists(output_path)) {
    fs::dir_create(output_path)
  }
  
  # Create file names pattern to be read
  glob_pattern <- paste0(raw_files_path, "*")
  
  files <- fs::dir_ls(path = raw_files_path,
                      glob = glob_pattern)
  
  # Ignore files to be read
  if (ignore_file == "metadata") {
    file_to_ignore  <- files[stringr::str_detect(files, "metadata")]
    
    # Validate if file to ignore exists
    if (!rlang::is_empty(file_to_ignore) == TRUE) {
      files <- files[files != file_to_ignore]
      print(paste("The file to be ignore when reading is:", file_to_ignore))
    }
    
  } else if (ignore_file != "metadata") {
    file_to_ignore <- files[stringr::str_detect(files, ignore_file)]
    
    # Validate if file to ignore exists
    if (!rlang::is_empty(file_to_ignore) == TRUE) {
      files <- files[files != file_to_ignore]
      print(paste("The file to be ignore when reading is:", file_to_ignore))
    }
  }
  
  pixels <- files %>%
    purrr::map_dfr(readr::read_csv, .id = "file_id") %>%
    janitor::clean_names()
  
  ### 2. CLEAN DATASET ----
  data.table::setDT(pixels)
  pixels[ , c("folder_1", "folder_2", "file_id") := tstrsplit(file_id, "/", fixed = TRUE)]
  pixels[ , ":="(file_id = stringr::str_remove(file_id, ".csv"))]
  pixels[ , ":="(location = stringr::str_extract(geo, "\\[(.*?)\\]"))]
  pixels[ ,  c("lat", "long") := tstrsplit(location, ",", fixed = TRUE)]
  pixels[ , ":="(lat = stringr::str_extract(lat, "-?[0-9.]+"),
                 long = stringr::str_extract(long, "-?[0-9.]+"))]
  pixels[ , ":="(date = lubridate::ymd(file_id))]
  pixels[ , c("file_id", "system_index", 
              "folder_1", "folder_2",
              "geo", "location") := NULL]
  
  ### 3. EXPORT OBJECT ----
  output_path <- paste0(output_path, "/", output_file_name)
  saveRDS(object = pixels, file = output_path)
  print(paste("Your processed data was saved in", output_path))
}


#' @title Ingest MODIS GPP data 
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description It takes the folder containing the csv files downloaded from
#' Google Drive (exported from GEE) and parse into one RDS object all the files.
#' 
#' @details This is designed to parsed, clean variable names, write the NAs
#' into one RDS object that will be exported for the GPP datasets with
#' an spatial resolution of 250m
#' 
#' @param raw_files_path Folder containing the `.csv` files with the MODIS GPP info.
#' @param output_path Folder where the processed data should be save. Do not 
#' include the trailing slash `/`.
#' @param output_file_name The name you want to give to the final `.rds` file
#' @param ignore_file Optional. If there is a metadata file that should be 
#' ignored when creating the final file, this should be indicated here.
#'
#' @example 
#' \dontrun{
#' ingest_modis_gpp(raw_files_path = "data_test_temporal/more_folders/", 
#'              output_path = "test", 
#'              output_file_name = "test.rds",
#'              ignore_file = "metadata")
#'}
#'

# ingest_modis_gpp(raw_files_path = "data/gee_santa_rosa_modis_gpp_250",
#                  output_path = "data_satellite_processed",
#                  output_file_name = "santa_rosa_modis_gpp_250_clean.rds",
#                  ignore_file = "metadata")
# 
# 
# 
# ingest_modis_gpp <- function(raw_files_path, output_path, output_file_name,
#                              ignore_file = "metadata") {
#   
#   # Paths validation
#   ## Existing path
#   if (fs::dir_exists(raw_files_path) == FALSE) {
#     stop(paste(raw_files_path, "is not a valid path"))
#   }
#   
#   ## Output path
#   if (!fs::dir_exists(output_path)) {
#     fs::dir_create(output_path)
#   }
#   
#   # Create file names pattern to be read
#   glob_pattern <- paste0(raw_files_path, "*")
#   
#   files <- fs::dir_ls(path = raw_files_path,
#                       glob = glob_pattern)
#   
#   # Ignore files to be read
#   if (ignore_file == "metadata") {
#     file_to_ignore  <- files[stringr::str_detect(files, "metadata")]
#     
#     # Validate if file to ignore exists
#     if (!rlang::is_empty(file_to_ignore) == TRUE) {
#       files <- files[files != file_to_ignore]
#       print(paste("The file to be ignore when reading is:", file_to_ignore))
#     }
#     
#   } else if (ignore_file != "metadata") {
#     file_to_ignore <- files[stringr::str_detect(files, ignore_file)]
#     
#     # Validate if file to ignore exists
#     if (!rlang::is_empty(file_to_ignore) == TRUE) {
#       files <- files[files != file_to_ignore]
#       print(paste("The file to be ignore when reading is:", file_to_ignore))
#     }
#   }
#   
#   pixels <- files %>%
#     purrr::map_dfr(readr::read_csv, .id = "file_id") %>%
#     janitor::clean_names()
#   
#   ### 2. CLEAN DATASET ----
#   data.table::setDT(pixels)
#   pixels[ , c("folder_1", "folder_2", "file_id") := tstrsplit(file_id, "/", fixed = TRUE)]
#   pixels[ , ":="(file_id = stringr::str_remove(file_id, ".csv"))]
#   pixels[ , ":="(location = stringr::str_extract(geo, "\\[(.*?)\\]"))]
#   pixels[ ,  c("lat", "long") := tstrsplit(location, ",", fixed = TRUE)]
#   pixels[ , ":="(lat = stringr::str_extract(lat, "-?[0-9.]+"),
#                  long = stringr::str_extract(long, "-?[0-9.]+"))]
#   pixels[ , ":="(date = lubridate::ymd(file_id))]
#   pixels[ , c("file_id", "system_index", 
#               "folder_1", "folder_2",
#               "geo", "location") := NULL]
#   
#   ### 3. EXPORT OBJECT ----
#   output_path <- paste0(output_path, "/", output_file_name)
#   saveRDS(object = pixels, file = output_path)
#   print(paste("Your processed data was saved in", output_path))
# }
# 
