get_folder_with_masks <- function(data_source) {
  parsed_str <- strsplit(data_source, "/")[[1]]
  parsed_str[length(parsed_str)] <- "data_masks"
  data_source_masks <- paste(parsed_str, collapse = "/")
data_source_masks
}

choose_random_picture <- function(random_seed, date_str, withOil, masks) {
  set.seed(random_seed)
  if (withOil) {
    tab_slice <- statistical_data[grepl(date_str, file) &
                                  (noOil == 0) &
                                  (share_black >= 0.001) &
                                  (file %in% masks), file]
  } else {
    tab_slice <- statistical_data[grepl(date_str, file), file]
  }
  filename <- tab_slice[sample(1:length(tab_slice), 1)]
filename
}

write_to_temp_file <- function(folder, filename) {
  tempdf <- data.frame(file = filename, noOil = 0)
  fwrite(tempdf, paste0(folder, 'temp.txt'), sep = ',')
}

copy_png_file <- function(filename, source_folder, destination_folder) {
  file.copy(paste0(source_folder, filename),
            paste0(destination_folder, filename))
}
