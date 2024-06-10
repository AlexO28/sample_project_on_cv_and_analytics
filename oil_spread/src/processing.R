library(imager)
library(data.table)

extract_dimensions_from_name <- function(file_name) {
  file_name <- strsplit(file_name, "\\.")[[1]][1]
  parsed_info <- strsplit(file_name, "_")[[1]]
  height <- parsed_info[length(parsed_info)]
  width <- parsed_info[length(parsed_info)-1]
list(width = as.numeric(width), height = as.numeric(height))
}

get_image <- function(filePath = "wakasho.png") {
  filePath <- paste0("./data/", filePath)
  as.data.frame(load.image(filePath), wide = "c") 
}

get_meteo_data <- function(filePath = "./data/scenario_meteo_data.csv",
                           wind_coefficient = 0.03,
                           scenario_date = as.Date("2020-08-11"),
                           multip = 1) {
  filePath <- paste0("./data/", filePath)
  tab <- fread(filePath)
  tab[, Date := as.Date(datetime)]
  if (multip > 0) {
    tab <- tab[Date >= scenario_date, ]
  } else {
    tab <- tab[Date < scenario_date, ]
    tab[, windspeed := multip*windspeed]
  }
  if (nrow(tab) == 0) {
    stop("Необходимых данных в метео-файле нет! Измените дату!")
  }
  min_date <- tab$Date[1]
  tab$windspeed <- tab$windspeed*wind_coefficient*1000
  tab$vx <- -tab$windspeed*sin(pi*(tab$winddir/180))
  tab$vy <- -tab$windspeed*cos(pi*(tab$winddir/180))
  tab <- tab[, list(vx, vy)]
  tab$time <- (0:(nrow(tab)-1))
  tab$date <- min_date
as.data.frame(tab)
}

get_statistical_data <- function(file_name) {
  filePath <- paste0("./data/", 'statistical_data.csv')
  statistical_data <- fread(filePath)
  file_name <- strsplit(file_name, "\\.")[[1]][1]
  candidate <- statistical_data[Event == file_name,]
  if (nrow(candidate) == 0) {
    candidate <- statistical_data[2, ]
  }
candidate
}

calculate_fetch_length <- function(file_name) {
  image <- get_image(file_name)
  dimensions <- extract_dimensions_from_name(file_name)
  image <- mark_red_areas(image)
  fetch_length <- sqrt(length(image[image>0])/length(image)) *
    sqrt(dimensions$height) * sqrt(dimensions$width)/pi
fetch_length
}

mark_red_areas <- function(tab) {
  tab$target <- ifelse((tab$c.1 > 0.6) & (tab$c.2 < 0.4) & (tab$c.3 < 0.4),
                       1, 0)
tab
}

mark_green_areas <- function(tab) {
  tab$target[(tab$c.1 < 0.1) & (tab$c.2 > 0.5) & (tab$c.3 < 0.1)] <- -1
  tab <- tab[, c("x", "y", "target")]
tab
}

turn_into_matrix <- function(tab) {
  maxX <- max(tab$x)
  maxY <- max(tab$y)
  mat <- matrix(0, maxX, maxY)
  for (j in which(tab$target == 1)) {
    mat[tab$x[j], tab$y[j]] <- 1
  }
  if (nrow(tab[tab$target<0, ]) > 0) {
    for (j in which(tab$target == -1)) {
      mat[tab$x[j], tab$y[j]] <- -1
    }
  }
mat
}
