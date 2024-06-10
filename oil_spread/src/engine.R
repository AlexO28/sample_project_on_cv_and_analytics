calculate_diffusion_coefficient <- function(water_temperature) {
  if (water_temperature < 0) {stop('Температура воды должна быть не меньше 0.')}
0.002*(water_temperature/22)^{1.53}
}

calculate_model <- function(diffusion_coefficient, velocity1, velocity2,
                            granularityLevel, T_max, timeStep,
                            method, data_file,
                            use_emulsification, type_of_oil, koef = 1,
                            use_evaporation, schmidt_number, evaporation_r,
                            mass_transfer_rate, waves_contributions,
                            currents_contributions,
                            use3DModel, viscosity, rho_oil, diam_oil, depth,
                            predictFuture = TRUE) {
  if (T_max <= 0) {stop("Максимальное время предсказания должно быть положительным!")}
  prepared_data <- prepare_matrix(data_file)
  mat <- prepared_data$mat
  width <- prepared_data$width
  height <- prepared_data$height
  multip1 <- 3600*(nrow(mat)/granularityLevel)/width
  multip2 <- 3600*(ncol(mat)/granularityLevel)/height
  multip_squared <- 3600*(((nrow(mat)/granularityLevel)/width)^2 +
                            ((ncol(mat)/granularityLevel)/height)^2)
  if (use3DModel) {
    res <- model_3D(mat,
                    T_max+1,
                    diffusion_coefficient * multip_squared,
                    c(multip1*(velocity1 + waves_contributions[1] +
                                 currents_contributions[1]),
                      multip2*(velocity2 + waves_contributions[2] +
                                 currents_contributions[2])),
                    depth, viscosity, 1000, rho_oil, diam_oil,
                    granularityLevel, timeStep, method)
  } else {
    res <- model(mat,
                 T_max+1,
                 diffusion_coefficient * multip_squared,
                 c(multip1*(velocity1 + waves_contributions[1] +
                              currents_contributions[1]),
                   multip2*(velocity2 + waves_contributions[2] +
                              currents_contributions[2])),
                 granularityLevel,
                 timeStep,
                 method)
  }
  if (use_emulsification) {
    res <- apply_emulsification(res, c(velocity1, velocity2)/abs(koef),
                                type_of_oil, predictFuture)
  }
  if (use_evaporation && (koef > 0)) {
    res <- apply_evaporation(res, mass_transfer_rate, schmidt_number,
                             evaporation_r, c(velocity1, velocity2)/abs(koef),
                             predictFuture)
  }
  list(res = res, mat = mat, width = width, height = height)
}

prepare_matrix <- function(data_file) {
  image <- get_image(data_file)
  dimensions <- extract_dimensions_from_name(data_file)
  tab <- mark_red_areas(image)
  tab <- mark_green_areas(tab)
  mat <- turn_into_matrix(tab)
  list(mat = mat, width = dimensions$width, height = dimensions$height)
}

calculate_scenario <- function(mat, diffusion_coefficient,
                               velocity1, velocity2,
                               granularityLevel, method,
                               use_emulsification, type_of_oil, koef = 1,
                               use_evaporation, schmidt_number, evaporation_r,
                               mass_transfer_rate, waves_contributions,
                               currents_contributions,
                               use3DModel, viscosity, rho_oil, diam_oil, depth,
                               predictFuture = TRUE) {
  if (use3DModel) {
    res <- model_3D(mat, 1, diffusion_coefficient,
                    c(velocity1 +
                        waves_contributions[1] + currents_contributions[1],
                      velocity2 +
                        waves_contributions[2] + currents_contributions[2]),
                    depth, viscosity, 1000, rho_oil, diam_oil,
                    granularityLevel, 1, method)
  } else {
    res <- model(mat, 1, diffusion_coefficient,
                 c(velocity1 +
                     waves_contributions[1] + currents_contributions[1],
                   velocity2 +
                     waves_contributions[2] + currents_contributions[2]),
                 granularityLevel, 1, method)
  }
  if (use_emulsification) {
    res <- apply_emulsification(res,
                                0.01*c(velocity1, velocity2)/abs(koef),
                                type_of_oil, predictFuture)
  }
  if (use_evaporation && (koef > 0)) {
    res <- apply_evaporation(res, mass_transfer_rate, schmidt_number,
                             evaporation_r,
                             0.01*c(velocity1, velocity2)/abs(koef),
                             predictFuture)
  }
res
}

apply_auto_detection <- function(data_file) {
  input_data <- data.table(file_name = data_file, file_path = "../data/")
  fwrite(input_data, "./data/input_data.csv", sep = ",")
  cur_folder <- getwd()
  setwd("./src/")
  system("python3 link_oil_detection_api.py")
  setwd(cur_folder)
  output_data <- fread("./data/output_file.csv")
  if (output_data$done == FALSE) {
    stop("Автодетекция не сработала!")
  }
output_data$output_file[1]
}
