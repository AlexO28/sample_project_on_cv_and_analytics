server <- function(input, output) {
  
  time_step <- reactive(input$step)
  time_var <- reactive(input$time)
  predict_past <- reactive(input$predictPast)
  water_temperature <- reactive(input$water_temperature)
  velocity1 <- reactive(input$velocity1)
  velocity2 <- reactive(input$velocity2)
  granularity_level <- reactive(input$granularity_level)
  T_max <- reactive(input$T_max)
  method <- reactive(input$method)
  data_file <- reactive(input$data_file)
  meteo_file <- reactive(input$meteo_file)
  wind_coefficient <- reactive(input$wind_coefficient)
  scenario_date <- reactive(input$scenario_date)
  use_land <- reactive(input$useLand)
  use_emulsification <- reactive(input$useEmulsification)
  type_of_oil <- reactive(input$typeOfOil)
  use_evaporation <- reactive(input$useEvaporation)
  schmidt_number <- reactive(input$schmidtNumber)
  evaporation_r <- reactive(input$evaporation_r)
  mass_transfer_rate <- reactive(input$massTransferRate)
  use_waves <- reactive(input$useWaves)
  use_currents <- reactive(input$useCurrents)
  use3DModel <- reactive(input$use3DModel)
  viscosity <- reactive(input$viscosity)
  rho_oil <- reactive(input$rho_oil)
  diam_oil <- reactive(input$diam_oil)
  auto_detection <- reactive(input$autoDetection)
  
  time_deb <- debounce(time_var, 2000)
  predict_past_deb <- debounce(predict_past, 2000)
  water_temperature_deb <- debounce(water_temperature, 2000)
  velocity1_deb <- debounce(velocity1, 2000)
  velocity2_deb <- debounce(velocity2, 2000)
  granularity_level_deb <- debounce(granularity_level, 2000)
  T_max_deb <- debounce(T_max, 2000)
  time_step_deb <- debounce(time_step, 2000)
  method_deb <- debounce(method, 2000)
  data_file_deb <- debounce(data_file, 2000)
  meteo_file_deb <- debounce(meteo_file, 2000)
  wind_coefficient_deb <- debounce(wind_coefficient, 2000)
  scenario_date_deb <- debounce(scenario_date, 2000)
  use_land_deb <- debounce(use_land, 2000)
  use_emulsification_deb <- debounce(use_emulsification, 2000)
  type_of_oil_deb <- debounce(type_of_oil, 2000)
  use_evaporation_deb <- debounce(use_evaporation, 2000)
  schmidt_number_deb <- debounce(schmidt_number, 2000)
  evaporation_r_deb <- debounce(evaporation_r, 2000)
  mass_transfer_rate_deb <- debounce(mass_transfer_rate, 2000)
  use_waves_deb <- debounce(use_waves, 2000)
  use_currents_deb <- debounce(use_currents, 2000)
  use3DModel_deb <- debounce(use3DModel, 2000)
  viscosity_deb <- debounce(viscosity, 2000)
  rho_oil_deb <- debounce(rho_oil, 2000)
  diam_oil_deb <- debounce(diam_oil, 2000)
  auto_detection_deb <- debounce(auto_detection, 2000)
  
  calculation_res <- reactive({
    print("calculation has started")
    multip <- 1
    if (auto_detection_deb() & grepl("unlabelled", data_file_deb())) {
      data_file <- apply_auto_detection(data_file_deb())
    } else {
      data_file <- data_file_deb()
    }
    if (predict_past_deb()) {multip <- -1}
    statistical_data <- get_statistical_data(data_file)
    if (use_waves_deb()) {
      fetch_length <- calculate_fetch_length(data_file)
      waves_speed <- derive_waves_speed(statistical_data,
                                        c(velocity1_deb(), velocity2_deb()),
                                        fetch_length)
      waves_contributions <- calculate_waves_contributions(velocity1_deb(),
                                                           velocity2_deb(),
                                                           waves_speed)
    } else {
      waves_contributions <- c(0, 0)
    }
    if (use_currents_deb()) {
      year <- statistical_data$Year[1]
      latitude <- statistical_data$Latitude[1]
      longitude <- statistical_data$Longitude[1]
      currents_contributions <- apply_model_to_latlong(latitude, longitude, year)
    } else{
      currents_contributions <- c(0, 0)
    }
    diffusion_coefficient <- calculate_diffusion_coefficient(water_temperature_deb())
    calculate_model(multip*diffusion_coefficient,
                    multip*velocity1_deb()*wind_coefficient_deb()/30,
                    multip*velocity2_deb()*wind_coefficient_deb()/30,
                    granularity_level_deb(),
                    T_max_deb(),
                    time_step_deb(),
                    method_deb(),
                    data_file,
                    use_emulsification_deb(),
                    type_of_oil_deb(),
                    koef = multip*wind_coefficient_deb(),
                    use_evaporation_deb(),
                    schmidt_number_deb(),
                    evaporation_r_deb(),
                    mass_transfer_rate_deb(),
                    multip*waves_contributions/10000,
                    multip*currents_contributions/10000,
                    use3DModel_deb(),
                    viscosity_deb(),
                    rho_oil_deb(),
                    diam_oil_deb(),
                    statistical_data$Depth[1],
                    predictFuture = multip > 0)
  })
  output$oilPlot <- renderPlot({
    layout(matrix(c(1, 1, 2, 3)))
    res <- calculation_res()$res
    mat <- calculation_res()$mat
    width <- calculation_res()$width
    height <- calculation_res()$height
    plot_oil_by_time(res$model,
                     res$obstacles,
                     time_deb(),
                     ceiling(nrow(mat)/input$granularity_level), 
                     ceiling(ncol(mat)/input$granularity_level),
                     width, height,
                     use_land_deb(), land_consumes = predict_past_deb())
    print("First plot has been drawn.")
    plot(calculate_oil_intensivity(res$model), main = "Концентрация нефти от времени",
         xlab = "Время", ylab = "Концентрация")
    plot(calculate_oil_area(res$model), main = "Площадь нефти от времени",
         xlab = "Время", ylab = "Площадь")
  })
  scenario_res <- reactive({
    if (wind_coefficient_deb() < 0) {stop("Коэффициент ветра должен быть положительным!")}
    print("scenario calculation has started")
    multip <- 1
    divisor <- 30
    if (predict_past_deb()) {
      multip <- -1
      divisor <- 30
    }
    statistical_data <- get_statistical_data(data_file_deb())
    if (use_waves_deb()) {
      fetch_length <- calculate_fetch_length(data_file_deb())
    }
    if (use_currents_deb()) {
      year <- statistical_data$Year[1]
      latitude <- statistical_data$Latitude[1]
      longitude <- statistical_data$Longitude[1]
      currents_contributions <- apply_model_to_latlong(latitude, longitude, year)
      currents_contributions <- currents_contributions/divisor
    } else{
      currents_contributions <- c(0, 0)
    }
    meteo_data <- get_meteo_data(meteo_file_deb(), wind_coefficient_deb()/divisor,
                                 scenario_date_deb(), multip)
    mat <- prepare_matrix(data_file_deb())
    prepared_data <- prepare_matrix(data_file_deb())
    mat <- prepared_data$mat
    width <- prepared_data$width
    height <- prepared_data$height
    nx <- nrow(mat)
    ny <- ncol(mat)
    multip1 <- (nx/granularity_level_deb())/width
    multip2 <- (ny/granularity_level_deb())/height
    multip_squared <- (((nx/granularity_level_deb())/width)^2 +
                          ((ny/granularity_level_deb())/height)^2)
    meteo_data$vx <- meteo_data$vx*multip1
    meteo_data$vy <- meteo_data$vy*multip2
    all_res <- list()
    obstacles <- c()
    for (j in 1:nrow(meteo_data)) {
      print(j)
      if (j > 1) {
        mat[mat == 1] <- mat[mat == 1] * granularity_level_deb()
        if (length(obstacles) == 0) {
          obstacles <- model_res$obstacles
        }
      }
      if (use_waves_deb()) {
        waves_speed <- derive_waves_speed(statistical_data,
                                          as.vector(t(meteo_data[j,
                                                                 c("vx", "vy")])),
                                          fetch_length)
        waves_contributions <- calculate_waves_contributions(meteo_data$vx[j]/wind_coefficient_deb(),
                                                             meteo_data$vy[j]/wind_coefficient_deb(),
                                                             waves_speed)
        waves_contributions <- waves_contributions/divisor
      } else {
        waves_contributions <- c(0, 0)
      }
      diffusion_coefficient <- calculate_diffusion_coefficient(water_temperature_deb())
      model_res <- calculate_scenario(mat,
                                      multip_squared*multip*diffusion_coefficient,
                                      meteo_data$vx[j],
                                      meteo_data$vy[j],
                                      ifelse(j==1, granularity_level_deb(), 1),
                                      method_deb(),
                                      use_emulsification_deb(),
                                      type_of_oil_deb(),
                                      koef = multip*wind_coefficient_deb(),
                                      use_evaporation_deb(),
                                      schmidt_number_deb(),
                                      evaporation_r_deb(),
                                      mass_transfer_rate_deb(),
                                      waves_contributions/10,
                                      currents_contributions/10,
                                      use3DModel_deb(),
                                      viscosity_deb(),
                                      rho_oil_deb(),
                                      diam_oil_deb(),
                                      statistical_data$Depth[1],
                                      predictFuture = multip > 0)
      mat <- t(process_result_for_s(model_res$model,
                                    2,
                                    ceiling(nx/granularity_level_deb()),
                                    ceiling(ny/granularity_level_deb())))
      all_res[[j]] <- model_res$model
    }
  return(list(res = all_res, mat = mat, meteo_data = meteo_data,
              obstacles = obstacles, width = width, height = height))
  })
  output$oilPlot2 <- renderPlot({
    results <- scenario_res()$res
    meteo_data <- scenario_res()$meteo_data
    mat <- scenario_res()$mat
    width <- scenario_res()$width
    height <- scenario_res()$height
    layout(matrix(c(1, 1, 2, 3)))
    plot_oil_by_time_multiple(results,
                              scenario_res()$obstacles,
                              time_deb(),
                              nrow(mat), 
                              ncol(mat),
                              width, height,
                              use_land_deb(),
                              land_consumes = predict_past_deb())
    date <- meteo_data$date[1]
    plot(meteo_data$time, meteo_data$vx,
         main =
           paste0("Горизонтальная компонента скорости от времени для ", date),
         xlab = "Время (в часах)", ylab = "Скорость (в ячейках/час)",
         type = "b")
    abline(h = 0, col = "red")
    plot(meteo_data$time, meteo_data$vy,
         main =
           paste0("Вертикальная компонента скорости от времени для ", date),
         xlab = "Время (в часах)", ylab = "Скорость (в ячейках/час)",
         type = "b")
    abline(h = 0, col = "red")
  })
}
