library(ReacTran)
library(deSolve)
library(plot.matrix)

g <- 9.80665

change_granularity <- function(mat, granularityLevel = 10) {
  if (granularityLevel == 1) {return(mat)}
  matGranularized <- matrix(0,
                            ceiling(nrow(mat)/granularityLevel),
                            ceiling(ncol(mat)/granularityLevel))
  for (i in 1:nrow(matGranularized)) {
    for (j in 1:ncol(matGranularized)) {
      matGranularized[i, j] <- sum(mat[
        (1 + (i-1)*granularityLevel):min((1 + i*granularityLevel - 1), nrow(mat)),
        (1 + (j-1)*granularityLevel):min((1 + j*granularityLevel - 1), ncol(mat))])
    }
  }
matGranularized
}

pde2D <- function(t, y, parms) {
  CONC <- matrix(nrow = parms$nx, ncol = parms$ny, y)
  Tran <- tran.2D(CONC, D.x = parms$diffusion_coefficient,
                  v.x = parms$velocity[1], v.y = parms$velocity[2],
                  dx = 1, dy = 1)
list(as.vector(Tran$dC))
}

pde3D <- function(t, y, parms) {
  print(t)
  CONC <- array(y, dim = c(parms$nx, parms$ny, parms$nz))
  Tran <- tran.3D(CONC, D.x = parms$diffusion_coefficient,
                  v.x = parms$velocity[1], v.y = parms$velocity[2],
                  v.z = parms$velocity[3],
                  dx = 1, dy = 1, dz = 1)
list(as.vector(Tran$dC))
}

model <- function(mat, T, diffusion_coefficient, velocity,
                  granularityLevel = 10, timeStep = 1, method = "lsodes") {
  matGranularized <- change_granularity(mat, granularityLevel = granularityLevel)
  matObstacles <- matGranularized
  matObstacles[matObstacles>0] <- 0
  matGranularized[matGranularized<0] <- 0
  parms <- list(diffusion_coefficient = diffusion_coefficient,
                velocity = velocity, 
                nx = nrow(matGranularized), 
                ny = ncol(matGranularized))
  y <- as.vector(matGranularized)
  res <- tryCatch({
    if (method == "lsodes") {
      ode.2D(y, seq(0, T, by = timeStep), pde2D, parms,
             dimens = c(parms$nx, parms$ny),
             method = method,
             lrw = 187945)      
    } else {
      ode.2D(y, seq(0, T, by = timeStep), pde2D, parms,
             dimens = c(parms$nx, parms$ny),
             method = method)
    }
  }, error = function(cond) {
           stop("Данный уровень гранулярности не поддерживается методом решения ОДУ.
                Либо увеличьте уровень гранулярности, либо выберете другой метод!")
  })
  return(list(model = res, obstacles = matObstacles))
}

add_third_component_to_velocity <- function(velocity, viscosity,
                                            rho_water, rho_oil,
                                            diam_oil) {
  diam_oil <- diam_oil/10
  diam_crit <- 9.52*(viscosity^{2/3})/(g^{1/3}*(1 - rho_oil/rho_water))
  if (diam_oil < diam_crit) {
    w_b <- g*diam_oil^2*(1 - rho_oil/rho_water)/(18*viscosity)
  } else {
    w_b <- ((8/3)*g*(diam_oil^2)*(1 - rho_oil/rho_water))^0.5
  }
  velocity <- c(velocity, -w_b)
  print(velocity)
velocity
}

model_3D <- function(mat, T, diffusion_coefficient, velocity,
                     depth, viscosity, rho_water, rho_oil, diam_oil,
                     granularityLevel = 10, timeStep = 1, method = "lsodes") {
  matGranularized <- change_granularity(mat, granularityLevel = granularityLevel)
  matObstacles <- matGranularized
  matObstacles[matObstacles>0] <- 0
  matGranularized[matGranularized<0] <- 0
  velocity <- add_third_component_to_velocity(velocity, viscosity,
                                              rho_water, rho_oil, diam_oil)
  parms <- list(diffusion_coefficient = diffusion_coefficient,
                velocity = velocity, 
                nx = nrow(matGranularized), 
                ny = ncol(matGranularized),
                nz = round(depth/(granularityLevel^2)))
  y <- as.vector(matGranularized)
  y <- rep(y, parms$nz)
  res <- tryCatch({
    if (method == "lsodes") {
      ode.3D(y, seq(0, T, by = timeStep), pde3D, parms,
             dimens = c(parms$nx, parms$ny, parms$nz),
             method = method,
             lrw = 187945)
    } else {
      ode.3D(y, seq(0, T, by = timeStep), pde3D, parms,
             dimens = c(parms$nx, parms$ny, parms$nz),
             method = method)
    }
  }, error = function(cond) {
    stop("Данный уровень гранулярности не поддерживается методом решения ОДУ.
                Либо увеличьте уровень гранулярности, либо выберете другой метод!")
  })
  return(list(model = res, obstacles = matObstacles))
}

calculate_oil_intensivity <- function(res) {
  apply(res, 1, sum)
}

calculate_oil_area <- function(res) {
  apply(res, 1, function(x) {sum(ifelse(x>=1, 1, 0))})
}

process_result_for_s <- function(res, s, nx, ny) {
  mat <- t(matrix(ifelse(res[s, ][(length(res[s, ]) - nx*ny + 1):length(res[s, ])]>=1, 1, 0),
                  nx, ny))
mat
}

take_into_account_obstacles <- function(oil, obstacles,
                                        use_land, land_consumes) {
  set.seed(239)
  for (i in 1:nrow(oil)) {
    for (j in 1:ncol(oil)) {
      if ((oil[i, j] > 0) & (obstacles[i, j] < 0)) {
        if (use_land) {
          if (!land_consumes) {
            if (rbinom(1, 1, 0.5) > 0) {
              element <- find_nearest_element(i, j, obstacles, oil)
              if (!is.na(element[1])) {
                oil[element$i[1], element$j[1]] <- 1
              }
            }
          }
          oil[i, j] <- -1
        }
      } else if ((oil[i, j] == 0) & (obstacles[i, j] < 0)) {
        oil[i, j] <- -1
      }
    }
  }
oil
}

find_nearest_element <- function(i, j, obstacles, oil) {
  k_max <- max(i, j, nrow(oil)-i, ncol(oil)-j)
  for (k in 1:k_max) {
    for (t in max((j-k), 1):min((j+k), ncol(oil))) {
      if (oil[min(i + k, nrow(oil)), t] == 0) {
        return(list(i = min(i + k, nrow(oil)), j = t, k = k, side = 1))
      }
    }
    for (t in max((i-k), 1):min((i+k), nrow(oil))) {
      if (oil[t, min(j + k, ncol(oil))] == 0) {
        return(list(i = t, j = min(j + k, ncol(oil)), k = k, side = 2))
      }
    }
    for (t in max((i-k), 1):min((i+k), nrow(oil))) {
      if (oil[t, max(j - k, 1)] == 0) {
        return(list(i = t, j = max(j - k, 1), k = k, side = 3))
      }
    }
    for (t in max((j-k), 1):min((j+k), ncol(oil))) {
      if (oil[max(i - k, 1), t] == 0) {
        return(list(i = max(i - k, 1), j = t, k = k, side = 4))
      }
    }
  }
  return(NA)
}

prune_picture <- function(oil, initial_oil) {
  set.seed(239)
  initial_surface <- sum(initial_oil)
  current_surface <- sum(oil)
  if (current_surface > initial_surface) {
    prob <- (current_surface - initial_surface)/current_surface
  } else {
    prob <- NA
  }
  for (i in 1:nrow(oil)) {
    for (j in 1:ncol(oil)) {
      if (oil[i, j] > 0) {
        if (initial_oil[i, j] == 0) {
          if (!is.na(prob)) {
            if (rbinom(1, 1, prob) > 0) {
              oil[i, j] <- 0
              next
            }
          }
        }
      }
    }
  }
  oil
}

remove_chessboard <- function(oil, initial_oil) {
  for (i in 1:nrow(oil)) {
    for (j in 1:ncol(oil)) {
      if (oil[i, j] > 0) {
        if (check_chessboard_neighbors(oil, i, j)) {
          oil[i, j] <- 0
        }
      }
    }
  }
oil
}

check_chessboard_neighbors <- function(oil, i, j) {
  if (as.numeric(oil[min(i+1, nrow(oil)), j] == 0) +
      as.numeric(oil[max(i-1, 1), j] == 0) + 
      as.numeric(oil[i, min(j+1, ncol(oil))] == 0) +
      as.numeric(oil[i, max(j-1, 1)] == 0) >= 3) {
    return(TRUE)
  }
  return(FALSE)
}

plot_oil_by_time <- function(res, obstacles, t, nx, ny,
                             width, height,
                             use_land, land_consumes) {
  if (t <= 0) {stop("Момент времени должен быть положительным!")}
  tryCatch({oil <- process_result_for_s(res, t, nx, ny)},
           error = function(cond) {stop("Момент времени слишком велик! Его надо уменьшить!")})
  oil[is.na(oil) || is.character(oil)] <- 0
  if (land_consumes) {
    initial_oil <- process_result_for_s(res, 1, nx, ny)
    oil <- prune_picture(oil, initial_oil)
    oil <- remove_chessboard(oil)
  }
  obstacles <- t(obstacles)
  oil <- take_into_account_obstacles(oil, obstacles, use_land, land_consumes)
  tryCatch({plot(oil,
                 xlab = paste0("Горизонталь (", width/1000, " км)"),
                 ylab = paste0("Вертикаль (", height/1000, " км)"),
                 main = paste0("Распространение нефти (момент времени ", t, ")."),
                 col = identify_colors(oil))},
           error = function(cond) {stop("Выберите другой момент времени! Скорее всего, весь снимок заполнен нефтью или водой.")})
}

choose_time_slice_for_plotting <- function(res, time, nx, ny) {
  leaf_number <- time - 1
  oil <- process_result_for_s(res[[1 + leaf_number]], 1,
                              nx, ny)
oil
}

plot_oil_by_time_multiple <- function(results, obstacles, t, nx, ny,
                                      width, height, use_land, land_consumes) {
  if (t <= 0) {stop("Момент времени должен быть положительным!")}
  tryCatch({oil <- choose_time_slice_for_plotting(results, t, nx, ny)},
           error = function(cond) {stop("Момент времени слишком велик! Его надо уменьшить!")})
  oil[is.na(oil) || is.character(oil)] <- 0
  if (land_consumes) {
    initial_oil <- choose_time_slice_for_plotting(results, 1, nx, ny)
    oil <- prune_picture(oil, initial_oil)
    oil <- remove_chessboard(oil)
  }
  obstacles <- t(obstacles)
  oil <- take_into_account_obstacles(oil, obstacles, use_land, land_consumes)
  tryCatch({plot(oil,
                 xlab = paste0("Горизонталь (", width/1000, " км)"),
                 ylab = paste0("Вертикаль (", height/1000, " км)"),
                 main = paste0("Распространение нефти (момент времени ", t, ")."),
                 col = identify_colors(oil))},
           error = function(cond) {stop("Выберите другой момент времени! Скорее всего, весь снимок заполнен нефтью или водой.")})
}

identify_colors <- function(oil) {
  ranges <- c()
  if (-1 %in% oil) {ranges <- c(ranges, -1)}
  if (0 %in% oil) {ranges <- c(ranges, 0)}
  if (1 %in% oil) {ranges <- c(ranges, 1)}
  ranges <- ranges[order(ranges)]
  if (identical(ranges, c(-1, 0, 1))) {
    return(c("green", "blue", "red"))
  } else if (identical(ranges, c(-1, 0))) {
    return(c("green", "blue"))
  } else if (identical(ranges, c(-1, 1))) {
    return(c("green", "red"))
  } else if (identical(ranges, c(0, 1))) {
    return(c("blue", "red"))
  } else if (identical(ranges, -1)) {
    return(c("green"))
  } else if (identical(ranges, 0)) {
    return(c("blue"))
  } else if (identical(ranges, 1)) {
    return(c("red"))
  }
  return(c("green", "blue", "red"))
}