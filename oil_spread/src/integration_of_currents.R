apply_model_to_latlong <- function(latitude, longitude, year) {
  tab <- get_data_on_currents(paste0(getwd(), '/data/cm2011a1_87364.dat'))
  tab_all <- rbind(tab$tab_one_year, tab$tab_multi_years)
  cluster_0 <- find_closest_cluster(latitude, longitude, tab_all)
  tab_cluster <- tab_all[cluster == cluster_0, ]
  if (max(tab_cluster$year) >= year) {
    res_cluster <- derive_velocities_from_cluster(year, tab_cluster)
  } else {
    res_cluster <- predict_velocities_from_cluster(year, tab_cluster)
  }
  contributions <- approximate_latlong_by_cluster(res_cluster,
                                                  latitude,
                                                  longitude)
c(contributions[1], -contributions[2])
}

find_closest_cluster <- function(latitude_0, longitude_0, tab_all) {
  tab_all[, distance := ((latitude - latitude_0)^2 +
                         (longitude - longitude_0)^2)^0.5]
  tab_all <- tab_all[order(tab_all$distance), ]
tab_all$cluster[1]
}

derive_velocities_from_cluster <- function(year_0, tab) {
  tab_filtered <- tab[year >= year_0, list(latitude, longitude, mean_U, mean_V)]
  tab_filtered[, by = list(latitude, longitude),
                 list(mean_U := mean(mean_U), mean_V := mean(mean_V))]
tab_filtered
}

predict_velocities_from_cluster <- function(year, tab) {
  res_weighted <- predict_velocities_from_cluster_weighted(year, tab)
  res_ema <- predict_velocities_from_cluster_ema(year, tab)
  res_var <- predict_velocities_from_cluster_var(year, tab)
  res <- res_weighted
  res$mean_U <- (res_weighted$mean_U + res_ema$mean_U + res_var$mean_U)/3
  res$mean_V <- (res_weighted$mean_V + res_ema$mean_V + res_var$mean_V)/3
res
}

predict_velocities_from_cluster_weighted <- function(year_0, tab) {
  alpha <- 0.8
  tab_latlongs <- unique(tab[, list(latitude, longitude)])
  res_cluster <- c()
  for (j in 1:nrow(tab_latlongs)) {
    latitude_0 <- tab_latlongs$latitude[j]
    longitude_0 <- tab_latlongs$longitude[j]
    tab_filtered <- tab[(latitude == latitude_0) & (longitude == longitude_0), ]
    mean_U_0 <- alpha*tab_filtered$mean_U[nrow(tab_filtered)] +
      (1- alpha)*tab_filtered$mean_cluster_U[nrow(tab_filtered)]
    mean_V_0 <- alpha*tab_filtered$mean_V[nrow(tab_filtered)] +
      (1- alpha)*tab_filtered$mean_cluster_V[nrow(tab_filtered)]
    if (j == 1) {
      res_cluster <- data.frame(latitude = latitude_0,
                                longitude = longitude_0,
                                mean_U = mean_U_0,
                                mean_V = mean_V_0)
    } else {
      res_cluster <- rbind(res_cluster,
                           data.frame(latitude = latitude_0,
                                      longitude = longitude_0,
                                      mean_U = mean_U_0,
                                      mean_V = mean_V_0))
    }
  }
res_cluster
}

predict_velocities_from_cluster_ema <- function(year_0, tab) {
  tab_latlongs <- unique(tab[, list(latitude, longitude)])
  res_cluster <- c()
  for (j in 1:nrow(tab_latlongs)) {
    latitude_0 <- tab_latlongs$latitude[j]
    longitude_0 <- tab_latlongs$longitude[j]
    tab_filtered <- tab[(latitude == latitude_0) & (longitude == longitude_0), ]
    if (nrow(tab_filtered) == 1) {
      mean_U <- tab_filtered$mean_U[1]
      mean_V <- tab_filtered$mean_V[1]
    } else {
      mean_U_values <- tab_filtered$mean_U
      mean_V_values <- tab_filtered$mean_V
      mean_U_ema <- EMA(mean_U_values, 2)
      mean_V_ema <- EMA(mean_V_values, 2)
      year_cur <- tab_filtered$year[nrow(tab_filtered)]
      for (j in 1:(4*(-year_cur + year_0))-1) {
        mean_U_values <- c(mean_U_values, mean_U_ema[length(mean_U_ema)])
        mean_V_values <- c(mean_V_values, mean_V_ema[length(mean_V_ema)])
        mean_U_ema <- EMA(mean_U_values, 2)
        mean_V_ema <- EMA(mean_V_values, 2)
      }
      mean_U <- mean_U_ema[length(mean_U_ema)]
      mean_V <- mean_V_ema[length(mean_V_ema)]
    }
    if (j == 1) {
      res_cluster <- data.frame(latitude = latitude_0,
                                longitude = longitude_0,
                                mean_U = mean_U,
                                mean_V = mean_V)
    } else {
      res_cluster <- rbind(res_cluster,
                           data.frame(latitude = latitude_0,
                                      longitude = longitude_0,
                                      mean_U = mean_U,
                                      mean_V = mean_V))
    }
  }
res_cluster
}

predict_velocities_from_cluster_var <- function(year_0, tab) {
  tab_latlongs <- unique(tab[, list(latitude, longitude)])
  res_cluster <- c()
  for (j in 1:nrow(tab_latlongs)) {
    latitude_0 <- tab_latlongs$latitude[j]
    longitude_0 <- tab_latlongs$longitude[j]
    tab_filtered <- tab[(latitude == latitude_0) & (longitude == longitude_0), ]
    if (nrow(tab_filtered) == 1) {
      mean_U <- tab_filtered$mean_U[1]
      mean_V <- tab_filtered$mean_V[1]
    } else if (nrow(tab_filtered) == 2) {
      mean_U_values <- tab_filtered$mean_U
      mean_V_values <- tab_filtered$mean_V
      mean_U_ema <- EMA(mean_U_values, 2)
      mean_V_ema <- EMA(mean_V_values, 2)
      year_cur <- tab_filtered$year[nrow(tab_filtered)]
      for (j in 1:(4*(-year_cur + year_0))-1) {
        mean_U_values <- c(mean_U_values, mean_U_ema[length(mean_U_ema)])
        mean_V_values <- c(mean_V_values, mean_V_ema[length(mean_V_ema)])
        mean_U_ema <- EMA(mean_U_values, 2)
        mean_V_ema <- EMA(mean_V_values, 2)
      }
      mean_U <- mean_U_ema[length(mean_U_ema)]
      mean_V <- mean_V_ema[length(mean_V_ema)]
    } else {
      year_cur <- tab_filtered$year[nrow(tab_filtered)]
      n.ahead <- 4*(year_0 - year_cur) - 1
      res_U <- VAR(tab_filtered[, list(mean_U, std_dev_U, mean_cluster_U)],
                   p = 1, type = "none")
      res_V <- VAR(tab_filtered[, list(mean_V, std_dev_V, mean_cluster_V)],
                   p = 1, type = "none")
      predicted_values_U <- predict(res_U, n.ahead = n.ahead)
      predicted_values_V <- predict(res_V, n.ahead = n.ahead)
      model_U <- predicted_values_U$fcst$mean_U[length(predicted_values_U$fcst$mean_U)]
      model_V <- predicted_values_V$fcst$mean_V[length(predicted_values_U$fcst$mean_V)]
      if (is.na(model_U) || is.na(model_V)) {
        res <- VAR(tab_filtered[, list(mean_U, mean_V,
                                       mean_cluster_U, mean_cluster_V)],
                   p = 1, type = "none")
        predicted_values <- predict(res, n.ahead = n.ahead)
        model_U <- predicted_values$fcst$mean_U[length(predicted_values_U$fcst$mean_U)]
        model_V <- predicted_values$fcst$mean_V[length(predicted_values_U$fcst$mean_V)]
        if (is.na(model_U) || is.na(model_V)) {
          mean_U_values <- tab_filtered$mean_U
          mean_V_values <- tab_filtered$mean_V
          mean_U_ema <- EMA(mean_U_values, 2)
          mean_V_ema <- EMA(mean_V_values, 2)
          year_cur <- tab_filtered$year[nrow(tab_filtered)]
          for (j in 1:(4*(-year_cur + year_0))-1) {
            mean_U_values <- c(mean_U_values, mean_U_ema[length(mean_U_ema)])
            mean_V_values <- c(mean_V_values, mean_V_ema[length(mean_V_ema)])
            mean_U_ema <- EMA(mean_U_values, 2)
            mean_V_ema <- EMA(mean_V_values, 2)
          }
          mean_U <- mean_U_ema[length(mean_U_ema)]
          mean_V <- mean_V_ema[length(mean_V_ema)]
        }
      }
    }
    if (j == 1) {
      res_cluster <- data.frame(latitude = latitude_0,
                                longitude = longitude_0,
                                mean_U = mean_U,
                                mean_V = mean_V)
    } else {
      res_cluster <- rbind(res_cluster,
                           data.frame(latitude = latitude_0,
                                      longitude = longitude_0,
                                      mean_U = mean_U,
                                      mean_V = mean_V))
    }
  }
  res_cluster
}

approximate_latlong_by_cluster <- function(res_cluster, latitude_0, longitude_0) {
  res_cluster <- as.data.table(res_cluster)
  res_cluster[, weight := ((latitude - latitude_0)^2 +
                           (longitude - longitude_0)^2)^0.5]
  mean_U <- res_cluster[, sum(mean_U*weight)/sum(mean_U)]
  mean_V <- res_cluster[, sum(mean_V*weight)/sum(mean_V)]
c(mean_U, mean_V)
}
