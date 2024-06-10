library(data.table)
library(TTR)
library(vars)
library(dbscan)

get_data_on_currents <- function(file.path) {
  tab <- fread(file.path)
  names(tab) <- c("id", "latitude", "longitude", "instrument_depth",
                  "year_of_initial_deployment",
                  "number_of_months_of_deployment",
                  "mean_U", "mean_V", "std_dev_U", "std_dev_V", "nominal_depth")
  tab <- tab[, by=list(id, year_of_initial_deployment,
                       number_of_months_of_deployment),
               list(mean(instrument_depth), mean(mean_U), mean(mean_V),
                    mean(std_dev_U), mean(std_dev_V), mean(nominal_depth),
                    mean(latitude), mean(longitude))]
  names(tab)[4:11] <- c("instrument_depth", "mean_U", "mean_V",
                        "std_dev_U", "std_dev_V", "nominal_depth",
                        "latitude", "longitude")
  tab <- as.data.frame(tab)
  tab <- tab[order(tab$id,
                   tab$year_of_initial_deployment,
                   tab$number_of_months_of_deployment,
                   decreasing = c(FALSE, FALSE, FALSE)),]
  tab <- as.data.table(tab)
  tab[, ID := 1:nrow(tab)]
  tab_agg <- tab[, by = list(id), number_of_years := .N]
  tab_agg <- tab_agg[, by = list(id), max_id := max(ID)]
  tab_agg <- tab_agg[, by = list(id), min_id := min(ID)]
  tab_unique <- unique(tab_agg[, list(latitude, longitude)])
  clusters <- hdbscan(tab_unique, minPts = 5)
  tab_unique$cluster <- clusters$cluster
  setkeyv(tab_agg, c("latitude", "longitude"))
  setkeyv(tab_unique, c("latitude", "longitude"))
  tab_agg <- tab_agg[tab_unique]
  tab_agg <- tab_agg[, by = list(cluster, year_of_initial_deployment),
                       mean_cluster_U := mean(mean_U)]
  tab_agg <- tab_agg[, by = list(cluster, year_of_initial_deployment),
                       mean_cluster_V := mean(mean_V)]
list(tab_one_year = tab_agg[number_of_years == 1, ],
     tab_multi_years = tab_agg[number_of_years > 1, ])
}

split_currents_data <- function(tab_multi_years) {
  list(train = tab_multi_years[max_id != ID, ],
       test = tab_multi_years[max_id == ID, ])
}

baseline_model_on_train <- function(train) {
  train <- as.data.table(as.data.frame(train))
  train_next <- train[ID > min_id, list(id, ID, mean_U, mean_V)]
  train <- train[, list(id, ID, mean_U, mean_V)]
  setkey(train_next, "id")
  setkey(train, "id")
  joined <- train[train_next, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID == i.ID - 1,]
joined
}

weighted_model_on_train <- function(train, alpha) {
  train <- as.data.table(as.data.frame(train))
  train_next <- train[ID > min_id, list(id, ID, mean_U, mean_V)]
  train <- train[, list(id, ID, mean_U, mean_V, mean_cluster_U, mean_cluster_V)]
  setkey(train_next, "id")
  setkey(train, "id")
  joined <- train[train_next, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID == i.ID - 1,]
  joined[, mean_U := alpha*mean_U + (1-alpha)*mean_cluster_U]
  joined[, mean_V := alpha*mean_V + (1-alpha)*mean_cluster_V]
joined
}

baseline_model_on_test <- function(test, train) {
  test <- as.data.table(as.data.frame(test))
  train <- as.data.table(as.data.frame(train))
  test <- test[, list(id, ID, mean_U, mean_V)]
  train <- train[, list(id, ID, mean_U, mean_V)]
  setkey(test, "id")
  setkey(train, "id")
  joined <- train[test, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID == i.ID - 1,]
joined
}

weighted_model_on_test <- function(test, train, alpha = 0.8) {
  test <- as.data.table(as.data.frame(test))
  train <- as.data.table(as.data.frame(train))
  test <- test[, list(id, ID, mean_U, mean_V, mean_cluster_U, mean_cluster_V)]
  train <- train[, list(id, ID, mean_U, mean_V)]
  setkey(test, "id")
  setkey(train, "id")
  joined <- train[test, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID == i.ID - 1,]
  joined[, mean_U := alpha*mean_U + (1-alpha)*mean_cluster_U]
  joined[, mean_V := alpha*mean_V + (1-alpha)*mean_cluster_V]
joined
}

calculate_statistics <- function(res) {
  res[, (sum(abs(mean_U - i.mean_U)/(0.0001 + abs(i.mean_U) + abs(mean_U))) +
         sum(abs(mean_V - i.mean_V)/(0.0001 + abs(mean_V) + abs(i.mean_V))))/
      (2*nrow(res))]
}

prepare_train_data <- function(train) {
  train <- as.data.table(as.data.frame(train))
  train_next <- train[ID > min_id, ]
  setkey(train_next, "id")
  setkey(train, "id")
  joined <- train_next[train, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID - 1 == i.ID,]
joined
}

prepare_test_data <- function(test, train) {
  train <- as.data.table(as.data.frame(train))
  test <- as.data.table(as.data.frame(test))
  setkey(test, "id")
  setkey(train, "id")
  joined <- test[train, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID - 1 == i.ID,]
joined
}

interpretable_model_on_train <- function(train, alpha) {
  train <- as.data.table(as.data.frame(train))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V,
                                               std_dev_U, std_dev_V)]
    if (nrow(train_for_id) == 1) {
      train_for_id$i.mean_U <- NA
      train_for_id$i.mean_V <- NA
    } else if (nrow(train_for_id) == 2) {
      train_for_id$i.mean_U <- c(NA, train_for_id$mean_U[1])
      train_for_id$i.mean_V <- c(NA, train_for_id$mean_V[1])
    } else {
      train_for_id$mean_U_prev <-
        c(NA, train_for_id$mean_U[1:(nrow(train_for_id) - 1)])
      train_for_id$mean_V_prev <-
        c(NA, train_for_id$mean_V[1:(nrow(train_for_id) - 1)])
      train_for_id$std_dev_U_prev <-
        c(NA, train_for_id$std_dev_U[1:(nrow(train_for_id) - 1)])
      train_for_id$std_dev_V_prev <-
        c(NA, train_for_id$std_dev_V[1:(nrow(train_for_id) - 1)])
      train_for_id$mean_U_prev_2 <-
        c(NA, NA, train_for_id$mean_U[1:(nrow(train_for_id) - 2)])
      train_for_id$mean_V_prev_2 <-
        c(NA, NA, train_for_id$mean_V[1:(nrow(train_for_id) - 2)])
      train_for_id$i.mean_U <- train_for_id$mean_U_prev +
        sign(train_for_id$mean_U_prev - train_for_id$mean_U_prev_2) *
        train_for_id$std_dev_U_prev * alpha
      train_for_id$i.mean_V <- train_for_id$mean_V_prev + 
        sign(train_for_id$mean_V_prev - train_for_id$mean_V_prev_2) *
        train_for_id$std_dev_V_prev * alpha
      train_for_id$mean_U_prev <- NULL
      train_for_id$mean_V_prev <- NULL
      train_for_id$mean_U_prev_2 <- NULL
      train_for_id$mean_V_prev_2 <- NULL
      train_for_id$std_dev_U_prev <- NULL
      train_for_id$std_dev_V_prev <- NULL
    }
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
results
}

interpretable_model_on_test <- function(test, train) {
  train <- as.data.table(as.data.frame(train))
  test <- as.data.table(as.data.frame(test))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V,
                                               std_dev_U, std_dev_V)]
    if (nrow(train_for_id) == 1) {
      model_U  <- train_for_id$mean_U
      model_V  <- train_for_id$mean_V
    } else {
      model_U <- train_for_id$mean_U[nrow(train_for_id)] +
        sign(train_for_id$mean_U[nrow(train_for_id)] -
               train_for_id$mean_U[nrow(train_for_id)-1]) *
        train_for_id$std_dev_U[nrow(train_for_id)] * 0.025
      model_V <- train_for_id$mean_V[nrow(train_for_id)] +
        sign(train_for_id$mean_V[nrow(train_for_id)] -
               train_for_id$mean_V[nrow(train_for_id)-1]) *
        train_for_id$std_dev_V[nrow(train_for_id)] * 0.025
      
    }
    train_for_id <- data.frame(id = id_fixed,
                               ID = max(train_for_id$ID),
                               mean_U = model_U,
                               mean_V = model_V)
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
  test <- test[, list(id, ID, mean_U, mean_V)]
  results <- as.data.table(results)
  setkey(test, "id")
  setkey(results, "id")
  joined <- test[results, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID - 1 == i.ID,]
joined
}

ema_model_for_train <- function(train, n = 2) {
  train <- as.data.table(as.data.frame(train))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V)]
    if ((nrow(train_for_id) > 1) & (nrow(train_for_id) <= n)) {
      ema_U <- c(NA, train_for_id$mean_U[1:(nrow(train_for_id) - 1)])
      ema_V <- c(NA, train_for_id$mean_V[1:(nrow(train_for_id) - 1)])
    } else if (nrow(train_for_id) == 1) {
      ema_U <- NA
      ema_V <- NA
    } else {
      ema_U <- EMA(train_for_id$mean_U, n = min(n, nrow(train_for_id)-1))
      for (j in 1:length(ema_U)) {
        if (is.na(ema_U[j])) {
          ema_U[j] <- train_for_id$mean_U[j]
        }
      }
      ema_U <- c(NA, ema_U[1:(length(ema_U) - 1)])
      ema_V <- EMA(train_for_id$mean_V, n = min(n, nrow(train_for_id)-1))
      for (j in 1:length(ema_V)) {
        if (is.na(ema_V[j])) {
          ema_V[j] <- train_for_id$mean_V[j]
        }
      }
      ema_V <- c(NA, ema_V[1:(length(ema_V) - 1)])
    }
    train_for_id$ema_U <- ema_U
    train_for_id$ema_V <- ema_V
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
  results <- as.data.table(results)
  results <- results[!(is.na(ema_U)), ]
  names(results)[names(results) == "ema_U"] <- "i.mean_U"
  names(results)[names(results) == "ema_V"] <- "i.mean_V"
results
}


ema_model_for_test <- function(test, train, n = 2) {
  test <- as.data.table(as.data.frame(test))
  train <- as.data.table(as.data.frame(train))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V)]
    if (nrow(train_for_id) <= 1) {
      ema_U <- train_for_id$mean_U[length(train_for_id$mean_U)]
      ema_V <- train_for_id$mean_V[length(train_for_id$mean_V)]
    } else {
      ema_U <- EMA(train_for_id$mean_U, n = min(n, nrow(train_for_id)))
      for (j in 1:length(ema_U)) {
        if (is.na(ema_U[j])) {
          ema_U[j] <- train_for_id$mean_U[j]
        }
      }
      ema_U <- ema_U[length(ema_U)]
      ema_V <- EMA(train_for_id$mean_V, n = min(n, nrow(train_for_id)))
      for (j in 1:length(ema_V)) {
        if (is.na(ema_V[j])) {
          ema_V[j] <- train_for_id$mean_V[j]
        }
      }
      ema_V <- ema_V[length(ema_V)]
    }
    train_for_id <- data.frame(id = id_fixed,
                               ID = max(train_for_id$ID),
                               ema_U = ema_U,
                               ema_V = ema_V)
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
  test <- test[, list(id, ID, mean_U, mean_V)]
  results <- as.data.table(results)
  setkey(test, "id")
  setkey(results, "id")
  joined <- test[results, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID - 1 == i.ID,]
  names(joined)[names(joined) == "ema_U"] <- "i.mean_U"
  names(joined)[names(joined) == "ema_V"] <- "i.mean_V"
joined
}

var_model_for_train <- function(train) {
  train <- as.data.table(as.data.frame(train))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V,
                                               std_dev_U, std_dev_V,
                                               nominal_depth,
                                               mean_cluster_U, mean_cluster_V)]
    if (nrow(train_for_id) == 1) {
      model_U <- NA
      model_V <- NA
    } else if (nrow(train_for_id) == 2) {
      model_U <- c(NA, train_for_id$mean_U[1])
      model_V <- c(NA, train_for_id$mean_V[1])
    } else {
      p <- 1
      res_U <- VAR(train_for_id[, list(mean_U, std_dev_U, mean_cluster_U)],
                   p = 1, type = "none")
      res_V <- VAR(train_for_id[, list(mean_V, std_dev_V, mean_cluster_V)],
                   p = 1, type = "none")
      fitted_values_U <- fitted(res_U)
      fitted_values_V <- fitted(res_V)
      model_U <- c(NA, fitted_values_U[, 1])
      model_V <- c(NA, fitted_values_V[, 1])
    }
    train_for_id$i.mean_U <- model_U
    train_for_id$i.mean_V <- model_V
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
results
}

var_model_for_test <- function(test, train) {
  test <- as.data.table(as.data.frame(test))
  train <- as.data.table(as.data.frame(train))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V,
                                               std_dev_U, std_dev_V,
                                               nominal_depth,
                                               mean_cluster_U, mean_cluster_V)]
    if (nrow(train_for_id) == 1) {
      model_U <- train_for_id$mean_U
      model_V <- train_for_id$mean_V
    } else if (nrow(train_for_id) == 2) {
      model_U <- train_for_id$mean_U[2]
      model_V <- train_for_id$mean_V[2]
    } else {
      res_U <- VAR(train_for_id[, list(mean_U, std_dev_U, mean_cluster_U)],
                 p = 1, type = "none")
      res_V <- VAR(train_for_id[, list(mean_V, std_dev_V, mean_cluster_V)],
                   p = 1, type = "none")
      predicted_values_U <- predict(res_U, n.ahead = 1)
      predicted_values_V <- predict(res_V, n.ahead = 1)
      model_U <- predicted_values_U$fcst$mean_U[1]
      model_V <- predicted_values_V$fcst$mean_V[1]
      if (is.na(model_U)) {
        p <- 1
        res <- VAR(train_for_id[, list(mean_U, mean_V,
                                       mean_cluster_U, mean_cluster_V)],
                   p = p, type = "none")
        predicted_values <- predict(res, n.ahead = 1)
        model_U <- predicted_values$fcst$mean_U[1]
        model_V <- predicted_values$fcst$mean_V[1]
      }
      if (is.na(model_U)) {
        model_U <- train_for_id$mean_U
      }
      if (is.na(model_V)) {
        model_V <- train_for_id$mean_V
      }
    }
    train_for_id <- data.frame(id = id_fixed,
                               ID = max(train_for_id$ID),
                               mean_U = model_U,
                               mean_V = model_V)
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
  test <- test[, list(id, ID, mean_U, mean_V)]
  results <- as.data.table(results)
  setkey(test, "id")
  setkey(results, "id")
  joined <- test[results, nomatch = 0, allow.cartesian=TRUE]
  joined <- joined[ID - 1 == i.ID,]
joined
}

sma_model_for_train <- function(train, n = 3) {
  train <- as.data.table(as.data.frame(train))
  ids <- unique(train$id)
  results <- c()
  for (id_fixed in ids) {
    train_for_id <- train[id == id_fixed, list(id, ID, mean_U, mean_V)]
    if (nrow(train_for_id) < n) {
      sma_U <- train_for_id$mean_U
      sma_V <- train_for_id$mean_V
    } else {
      sma_U <- SMA(train_for_id$mean_U, n = n)
      sma_U[is.na(sma_U)] <- train_for_id$mean_U[is.na(sma_U)]
      sma_V <- SMA(train_for_id$mean_V, n = n)
      sma_V[is.na(sma_V)] <- train_for_id$mean_V[is.na(sma_V)]
    }
    train_for_id$sma_U <- sma_U
    train_for_id$sma_V <- sma_V
    if (length(results) == 0) {
      results <- train_for_id
    } else {
      results <- rbind(results, train_for_id)
    }
  }
  results <- as.data.table(results)
results
}

blend_two_models <- function(res_1, res_2) {
  res_1 <- res_1[, list(ID, i.ID, mean_U, mean_V)]
  names(res_1) <- c("ID", "i.ID", "model_U", "model_V")
  setkey(res_1, "i.ID")
  names(res_2)[names(res_2) == "ID"] <- "i.ID"
  setkey(res_2, "i.ID")
  res <- res_2[res_1]
  res[, i.mean_V := (i.mean_V + model_V)/2]
  res[, i.mean_U := (i.mean_U + model_U)/2]
res
}

blend_three_models <- function(res_1, res_2, res_3) {
  res_1 <- res_1[, list(ID, i.ID, mean_U, mean_V)]
  names(res_1) <- c("ID", "i.ID", "model_U", "model_V")
  setkey(res_1, "i.ID")
  names(res_2)[names(res_2) == "ID"] <- "i.ID"
  setkey(res_2, "i.ID")
  res_3 <- res_3[, list(ID, i.mean_U, i.mean_V)]
  names(res_3) <- c("i.ID", "model_U_alt", "model_V_alt")
  setkey(res_3, "i.ID")
  res_2 <- res_2[res_3]
  res <- res_2[res_1]
  res[, i.mean_V := (i.mean_V + model_V + model_V_alt)/3]
  res[, i.mean_U := (i.mean_U + model_U + model_U_alt)/3]
res
}
