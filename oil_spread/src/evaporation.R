calculate_evaporation_rate <- function(mass_transfer, concentration,
                                       wind_speed, area, schmidt_number, r) {
  if ((r<0) || (r>2/3)) {stop('r must be between 0 and 2/3!')}
  evaporation_rate <- mass_transfer * concentration *
    (wind_speed)^{7/9} * (area^{-1/9}) * schmidt_number^{-r}
evaporation_rate
}

apply_evaporation <- function(model_res, mass_transfer_rate, schmidt_number,
                              evaporation_r, wind_speed, predictFuture) {
  wind_speed <- sqrt(wind_speed[1]^2 + wind_speed[2]^2)
  area <- determine_valid_area(model_res$obstacles)
  for (j in 2:nrow(model_res$model)) {
    model_res$model[j, ] <- apply_evaporation_to_time_moment(model_res$model[j, ],
                                                             mass_transfer_rate,
                                                             area,
                                                             schmidt_number,
                                                             evaporation_r,
                                                             3600*wind_speed,
                                                             predictFuture)
  }
model_res
}

determine_valid_area <- function(obstacles) {
  length(obstacles[obstacles>=0])
}

apply_evaporation_to_time_moment <- function(concentration,
                                             mass_transfer_rate,
                                             area,
                                             schmidt_number,
                                             evaporation_r,
                                             wind_speed,
                                             predictFuture) {
  if (length(concentration[concentration>0]) > 0) {
    koef <- mass_transfer_rate*mean(concentration[concentration>0])*wind_speed^{7/9}/
                  (area^{1/9}*schmidt_number^{evaporation_r})
    if (predictFuture) {
      return(min(max(1 - 0.3 * koef, 0), 1)*concentration)      
    } else {
      koef <- min(max(1 - 0.3 * koef, 0), 1)
      if (koef > 0) {
        return(concentration/koef)        
      } else {
        return(concentration)
      }
    }
  } else {
    return(concentration)
  }
}
