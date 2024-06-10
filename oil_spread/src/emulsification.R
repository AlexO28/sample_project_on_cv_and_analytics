calculate_emulsification_rate <- function(t, emulsion_coef, wind_speed, OC) {
  D <- emulsion_coef * (wind_speed + 1)^2/OC
  emulsification_rate <- max(1 - exp(-D*t)/D, 0)
emulsification_rate
}

apply_emulsification <- function(model_res, wind_speed, type_of_oil,
                                 predictFuture) {
  wind_speed <- sqrt(wind_speed[1]^2 + wind_speed[2]^2)
  emulsion_coef <- determine_emulsion_coef(type_of_oil)
  OC <- determine_OC(type_of_oil)
  for (j in 1:nrow(model_res$model)) {
    model_res$model[j, ] <- apply_emulsification_to_time_moment(model_res$model,
                                                                j,
                                                                emulsion_coef,
                                                                3600*wind_speed,
                                                                OC,
                                                                predictFuture)
  }
model_res
}

apply_emulsification_to_time_moment <- function(model_res, time,
                                                emulsion_coef,
                                                wind_speed, OC,
                                                predictFuture) {
  emulsification_rate <- calculate_emulsification_rate(time-1,
                                                       emulsion_coef,
                                                       wind_speed, OC)
  if (predictFuture) {
    return(model_res[time, ]*(1-0.3*emulsification_rate))
  } else {
    return(model_res[time, ]/(1-0.3*emulsification_rate))    
  }

}

determine_emulsion_coef <- function(type_of_oil) {
  if (type_of_oil == "лёгкая") {return(2 * 10^{-6})}
  if (type_of_oil == "тяжёлая") {return(4 * 10^{-6})}
  stop('Не поддерживаемый тип нефти!')
}

determine_OC <- function(type_of_oil) {
  if (type_of_oil == "лёгкая") {return(0.7)}
  if (type_of_oil == "тяжёлая") {return(1.15)}
  stop('Не поддерживаемый тип нефти!')
}
