EPS <- 0.000001

calculate_waves_speed <- function(water_depth, wind_speed, fetch_length) {
  alpha <- tanh(0.53*(g*water_depth/(wind_speed + EPS)^2)^{0.75})
  beta <- tanh(0.833*(g*water_depth/(wind_speed + EPS)^2)^{0.375})
  wave_height <- 0.283*alpha*(wind_speed^2/g)*
    tanh((0.0125/alpha)*(g*fetch_length/((wind_speed + EPS)^2))^{0.42})
  wave_period <- 7.54*beta*(wind_speed/g)*
    tanh((0.077/beta)*(g*fetch_length/((wind_speed + EPS)^2))^{0.25})
  t <- (wave_height/g)^{0.5}
  wave_speed <- (wave_period/(2*(t + EPS)))
wave_speed
}

derive_waves_speed <- function(statistical_data, wind_speed, fetch_length) {
  depth <- statistical_data$Depth[1]
  wind_speed <- sqrt(wind_speed[1]^2 + wind_speed[2]^2)
  waves_speed <- calculate_waves_speed(depth, wind_speed, fetch_length)
waves_speed
}

calculate_waves_contributions <- function(velocity1, velocity2, waves_speed) {
  share_1 <- velocity1/sqrt(velocity1^2 + velocity2^2 + EPS)
  share_2 <- velocity2/sqrt(velocity1^2 + velocity2^2 + EPS)
  waves_speed_1 <- waves_speed*share_1
  waves_speed_2 <- waves_speed*share_2
c(waves_speed_1, waves_speed_2)
}
