#############Contains the different statistic functions

T_diff <- function(W,Y_obs) {

  data <- as.data.frame( cbind(W,Y_obs) )

  Y_t <- mean(data[data$W == 1, 'Y_obs'])

  Y_c <- mean(data[data$W == 0, 'Y_obs'])

  return( abs(Y_t - Y_c) )

}


T_log <- function(W,Y_obs) {

  data <- as.data.frame( cbind(W,Y_obs) )

  data$Y_obs <- log(data$Y_obs)

  Y_t <- mean(data[data$W == 1, 'Y_obs'])

  Y_c <- mean(data[data$W == 0, 'Y_obs'])

  return( abs(Y_t - Y_c) )

}


T_median <- function(W,Y_obs) {

  data <- as.data.frame( cbind(W,Y_obs) )

  Y_t <- median(data[data$W == 1, 'Y_obs'])

  Y_c <- median(data[data$W == 0, 'Y_obs'])

  return( abs(Y_t - Y_c) )

}


T_tstat <- function(W,Y_obs) {

  data <- as.data.frame( cbind(W,Y_obs) )

  Y_t <- mean(data[data$W == 1, 'Y_obs'])

  Y_c <- mean(data[data$W == 0, 'Y_obs'])

  S_c_sq <- var(data[data$W == 1, 'Y_obs'])

  S_t_sq <- var(data[data$W == 0, 'Y_obs'])

  N_t <- sum(data$W)

  N_c <- length(W) - N_t

  T_ <- abs(  (Y_t - Y_c)/sqrt(S_c_sq/N_c + S_t_sq/N_t )   )

  return( T_ )

}


T_ks <- function(W,Y_obs) {

  data <- as.data.frame( cbind(W,Y_obs) )

  data_t <- data[!(data$W == 0),]

  data_c <- data[!(data$W == 1),]

  N_t <- sum(data$W)

  N_c <- length(W) - N_t

  F_c <- function(y) {

    1/N_c * sum(1*(data_c$Y_obs <= y))

  }

  F_t <- function(y) {

    1/N_t * sum(1*(data_t$Y_obs <= y))

  }

  vector <- rep(0, length(data$W))

  for (i in 1:length(data$W))  {

    vector[i] <- F_t(data$Y_obs[i]) - F_c(data$Y_obs[i])

  }


  T_ <- max(vector)

  return(T_)

}





