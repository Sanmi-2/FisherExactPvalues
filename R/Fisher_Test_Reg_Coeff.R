#' Estimates Estimates Fisher's Exact P-values to test Additive Sharp Null Hypotheses with Covariates Using the Statistic T_reg_coeff
#' @description Estimates Estimates Fisher's Exact P-values to test Additive Sharp Null Hypotheses with Covariates Using the Statistic T_reg_coeff. See user's manual for a more detailed description.
#' @param W A vector that indicates treatment status. W takes the value of 1 for units that were treated and zero otherwise.
#' @param Y_obs A vector with the observed outcomes of the variable of interest.
#' @param N Number of randomly simulated treatment vectors used to obtain the distribution of the test statistic.
#' @param H0 Additive sharp null hypothesis that will be tested. If H0 = c, we will test the null that the treatment effect is equal to c for all units.
#' @param x A matrix of covariates to be used as control variables.
#' @return A list containing two elements. The first element specifies the p-value that we obtain from the hypothesis test. The second element of the list provides an approximation of the distribution of the test statistic.
#' @examples
#'
#' #This example is a continuation of the one provided in the documentation for the 'Fisher_Exact_Test' function
#'
#'Z <- as.matrix(cbind(pooled_hh$asset_tot_value_bsl,pooled_hh$ctotal_pcmonth_bsl,pooled_hh$cfood_pcmonth_bsl))
#'
#'Fisher_Test_Reg_Coeff(W=W, Y_obs = Y, N = 500, H0 = 0, x = Z)
#'



#' @export
Fisher_Test_Reg_Coeff <- function(W, Y_obs, N, H0, x) {


  T_Reg_Coeff <- function(W,Y_obs,x) {

    ones <- as.vector(rep(1, length(W)))

    X <- as.matrix(cbind(W,x,ones))

    beta_hat <- solve(t(X) %*% X) %*% ( t(X) %*% Y_obs )

    return( beta_hat[1] )

  }



  N_t <- sum(W)

  N_c <- length(W) - N_t

  W_mat <- matrix(0, length(W), N+1)

  W_mat[,1] <- W

  for (i in 1:N) {

    W_mat[,i+1] <-  sample(c(rep(0,N_c),rep(1,N_t)))

  }

  Y_mat <- matrix(0, length(W), N+1)

  Y_mat[,1] <- Y_obs


  for (i in 1:N) {


    for (j in 1:length(W)) {

      if (W_mat[,i+1][j] > W[j]) {

        Y_mat[j,i+1] <- (Y_mat[j,1] + H0)

      }

      if (W_mat[,i+1][j] < W[j]) {

        Y_mat[j,i+1] <- (Y_mat[j,1] - H0)

      }

      if (W_mat[,i+1][j] == W[j]) {

        Y_mat[j,i+1] <- (Y_mat[j,1])

      }

    }

  }


  T_vector <- as.vector(rep(0, N+1))


  T_vector <- as.vector(rep(0, N+1))

  T_vector[1] <- T_Reg_Coeff(W,Y_obs,x)

  for (i in 1:N) {

    T_vector[i+1] <- T_Reg_Coeff(W_mat[,i+1],Y_mat[,i+1],x)

  }


  ecdf_fun <- function(x,perc) ecdf(x)(perc)

  percentile <- ecdf_fun(T_vector,T_vector[1])

  if (percentile <= 0.5) {

    p_value <- percentile * 2

  }

  if (percentile > 0.5) {

    p_value <- ( 1 - percentile )*2

  }

  output <- list(p_value,T_vector)

  return(output)


}






