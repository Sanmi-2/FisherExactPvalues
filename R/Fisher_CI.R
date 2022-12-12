#' Estimates 'Fisher' Confidence Intervals for a Common Additive Treatment Effect.
#' @description Estimates 'Fisher' Confidence Intervals for a Common Additive Treatment Effect. To use this function, the user must provide upper and lower bounds for the area over which to search the confidence interval. The area between the upper and lower bounds is divided in a grid of 1000 points, and the function estimates the percentile of the observed test statistic under H0: additive treatment effect = c for all point c in the grid. The boundaries of the confidence interval are given by the points of the grid c_l and c_h such that under H0: treatment effect = c_l, the observed test statistic lies in the alpha/2 percentile, and under H0: treatment effect = c_h the observed test statistic lies in the 1-alpha/2 percentile. See user's manual for a more detailed description.
#' @param fun Specifies the function that will be used for the test statistic. Options are "Diff", "Log", "Median", "T-statistic", and "Kolmogorov-Smirnov". See user manual for further details.
#' @param W A vector that indicates treatment status. W takes the value of 1 for units that were treated and zero otherwise.
#' @param Y_obs A vector with the observed outcomes of the variable of interest.
#' @param N Number of randomly simulated treatment vectors used to obtain the distribution of the test statistic.
#' @param lower Lower bound for the interval in which the function will search for the upper and lower bounds of the confidence interval.
#' @param upper Upper bound for the interval in which the function will search for the upper and lower bounds of the confidence interval.
#' @param alpha Specifies the significance level of the confidence intervals. For instance, if alpha = 0.05, the function will provide the boundaries for the 95 percent confidence intervals.
#' @return Returns a list containing four elements. The first and second elements correspond to the lower and upper bounds of the confiedence interval, respectively. The third and fourth elements of the list correspond to the p-values associated with the null that the treatment effect is equal to the lower and upper bounds, respectively. If the function found points in the grid that approximate the true upper and lower bounds well, these p-values should both be close to alpha.
#'




#' @export
Fisher_CI <- function(fun, W, Y_obs, N, lower, upper, alpha) {


  if (fun == "Diff") {

    T_stat <- T_diff

  }

  if (fun == "Log") {

    T_stat <- T_log

  }

  if (fun == "Median") {

    T_stat <- T_median

  }

  if (fun == "T-statistic") {

    T_stat <- T_tstat

  }

  if (fun == "Kolmogorov-Smirnov") {

    T_stat <- T_ks

  }

  data_ <- as.data.frame( cbind(W,Y_obs) )

  N_t <- sum(W)

  N_c <- length(W) - N_t

  W_mat <- matrix(0, length(W), N+1)

  W_mat[,1] <- W

  for (i in 1:N) {

    W_mat[,i+1] <-  sample(c(rep(0,N_c),rep(1,N_t)))

  }


  percentile_fun <- function(H0) {

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

    T_vector[1] <- T_stat(W,Y_obs)

    for (i in 1:N) {

      T_vector[i+1] <- T_stat(W_mat[,i+1],Y_mat[,i+1])

    }


    ecdf_fun <- function(x,perc) ecdf(x)(perc)

    percentile <- ecdf_fun(T_vector,T_vector[1])

    return(percentile)

  }


  xgrid <- seq(from=lower,to=upper,len=1000)


  vector_ <- as.vector(rep(0, 1000))

  for (i in 1:1000) {

    vector_[i] <- percentile_fun(xgrid[i])

  }

  lower_bound_entry <- which.min(abs(vector_ - alpha/2))

  upper_bound_entry <- which.min(abs(vector_ - (1 - alpha/2)))

  lower_bound <- xgrid[lower_bound_entry]

  upper_bound <- xgrid[upper_bound_entry]


  p_val_lower <- percentile_fun(lower_bound)*2

  p_val_upper <- ( 1 - percentile_fun(upper_bound) )*2

  output <- list(lower_bound,upper_bound, p_val_lower,p_val_upper)

  return(output)

}

