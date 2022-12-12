#' Estimates Fisher's Exact P-values to test Additive Sharp Null Hypotheses
#'
#' @param fun Specifies the function that will be used for the test statistic. Options are "Diff", "Log", "Median", "T-statistic", and "Kolmogorov-Smirnov". See user manual for further details.
#' @param W A vector that indicates treatment status. W takes the value of 1 for units that were treated and zero otherwise.
#' @param Y_obs A vector with the observed outcomes of the variable of interest.
#' @param N Number of randomly simulated treatment vectors used to obtain the distribution of the test statistic.
#' @param H0 Additive sharp null hypothesis that will be tested. If H0 = c, we will test the null that the treatment effect is equal to c for all units.
#' @return A list containing two elements. The first element specifies the p-value that we obtain from the hypothesis test. The second element of the list provides an approximation of the distribution of the test statistic.
#' @examples
#' pooled_hh <- read_dta("/data_modified/data_modified/pooled_hh.dta")
#'
#'pooled_hh <- pooled_hh[!(pooled_hh$ctotal_pcmonth_end == 0),]
#'pooled_hh <- pooled_hh[!(pooled_hh$cfood_pcmonth_end== 0),]
#'
#'pooled_hh$Expenditure <- log(pooled_hh$ctotal_pcmonth_end)
#'
#'pooled_hh$Food_Expenditure <- log(pooled_hh$cfood_pcmonth_end)
#'
#'pooled_hh <- pooled_hh[!(is.na(pooled_hh$treatment)),]
#'
#'pooled_hh <- pooled_hh[!(is.na(pooled_hh$Expenditure)),]
#'
#'pooled_hh <- pooled_hh[!(is.na(pooled_hh$Food_Expenditure)),]
#'
#'Y <- pooled_hh$Expenditure

#'W <- pooled_hh$treatment
#'
#'###########Test H0: treatment effect = 0 for all units with T(W,Y_obs) = T^{diff}
#'First_Test <- Fisher_Exact_Test(fun = "Diff", W=W, Y_obs = Y, N = 1000, H0 = 0)
#'
#'
#'##########Test H0: treatment effect on household's food expenditure was equal to 1 for all observations
#'Y_2 <- pooled_hh$Food_Expenditure
#'
#'W <- pooled_hh$treatment
#'
#'Second_Test <- Fisher_Exact_Test(fun = "Kolmogorov-Smirnov", W=W, Y_obs = Y_2, N = 1000, H0 = 1)
#'
#'print("the p-value is equal to")
#'
#'Second_Test[1]
#'
#'
#'
#'




#' @export
Fisher_Exact_Test <- function(fun, W, Y_obs, N, H0) {


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

  if (percentile <= 0.5) {

    p_value <- percentile * 2

  }

  if (percentile > 0.5) {

    p_value <- ( 1 - percentile )*2

  }

  output <- list(p_value,T_vector)

  return(output)


}



