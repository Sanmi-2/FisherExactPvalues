
#' Combines two different test statistics to perform a joint hypothesis test.
#'
#' @param test_1 An object of class "Fisher_Exact_Test". Takes in the output of the function Fisher_Exact_Test.
#' @param test_2 An object of class "Fisher_Exact_Test". Takes in the output of the function Fisher_Exact_Test.
#' @param fun The function for the test statistic. Options are "Mean" and "Max". See user manual for more information.
#' @return A list containing two elements. The first element specifies the p-value that we obtain from the hypothesis test. The second element of the list provides an approximation of the distribution of the test statistic.
#' @examples
#' ###This example is a continuation of the example for the documentation of the function "Fisher_Exact_Test"
#'
#' First_Test <- Fisher_Exact_Test(fun = "Diff", W=W, Y_obs = Y, N = 1000, H0 = 0)
#'
#' Second_Test <- Fisher_Exact_Test(fun = "Kolmogorov-Smirnov", W=W, Y_obs = Y_2, N = 1000, H0 = 1)
#'
#' Multiple_Tests(First_Test,Second_Test, fun = "Mean")
#'



#' @export
Multiple_Tests <- function(test_1,test_2,fun) {

  T_1 <- unlist(test_1[2])

  T_2 <- unlist(test_2[2])

  if (fun == "Mean") {

    T_comb <- .5 * T_1 + 0.5 * T_2

  }

  if (fun == "Max") {

    T_comb <- as.vector(rep(0, length(T_1)))

    for (i in 1:length(T_1)) {

      T_comb[i] <- max(T_1[i],T_2[i])

    }


  }


  ecdf_fun <- function(x,perc) ecdf(x)(perc)

  percentile <- ecdf_fun(T_comb,T_comb[1])

  if (percentile <= 0.5) {

    p_value <- percentile * 2

  }

  if (percentile > 0.5) {

    p_value <- ( 1 - percentile )*2

  }

  output <- list(p_value,T_comb)

  return(output)


}

