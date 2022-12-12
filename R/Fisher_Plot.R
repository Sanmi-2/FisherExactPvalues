#' Plots histograms and kernel densities of the distribution of the test statistic.
#'
#' @param output An object of class "Fisher_Exact_Test" or "Multiple_Tests". Takes in the output of the function Fisher_Exact_Test() or Multiple_Tests().
#' @param plot_type Specifies if the plot will be a histogram or a kernel density. Options are "Histogram" and "Kernel".
#' @return Returns a plot of the specified type. The purple line marks the value of the observed test statistic.
#' @examples
#' ###This example is a continuation of the example for the documentation of the function "Fisher_Exact_Test"
#'
#' Fisher_Plot(First_Test,plot_type = "Histogram")
#'
#'Fisher_Plot(First_Test,plot_type = "Kernel")


#' @export
Fisher_Plot <- function(output,plot_type) {

  T_dist <- unlist(output[2])

  if (plot_type == "Histogram") {

    hist(T_dist, main = "Histogram of the Statistic", xlab = "Value of T", ylab = "Count", col = "grey")

    abline(v = T_dist[1], col="purple")
  }


  if (plot_type == "Kernel") {

    plot(density(T_dist), main = "Kernel Density of the Statistic")
    polygon(density(T_dist), col="light grey", border="dark grey")
    abline(v = T_dist[1], col="purple")
  }



}
