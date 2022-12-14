% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fisher_CI.R
\name{Fisher_CI}
\alias{Fisher_CI}
\title{Estimates 'Fisher' Confidence Intervals for a Common Additive Treatment Effect.}
\usage{
Fisher_CI(fun, W, Y_obs, N, lower, upper, alpha)
}
\arguments{
\item{fun}{Specifies the function that will be used for the test statistic. Options are "Diff", "Log", "Median", "T-statistic", and "Kolmogorov-Smirnov". See user manual for further details.}

\item{W}{A vector that indicates treatment status. W takes the value of 1 for units that were treated and zero otherwise.}

\item{Y_obs}{A vector with the observed outcomes of the variable of interest.}

\item{N}{Number of randomly simulated treatment vectors used to obtain the distribution of the test statistic.}

\item{lower}{Lower bound for the interval in which the function will search for the upper and lower bounds of the confidence interval.}

\item{upper}{Upper bound for the interval in which the function will search for the upper and lower bounds of the confidence interval.}

\item{alpha}{Specifies the significance level of the confidence intervals. For instance, if alpha = 0.05, the function will provide the boundaries for the 95 percent confidence intervals.}
}
\value{
Returns a list containing four elements. The first and second elements correspond to the lower and upper bounds of the confiedence interval, respectively. The third and fourth elements of the list correspond to the p-values associated with the null that the treatment effect is equal to the lower and upper bounds, respectively. If the function found points in the grid that approximate the true upper and lower bounds well, these p-values should both be close to alpha.
}
\description{
Estimates 'Fisher' Confidence Intervals for a Common Additive Treatment Effect. To use this function, the user must provide upper and lower bounds for the area over which to search the confidence interval. The area between the upper and lower bounds is divided in a grid of 1000 points, and the function estimates the percentile of the observed test statistic under H0: additive treatment effect = c for all point c in the grid. The boundaries of the confidence interval are given by the points of the grid c_l and c_h such that under H0: treatment effect = c_l, the observed test statistic lies in the alpha/2 percentile, and under H0: treatment effect = c_h the observed test statistic lies in the 1-alpha/2 percentile. See user's manual for a more detailed description.
}
