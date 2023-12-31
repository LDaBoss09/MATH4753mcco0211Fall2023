% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboots.R
\name{myboots}
\alias{myboots}
\title{My Bootstrapping Function}
\usage{
myboots(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{Number of iterations to bootstrap}

\item{x}{The sample to perform bootstrapping on}

\item{fun}{The function to be estimated}

\item{alpha}{The alpha error level}

\item{cx}{Size of plotting text and symbols}

\item{...}{Other parameters for the produced histogram}
}
\value{
A histogram of the sample statistics for each iteration followed by a point estimate for the population parameter and a 100(1-alpha)% confidence interval for the same parameter. Also a list of confidence intervals, functions, and samples.
}
\description{
My Bootstrapping Function
}
\examples{
myboots(iter=10000, x=c(1,2,3,4,5,6,7,8,9,10), fun='mean', alpha=0.05, cx=1.5)
}
