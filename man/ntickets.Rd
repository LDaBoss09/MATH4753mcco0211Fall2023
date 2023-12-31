% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{N-Tickets}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{Total number of seats on the flight}

\item{gamma}{Probability of overbooking}

\item{p}{Probability that a person shows up}
}
\value{
A named list containing all parameters and two plots displaying the optimal number of tickets (n) to sell for a discrete distribution and continuous approximation
}
\description{
N-Tickets
}
\examples{
ntickets(N=200, gamma=0.02, p=0.95)
}
