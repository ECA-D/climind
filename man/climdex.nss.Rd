% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.nss}
\alias{climdex.nss}
\title{New snow sum}
\usage{
climdex.nss(ci, freq = c("annual", "monthly", "halfyear", "seasonal"))
}
\arguments{
\item{ci}{Object of type climdexInput (representing the daily snowfall timeseries in [cm]).}

\item{freq}{Time frequency to aggregate to. Allowed are: "annual","halfyear" or "seasonal". Default: "annual".}
}
\value{
A vector containing the timeseries of the snowfall sum.
}
\description{
This function computes the climdex index NSS: The sum of all snowfall measured within a year.
}
\author{
Rebekka Posselt (MeteoSwiss)
}
