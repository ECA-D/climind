% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.ddeast}
\alias{climdex.ddeast}
\title{Easterly winds}
\usage{
climdex.ddeast(ci, freq = c("annual", "halfyear", "seasonal", "monthly"))
}
\arguments{
\item{ci}{Object of type climdexInput (representing the daily mean wind direction in [deg] with 90deg being wind from east)}

\item{freq}{Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".}
}
\value{
A vector containing the time series of days with easterly winds.
}
\description{
This function computes the climdex index DDeast: Days with easterly wind (45deg < wind_dir <= 135deg).
}
\references{
\url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
}
\author{
Rebekka Posselt (MeteoSwiss)
}
