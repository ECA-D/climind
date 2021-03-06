% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.ddsouth}
\alias{climdex.ddsouth}
\title{Southerly winds}
\usage{
climdex.ddsouth(ci, freq = c("annual", "halfyear", "seasonal", "monthly"))
}
\arguments{
\item{ci}{Object of type climdexInput (representing the daily mean wind direction in [deg] with 180deg being wind from south)}

\item{freq}{Time frequency to aggregate to. Allowed are: "annual","halfyear", "seasonal" or "monthly". Default: "annual".}
}
\value{
A vector containing the time series of days with southerly winds.
}
\description{
This function computes the climdex index DDsouth: Days with southerly wind (135deg < wind_dir <= 225deg).
}
\references{
\url{http://www.ecad.eu/indicesextremes/indicesdictionary.php}
}
\author{
Rebekka Posselt (MeteoSwiss)
}
