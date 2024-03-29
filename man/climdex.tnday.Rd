% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.tnday}
\alias{climdex.tnday}
\alias{climdex.txndaymax}
\alias{climdex.txndaymin}
\alias{climdex.tnndaymax}
\alias{climdex.tnndaymin}
\alias{climdex.tmndaymax}
\alias{climdex.tmndaymin}
\title{Min/Max of mean n-day Min/Avg/Max.-Temperature}
\usage{
climdex.txndaymax(ci, ndays = 5, freq = c("monthly", "annual", "halfyear",
  "seasonal"), center.mean.on.last.day = FALSE)

climdex.txndaymin(ci, ndays = 5, freq = c("monthly", "annual", "halfyear",
  "seasonal"), center.mean.on.last.day = FALSE)

climdex.tnndaymax(ci, ndays = 5, freq = c("monthly", "annual", "halfyear",
  "seasonal"), center.mean.on.last.day = FALSE)

climdex.tnndaymin(ci, ndays = 5, freq = c("monthly", "annual", "halfyear",
  "seasonal"), center.mean.on.last.day = FALSE)

climdex.tmndaymax(ci, ndays = 5, freq = c("monthly", "annual", "halfyear",
  "seasonal"), center.mean.on.last.day = FALSE)

climdex.tmndaymin(ci, ndays = 5, freq = c("monthly", "annual", "halfyear",
  "seasonal"), center.mean.on.last.day = FALSE)
}
\arguments{
\item{ci}{Object of type climdexInput (representing daily Tmax/Tavg/Tmin).}

\item{ndays}{number of days to consider (default=5).}

\item{freq}{Time frequency to aggregate to (default="monthly").}

\item{center.mean.on.last.day}{Whether to center the n-day running mean on
the last day of the window, instead of the center day.}
}
\description{
This functions compute the climdex indeces Txndaymax, Txndaymin, 
                                           Tnndaymax, Tnndaymin, 
                                           Tmndaymax, Tmndaymin.

This function takes a climdexInput object as input and computes the climdex
index T[x|m|n]nday[min|max]: min/max of mean n-day [max|avg|min] temperature.
}
\author{
Rebekka Posselt (MeteoSwiss)
}
