% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.r20mm}
\alias{climdex.r20mm}
\title{Precipitation Exceeding 20mm Per Day}
\usage{
climdex.r20mm(ci, freq = c("monthly", "annual", "halfyear", "seasonal"))
}
\arguments{
\item{ci}{Object of type climdexInput.}
}
\value{
A vector containing the value of the index for each year.
}
\description{
This function computes the climdex index R20mm.
}
\details{
This function takes a climdexInput object as input and computes the climdex
index R20mm: the annual count of days where daily precipitation is more than 20mm per day.
}
