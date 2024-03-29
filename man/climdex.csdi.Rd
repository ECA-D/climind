% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indices.R
\name{climdex.csdi}
\alias{climdex.csdi}
\title{Cold Spell Duration Index}
\usage{
climdex.csdi(ci, freq = c("monthly", "annual", "halfyear", "seasonal"),
  spells.can.span.years = FALSE)
}
\arguments{
\item{ci}{Object of type climdexInput.}

\item{spells.can.span.years}{Whether to allow spells of dry/wet days to span
years.}
}
\value{
A vector containing the value of the index for each year.
}
\description{
This function computes the climdex index CSDI.
}
\details{
This function takes a climdexInput object as input and computes the climdex
index CSDI (Cold Spell Duration Index).

The cold spell duration index is defined as the number of days
each year which are part of a "cold spell". A "cold spell" is defined as a
sequence of 6 or more days in which the daily minimum temperature is below
the 10th percentile of daily minimum temperature for a 5-day running window
surrounding this day during the baseline period.

The \code{spells.can.span.years} option specifies whether spells can cross
year boundaries -- i.e., span years. The default for this is the same as
fclimdex.
}
\note{
These functions may calculate slightly different results than
fclimdex.

Behaviour of climdex.wsdi and climdex.csdi differ somewhat from fclimdex.
fclimdex considers all days in a warm or cold spell to be part of the year
in which the spell ended.  climdex.wsdi and climdex.csdi split the spell
such that days in each spell are allocated to the separate years in the days
occurred.
}
\references{
\url{http://etccdi.pacificclimate.org/list_27_indices.shtml}
}
\seealso{
\code{\link{climdexInput.raw}}, \code{\link{climdexInput.csv}},
\code{\link{threshold.exceedance.duration.index}}.
}
\keyword{climate}
\keyword{ts}
