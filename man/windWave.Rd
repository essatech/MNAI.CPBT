% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/windWave.R
\name{windWave}
\alias{windWave}
\title{Wind Wave Model}
\usage{
windWave(wind_speed = 20, fetch_distance = 5000, water_depth = 100)
}
\arguments{
\item{wind_speed}{Wind speed in units of meters per second.}

\item{fetch_distance}{Fetch distance in units of meters.}

\item{water_depth}{Average offshore water depth in units of meters.}
}
\value{
An estimate of the offshore wave height (meters) and wave period
(seconds).
}
\description{
Estimates the wave height and period for a given wind speed,
fetch distance and water depth.
}
\examples{
waveHeightPeriod <- windWave(wind_speed = 20, fetch_distance = 5000,
water_depth = 50)
print(waveHeightPeriod)
}
