% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ErosionTransectsUtilMangrove.R
\name{ErosionTransectsUtilMangrove}
\alias{ErosionTransectsUtilMangrove}
\title{Erosion Transects Utility Mangrove}
\usage{
ErosionTransectsUtilMangrove(
  Ho = 2.5,
  To = 7,
  total_wsl_adj = NA,
  linkbeach = NA,
  wave_data = NA,
  storm_duration = 3,
  Longshore = 100,
  PropValue = 200,
  Tr = 10,
  disc = 0.05,
  TimeHoriz = 50,
  mean_sea_level = 0.01,
  mean_high_water = 0.7
)
}
\arguments{
\item{Ho}{Initial offshore wave height in meters.}

\item{To}{Initial offshore wave period in seconds.}

\item{total_wsl_adj}{Total water surface level above the chart datum. Recall
that the chart datum and TopoBathy DEM are referenced to have 0 at low
water. It is recommended that the erosion model is run with mean sea level.}

\item{linkbeach}{Dataframe returned from LinkProfilesToBerms. Should contain.
columns for forshore slope `fore_slp`, Erosion Constant `me` in kg/Ns and
density `Cm` in km/m3.}

\item{wave_data}{sf and dataframe spatial points returned from WaveModel.}

\item{storm_duration}{Numeric. Storm duration in hours.}

\item{Longshore}{Longshore distance in meters should match
ShorelinePointDist used in samplePoints.}

\item{PropValue}{Generally land value in dollars per square meter of beach.
(not used if beach polygon property values are provided).}

\item{Tr}{Numeric. Return period (frequency) of the simulated storm (in years).}

\item{disc}{Annual valuation discount rate over the time horizon (0 - 1).}

\item{TimeHoriz}{Time horizon (in years) for long term cumulative
valuation given a storm return frequency.
Typically 100-year horizons are used.}

\item{mean_sea_level}{Mean sea level elevation in meters above chart datum.
Note that the chart datum should be adjusted to LLW for these estimates}

\item{mean_high_water}{Mean high water level elevation in meters above
chart datum.}
}
\value{
A data frame with erosion estimates for each cross-shore profile.
_NoVeg is estimated without submerged vegetation and _Veg is an estimate with
submerged vegetation
}
\description{
Mudflat erosion model originally developed by Dr. Greg Guannel
for the Coastal Natural Capital InVEST project. This function estimates
beach retreat and wave runup for mangrove mudflats.
}
\details{
The coastal erosion model was originally developed by Dr. Greg
Guannel for the Coastal Natural Capital InVEST project. This function
estimates lateral beach erosion and wave runup for each cross-shore profile.
Foreshore parameters for berm width, height etc. are provided as spatial
polygons for each beach section. These are then linked to underlying
cross-shore profiles for erosion estimates. The return object is a data frame
showing erosion estimates for each cross-shore profile.
}
\references{
InVEST: Wave Attenuation & Erosion Reduction: Coastal Protection (G. Guannel)

Guannel et al. (2014) Integrated modeling framework to quantify the coastal
protection services supplied by vegetation. Journal of
Geophysical Research: Oceans. DOI: 10.1002/2014JC009821.
}
