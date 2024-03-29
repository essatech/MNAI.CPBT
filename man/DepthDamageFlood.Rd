% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DepthDamageFlood.R
\name{DepthDamageFlood}
\alias{DepthDamageFlood}
\title{Depth Damage Flood Summaries}
\usage{
DepthDamageFlood(Bldgs = NA, flood_contours = NA, HAZUS = NA)
}
\arguments{
\item{Bldgs}{sf and dataframe polygon object of building structure foot
prints. This dataset must have HAZUS depth-damge curves IDs populated for
each structure (DDID) as well as estimates of the replacement value cost
for each structure in dollars (VAL). See data(Bldgs) for an example input.}

\item{flood_contours}{Flood contours object returned from FloodContours.}

\item{HAZUS}{HAZUS depth-damage reference curves. Note that these are
included in the package see data(HAZUS). Users may modify these curves,
but it is expected that most users will use default values.}
}
\value{
Flood damage cost summaries for the storm event with the number of
structures flooded, the mean flood depth and the total damage cost. Values
are provided for scenarios with submerged vegetation and without submerged
vegetation.
#' \describe{
  \item{nStructure}{Number of structures flooded.}
  \item{MedianDepth}{Median depth of flooded structure.}
  \item{MaxDepth}{Maximum flood depth across all structures.}
  \item{Damage}{Total flood structural damage cost in dollars.}
}
}
\description{
Summarizes flood damage values from a storm event using depth
damage curves for each structure with a given flood water depth.
}
\examples{
\dontrun{
library(MNAI.CPBT)
data(Coastline)
# Generate cross-shore profile lines along the coastline.
ShorelinePointDist = 150
crossshore_profiles <- samplePoints(
  Coastline = Coastline,
  ShorelinePointDist = ShorelinePointDist,
  BufferDist = 50, RadLineDist = 1.5)
crossshore_lines <- crossshore_profiles[[2]]


# Extract elevation values along each profile
rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
TopoBathy <- raster::raster(rpath)
pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)


# Run SignalSmooth function to smooth elevation profiles
pt_elevs <- SignalSmooth(point_elev = pt_elevs,
SmoothParameter = 5)


# Clean the cross-shore profiles with CleanTransect
cleantransect <- CleanTransect(
  point_elev = pt_elevs,
  RadLineDist = 1.5, MaxOnshoreDist = 0.01, trimline = NA
)

# Merge vegetation onto lines
data(Vegetation)
dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)

# Run the wave evolution model
wave_data <- WaveModel(dat = dat_veg,
  total_wsl_adj = 1.2,
  Ho = 1.5, To = 5)

# Link data to foreshore beach attributes
linkbeach <- LinkProfilesToBeaches(BeachAttributes = BeachAttributes,
dat = wave_data)

# Run the erosion model
erosion <- ErosionTransectsUtil(
    Ho = 2, To = 8, total_wsl_adj = 1.2,
    linkbeach = linkbeach, wave_data = wave_data,
    storm_duration = 3, Tr = 10,
    Longshore = ShorelinePointDist, PropValue = 200,
    disc = 0.05, TimeHoriz = 50)

# Get the erosion damage totals across the study area
erosion_totals <- ErosionTotals(wave_data = wave_data,
  erosion = erosion, Longshore = ShorelinePointDist)

# Build flood contours (and rasters of water surface elevation)
# over the study area.
 flood_contours <- FloodContours(TopoBathy = TopoBathy, mean_high_water = 1,
  total_wsl_adj = 1.2, erosion_totals = erosion_totals)

# Calculate the total flood damage cost using depth damage curves

 data(Bldgs)
 data(HAZUS)
 dd_flood <- DepthDamageFlood(Bldgs = Bldgs,
   flood_contours = flood_contours,
   HAZUS = HAZUS)

 # Total damage cost of flooding (dollars)
 dd_flood$VegDamage


}
}
