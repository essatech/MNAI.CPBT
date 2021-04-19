testthat::test_that("Gen cross shore profiles", {
  data(Coastline)
  shoreline_points <- samplePoints(
     Coastline = Coastline,
     ShorelinePointDist = 150, # where to sample points on shoreline
     BufferDist = 300.0, # Buffer distance in meters for vertical line
     RadLineDist = 3.0 # Line dist in km
  )
  points <- shoreline_points[[1]]
  lines <- shoreline_points[[2]]
  expect_equal(nrow(points), 4)
  expect_equal(nrow(lines), 4)
  expect_type(lines, 'list')
  expect_type(points, 'list')
  ml <- round(as.numeric(sf::st_length(lines[1,])), 2)
  expect_equal(ml, 5998.6)
})


testthat::test_that("sample profile generation under alternative scenarios", {

  data(Coastline)
  shoreline_points <- samplePoints(
    Coastline = Coastline,
    ShorelinePointDist = 150, # where to sample points on shoreline
    BufferDist = 50, # Buffer distance in meters for vertical line
    RadLineDist = 1.5 # Line dist in km
  )
  lines <- shoreline_points[[2]]
  expect_type(lines, 'list')

  rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT",
                        mustWork = TRUE)
  TopoBathy <- raster::raster(rpath)
  pt_elevs <- ExtractElev(lines, TopoBathy)
  has_values <- !(all(is.na(pt_elevs$elev)))
  max_elev <- round(max(pt_elevs$elev, na.rm=TRUE),0)
  min_elev <- round(min(pt_elevs$elev, na.rm=TRUE),0)
  #   Getting values

  expect_true(has_values)
  expect_equal(max_elev, 5)
  expect_equal(min_elev, -5)

  # Test SignalSmooth
  smooth0 <- SignalSmooth(point_elev = pt_elevs, SmoothParameter = 0)
  smooth1 <- SignalSmooth(point_elev = pt_elevs, SmoothParameter = 1)
  smooth5 <- SignalSmooth(point_elev = pt_elevs, SmoothParameter = 5)
  smooth10 <- SignalSmooth(point_elev = pt_elevs, SmoothParameter = 10)
  smooth100 <- SignalSmooth(point_elev = pt_elevs, SmoothParameter = 100)

  expect_type(smooth0$elev_smooth, 'double')
  expect_type(smooth1$elev_smooth, 'double')
  expect_type(smooth5$elev_smooth, 'double')
  expect_type(smooth10$elev_smooth, 'double')
  expect_type(smooth100$elev_smooth, 'double')

  m1 <- max(smooth100$elev_smooth, na.rm=TRUE)
  m2 <- max(smooth0$elev_smooth, na.rm=TRUE)
  expect_true(m2 > m1)

  m1 <- min(smooth100$elev_smooth, na.rm=TRUE)
  m2 <- min(smooth0$elev_smooth, na.rm=TRUE)
  expect_true(m2 < m1)


  # Test transect cleaner
  ct1 <- CleanTransect(point_elev = smooth0, RadLineDist = 1.5,
                       MaxOnshoreDist = 1, trimline = NA)
  ct2 <- CleanTransect(point_elev = smooth10, RadLineDist = 1.5,
                       MaxOnshoreDist = 50, trimline = NA)
  ct3 <- CleanTransect(point_elev = smooth100, RadLineDist = 1.5,
                       MaxOnshoreDist = 0.5, trimline = NA)

  expect_type(ct1$elev_smooth, 'double')
  expect_type(ct2$elev_smooth, 'double')
  expect_type(ct3$elev_smooth, 'double')

  expect_type(ct1$Xpos, 'integer')
  expect_type(ct2$Xpos, 'integer')
  expect_type(ct3$Xpos, 'integer')

  expect_true(min(ct1$elev_smooth, na.rm = TRUE) < 0)
  expect_true(min(ct2$elev_smooth, na.rm = TRUE) < 0)
  expect_true(min(ct3$elev_smooth, na.rm = TRUE) < 0)

  expect_true(max(ct1$elev_smooth, na.rm = TRUE) > 0)
  expect_true(max(ct2$elev_smooth, na.rm = TRUE) > 0)
  expect_true(max(ct3$elev_smooth, na.rm = TRUE) > 0)

  # Test trim line
  data(Trimline)
  Tl1 <- CleanTransect(point_elev = smooth100, RadLineDist = 1.5,
                       MaxOnshoreDist = 50, trimline = Trimline)
  Tl2 <- CleanTransect(point_elev = smooth100, RadLineDist = 1.5,
                       MaxOnshoreDist = 0, trimline = Trimline)
  Tl3 <- CleanTransect(point_elev = smooth100, RadLineDist = 1.5,
                       MaxOnshoreDist = 999999, trimline = Trimline)
  expect_true(nrow(Tl1) >  nrow(Tl2))
  expect_true(nrow(Tl1) ==  nrow(Tl3))

})

