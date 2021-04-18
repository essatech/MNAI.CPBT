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


testthat::test_that("elevation point sample on generated profiles", {
  data(Coastline)
  shoreline_points <- samplePoints(
    Coastline = Coastline,
    ShorelinePointDist = 150, # where to sample points on shoreline
    BufferDist = 50, # Buffer distance in meters for vertical line
    RadLineDist = 1.5 # Line dist in km
  )
  lines <- shoreline_points[[2]]

  data(TopoBathy)
  pt_elevs <- ExtractElev(lines, 5, TopoBathy)
  has_values <- !(all(is.na(pt_elevs$elev)))
  max_elev <- round(max(pt_elevs$elev, na.rm=TRUE),0)
  min_elev <- round(min(pt_elevs$elev, na.rm=TRUE),0)
  # Getting values
  expect_true(has_values)
  expect_equal(max_elev, 5)
  expect_equal(min_elev, -5)

})

