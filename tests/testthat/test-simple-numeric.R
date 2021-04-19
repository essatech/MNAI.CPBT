# Tests the windWave function
testthat::test_that("submergedStructure() returns correct values and format", {
  wwdat <- windWave(wind_speed = 20, fetch_distance = 5000, water_depth = 50)
  expect_type(wwdat, "double")
  expect_equal(names(wwdat), c("Height", "Period"))
})

# Tests the submergedStructure function
testthat::test_that("submergedStructure() returns correct values", {
  ssdat1 <- submergedStructure(Hi = 2.5, To = 7, hi = 2, hc = 3, Cwidth = 2,
                               Bwidth = 5)
  ssdat2 <- submergedStructure(Hi = 2.5, To = 7, hi = 10, hc = 3, Cwidth = 2,
                               Bwidth = 5)
  ssdat3 <- submergedStructure(Hi = 2.5, To = 7, hi = 6, hc = 3, Cwidth = 2,
                               Bwidth = 5)
  expect_equal(ssdat1, 0)
  expect_equal(ssdat2, 1)
  expect_equal(ssdat3, 0.8)
})

# Math Wrappers
testthat::test_that("Math wrappers for log and trig", {
  expect_equal(round(math.sqrt(25), 3), 5)
  expect_equal(round(math.tanh(3), 3), 0.995)
  expect_equal(round(math.sinh(3), 3), 10.018)
  expect_equal(round(math.sin(3), 3), 0.141)
  expect_equal(round(math.cosh(3), 3), 10.068)
  expect_equal(math.floor(8.89), 8)
  expect_equal(len(c(1, 2, 3)), 3)
  expect_equal(log.info("test"), "test")
  expect_equal(log.debug("test"), "test")
  expect_equal(log.warning("test"), "test")
})


# iterativek type
testthat::test_that("iterativek", {
  wn <- iterativek(2000, 50)
  expect_type(wn, "double")
  wn2 <- round(wn, 1)
  expect_equal(wn2, 407747.2)
  wn <- iterativek(0.03, 50)
  wn2 <- round(wn, 4)
  expect_equal(wn2, 0.0014)
})

