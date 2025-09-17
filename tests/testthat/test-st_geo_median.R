test_that("Code fails if input is no sf object", {
  expect_error(st_geo_median(0))
})

test_that("Code fails if input has no crs ", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  expect_error(st_geo_median(pts))
})

test_that("Code fails if input has a geographical crs ", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  expect_error(st_geo_median(pts))
})

test_that("Code returns a sf object", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_s3_class(test, "sf")
})

test_that("Code gives the result the correct crs", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_equal(sf::st_crs(test), sf::st_crs(pts))
})

test_that("Code gives expected result for valid input: point", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  testresult <- sf::st_sfc(sf::st_point(c(1, 1)))
  sf::st_crs(testresult) <- "EPSG:3857"
  expect_equal(test$geometry, testresult)
})

test_that("Code gives expected result for valid input: distance", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_equal(test$distance, 2.97626353)
})

test_that("Code gives expected result for valid input: niter", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_equal(test$niter, 14)
})

test_that("Code gives expected result for valid input: reltol", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  reltol <- test$reltol
  expect_equal(round(reltol, 8), round(2.286785e-08, 8))
})

test_that("Code gives expected result for valid input: number of points", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_equal(test$number_of_points, 3)
})

# Add test that code works with a grouping argument
test_that("Code returns a sf object", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  pts$index <- 1:3
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts, "index")
  expect_s3_class(test, "sf")
})

# Add test that code skips empty partdataframes

# Add test that code returns a single point if a partdataframe only has a single point
# or if the sf data only has a single point.
test_that("Code returns a sf object if only two points are given", {
  pts <- sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_s3_class(test, "sf")
})

test_that("Code returns a sf object if only one point is given", {
  pts <- sf::st_sfc(sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:3857"
  test <- st_geo_median(pts)
  expect_s3_class(test, "sf")
})

# Add tests for st_geo_median_inner: That result is correct value,
# that result has correct structure.
