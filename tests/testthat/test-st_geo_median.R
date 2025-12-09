# --- Setup: Create common data objects for tests ---

# Sample sf points for testing
pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
pts <- sf::st_as_sf(pts)
sf::st_crs(pts) <- "EPSG:3857"

# --- Test Suite ---

context("Testing st_geo_median function")

test_that("Code fails if input is no sf object", {
  expect_error(st_geo_median(0))
})

test_that("Code fails if input has no crs ", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2.2)))
  pts <- sf::st_as_sf(pts)
  expect_error(st_geo_median(pts))
})

test_that("Code fails if input has a geographical crs ", {
  sf::st_crs(pts) <- "EPSG:4326"
  expect_error(st_geo_median(pts))
})

test_that("Code returns a sf object", {
  test <- st_geo_median(pts)
  expect_s3_class(test, "sf")
})

test_that("Code gives the result the correct crs", {
  test <- st_geo_median(pts)
  expect_equal(sf::st_crs(test), sf::st_crs(pts))
})

test_that("Code gives expected result for valid input: point", {
  test <- st_geo_median(pts)
  testresult <- sf::st_sfc(sf::st_point(c(1, 1)))
  sf::st_crs(testresult) <- "EPSG:3857"
  expect_equal(test$geometry, testresult)
})

test_that("Code gives expected result for valid input: distance", {
  test <- st_geo_median(pts)
  expect_equal(test$distance, 2.97626353)
})

test_that("Code gives expected result for valid input: niter", {
  test <- st_geo_median(pts)
  expect_equal(test$niter, 14)
})

test_that("Code gives expected result for valid input: reltol", {
  test <- st_geo_median(pts)
  reltol <- test$reltol
  expect_equal(round(reltol, 8), round(2.286785e-08, 8))
})

test_that("Code gives expected result for valid input: number of points", {
  test <- st_geo_median(pts)
  expect_equal(test$number_of_points, 3)
})

# Add test that code works with a grouping argument
test_that("Code returns a sf object", {
  pts$index <- 1:3
  test <- st_geo_median(pts, "index")
  expect_s3_class(test, "sf")
})

# Add test that code skips empty partdataframes

# Add test that code returns a single point if a partdataframe only has a single point
# or if the sf data only has a single point.
test_that("Code returns a sf object if only two points are given", {
  test <- st_geo_median(pts)
  expect_s3_class(test, "sf")
})

test_that("Code returns a sf object if only one point is given", {
  test <- st_geo_median(pts)
  expect_s3_class(test, "sf")
})

# Tests for geo_median_inner:

test_that("geo_median_inner calculates the geometric median for standard 2D data", {
  # -------------------------------------------------------------------------
  # Case 1: Standard non-collinear points (Triangle)
  # -------------------------------------------------------------------------

  # Define 3 points forming a triangle where the median is somewhat distinct
  # Points: (0,0), (4,0), (0,3)
  P <- matrix(c(0, 4, 0, 0, 0, 3), ncol = 2)

  # Run function
  res <- geo_median_inner(P)

  # Check 1: Return structure types
  expect_type(res, "list")
  expect_named(res, c("p", "d", "reltol", "niter"))
  expect_type(res$p, "double")
  expect_length(res$p, 2)

  # Check 2: Convergence
  # For non-collinear points, it should iterate more than once
  expect_gt(res$niter, 1)
  # The relative tolerance achieved should be less than the default tolerance (1e-07)
  expect_lt(res$reltol, 1e-07)

  # Check 3: Sanity check on result coordinates
  # The geometric median must be inside the bounding box of the points
  expect_true(all(res$p >= 0))
  expect_true(res$p[1] <= 4)
  expect_true(res$p[2] <= 3)
})

test_that("geo_median_inner detects colinearity and returns the median", {
  # -------------------------------------------------------------------------
  # Case 2: Collinear points (The specific logic modification)
  # -------------------------------------------------------------------------

  # Define 3 points lying exactly on the line y = x
  # Points: (1,1), (2,2), (3,3)
  # The standard Geometric Median of 1D data is the Median,
  P_collinear <- matrix(c(1, 2, 3, 1, 2, 3), ncol = 2)

  res <- geo_median_inner(P_collinear)

  # Check 1: Result is the Median
  # Mean of (1,2,3) is 2.
  expected_mean <- c(2, 2)
  expect_equal(res$p, expected_mean, tolerance = 1e-8)

  # Check 2: Iterations skipped
  # Because QR rank is <= 1, the code sets p1 <- p0.
  # The while loop condition (max(abs(p0-p1)) > tol) fails immediately.
  # The iter counter starts at 1 and isn't incremented in the loop.
  expect_equal(res$niter, 1)

  # -------------------------------------------------------------------------
  # Case 3: Vertical line (Edge case for QR)
  # -------------------------------------------------------------------------
  # Points: (0,0), (0,10), (0,20)
  P_vert <- matrix(c(0, 0, 0, 0, 10, 20), ncol = 2)

  res_vert <- geo_median_inner(P_vert)

  # Should return arithmetic mean: (0, 10)
  expect_equal(res_vert$p, c(0, 10), tolerance = 1e-8)
  expect_equal(res_vert$niter, 1)
})

test_that("geo_median_inner handles trivial cases (1 or 2 points)", {
  # -------------------------------------------------------------------------
  # Case 4: Single Point
  # -------------------------------------------------------------------------
  P_single <- matrix(c(5, 5), ncol = 2)
  res_single <- geo_median_inner(P_single)

  # Rank is 0 (centered matrix is 0). Should return the point itself.
  expect_equal(res_single$p, c(5, 5))
  expect_equal(res_single$d, 0) # Distance to self is 0
  expect_equal(res_single$niter, 1)

  # -------------------------------------------------------------------------
  # Case 5: Two Points
  # -------------------------------------------------------------------------
  # Two points define a line (Rank 1). Should return the midpoint (mean).
  P_two <- matrix(c(0, 10, 0, 0), ncol = 2) # (0,0) and (10,0)
  res_two <- geo_median_inner(P_two)

  expect_equal(res_two$p, c(5, 0)) # Midpoint
  expect_equal(res_two$niter, 1)   # Should skip loop due to Rank 1 check
})

test_that("geo_median_inner respects control parameters", {
  # -------------------------------------------------------------------------
  # Case 6: Max Iterations Warning
  # -------------------------------------------------------------------------
  # Create a configuration that normally requires iterations
  P <- matrix(c(0, 10, 5, 0, 0, 10), ncol = 2)

  # Set maxiter to 1 to force the warning
  expect_warning(
    geo_median_inner(P, maxiter = 1),
    "Maximum number of iterations reached"
  )

  # -------------------------------------------------------------------------
  # Case 7: Custom Tolerance
  # -------------------------------------------------------------------------
  # If we set a very loose tolerance, it should finish faster (fewer iterations)
  # than with strict tolerance
  res_loose <- geo_median_inner(P, tol = 0.1)
  res_strict <- geo_median_inner(P, tol = 1e-9)

  expect_lt(res_loose$niter, res_strict$niter)
})
