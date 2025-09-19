# --- Setup: Create common data objects for tests ---

# A standard, well-connected graph
graph_hampi <- dodgr::weight_streetnet(dodgr::hampi, wt_profile = "bicycle")

# Sample sf points for testing
pts <- sf::st_sfc(
  sf::st_point(c(76.47398, 15.330)), # Central point
  sf::st_point(c(76.4599, 15.345)) # Peripheral point
)
pts <- sf::st_as_sf(pts, crs = "EPSG:4326")
pts$name <- c("central", "peripheral")

# --- Test Suite ---

context("Testing st_closeness_centrality function")

test_that("handles dodgr_streetnet input correctly", {
  res <- st_closeness_centrality(graph_hampi)

  # Check output class and structure
  expect_s3_class(res, "sf")
  expect_true("closeness" %in% names(res))

  # Check output dimensions
  # Note: Filtering might remove some vertices if graph is disconnected.
  # For hampi, the largest component should be the whole graph.
  graph_verts <- dodgr::dodgr_vertices(graph_hampi)
  graph_verts <- graph_verts[graph_verts$component == 1, ]
  expect_equal(nrow(res), nrow(graph_verts))

  # Check value types and ranges
  expect_type(res$closeness, "double")
  expect_true(all(res$closeness > 0))
})

test_that("handles sf points with a pre-built graph correctly", {
  res <- st_closeness_centrality(data = pts, graph = graph_hampi)

  # Check output class and structure
  expect_s3_class(res, "sf")
  expect_true("closeness" %in% names(res))
  expect_equal(nrow(res), nrow(pts))

  # Check calculation logic (central point should have higher closeness)
  closeness_central <- res$closeness[res$name == "central"]
  closeness_peripheral <- res$closeness[res$name == "peripheral"]
  expect_gt(closeness_central, closeness_peripheral)
})

test_that("errors on invalid or missing inputs", {
  # Error on wrong data type
  expect_error(
    st_closeness_centrality(data = data.frame(x = 1)),
    regexp = "Input 'data' must be an 'sf' dataframe of points or a 'dodgr_streetnet' object. If 'data' is not supplied, a graph or a placename and a transport_mode have to be supplied."
  )

  # Error when placename/transport_mode are needed but not provided
  expect_error(
    st_closeness_centrality(data = pts, graph = NULL, placename = "Hampi"),
    regexp = "both 'placename' and 'transport_mode' must be provided"
  )
})

test_that("normalized parameter works correctly", {
  res_norm <- st_closeness_centrality(pts, graph = graph_hampi, normalized = TRUE)
  res_unnorm <- st_closeness_centrality(pts, graph = graph_hampi, normalized = FALSE)

  # The values must be different
  expect_false(identical(res_norm$closeness, res_unnorm$closeness))

  # Normalized closeness (1/mean) should be higher than unnormalized (1/sum)
  # for any network with more than one reachable node.
  expect_true(all(res_norm$closeness > res_unnorm$closeness))
})

test_that("CRS is handled and restored correctly", {
  # Create points with a projected CRS (UTM Zone 50N for Hampi area)
  pts_proj <- sf::st_transform(pts, crs = "EPSG:32643")

  res <- st_closeness_centrality(pts_proj, graph = graph_hampi)

  # The output CRS must match the original input CRS
  expect_equal(sf::st_crs(res), sf::st_crs(pts_proj))
})

test_that("automatic graph building works (requires internet)", {
  # This test can be slow and requires an internet connection.
  skip_on_cran()

  res_auto <- st_closeness_centrality(
    data = pts,
    placename = "Hampi, India",
    transport_mode = "bicycle"
  )

  expect_s3_class(res_auto, "sf")
  expect_true("closeness" %in% names(res_auto))
  expect_equal(nrow(res_auto), nrow(pts))
})
