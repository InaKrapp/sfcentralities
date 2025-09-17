test_that("Distance calculation works:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal(result_wo_geom[2, ], 6032.592)
})

test_that("Distance of a point in the network to itself is zero:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal(result_wo_geom[1, ], 0)
})
# Test that the crs is the right one - else, modify.
test_that("If the CRS is not EPSG:4326, it gets transformed to EPSG:4326 for distance calculation:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts <- sf::st_transform(pts, "EPSG:24378")
  pts2 <- pts[1, ]
  # Calculate distance
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal(result_wo_geom[2, ], 6032.592)
})

test_that("If the CRS was modified for distance calculation, the change gets reversed:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts <- sf::st_transform(pts, "EPSG:24378")
  pts2 <- pts[1, ]
  # Calculate distance
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  expect_equal(sf::st_crs(result)$input, "EPSG:24378")
})


# Add test that the system works when no dodgr graph is supplied.
# Problem: This graph is downloaded from the internet - it might therefore change.
# For now, I test that a point has distance 0 to itself, but there should be a more elegant way.
# Also, I ask the user to enter a placename. Can I draw the place from the coordinates instead?
test_that("If no graph is supplied, the street network is drawn from Openstreetmap:", {
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance
  result <- st_network_distance(data = pts, to = pts2, placename = "hampi", transport_mode = "bicycle")
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal(result_wo_geom[1, ], 0)
})

test_that("Distance calculation works if only a single sf object is supplied:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  # Calculate distance:
  result <- st_network_distance(data = pts, graph = graph)
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal(result_wo_geom[2, 1], 6032.592)
})

test_that("Function returns a column named 'distance_to_{rownumber}' if no index is supplied:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal("distance_to_1" %in% colnames(result_wo_geom), TRUE)
})

test_that("Function returns a column named 'distance_to_{indexvalue}' if an index is supplied:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  pts2$index <- "A"
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph, index = "index")
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal("distance_to_A" %in% colnames(result_wo_geom), TRUE)
})

# Test that the inputs have the right classes - else, return an error.
test_that("Function returns an error if no sf object is supplied:", {
  expect_error(st_network_distance(0), "data must be a sf object")
})

# Test that the sf dataframe is modified correctly
test_that("Distance calculation returns a sf object:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  expect_s3_class(result, "sf")
})

test_that("Distance calculation returns a sf object with the right number of rows:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  expect_equal(nrow(result), 2)
})

test_that("Distance calculation returns a sf object with the right number of columns:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph)
  expect_equal(ncol(result), 2)
})

# Test that the object returns the right times if the user specifies times instead of distances:
test_that("Time calculation works:", {
  graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
  pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
  pts <- sf::st_as_sf(pts)
  sf::st_crs(pts) <- "EPSG:4326"
  pts2 <- pts[1, ]
  # Calculate distance:
  result <- st_network_distance(data = pts, to = pts2, graph = graph, calculate_times =  TRUE)
  # Drop geometry to get only the calculated values:
  result_wo_geom <- sf::st_drop_geometry(result)
  expect_equal(result_wo_geom[2, ], 1493.40618)
})

