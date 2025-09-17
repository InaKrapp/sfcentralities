# Load necessary libraries for testing
library(testthat)
library(sf)
library(dodgr)

# --- Sourcing the functions to be tested ---
# In a real package, these would be loaded by testthat::test_package().
# For a standalone script, you must source them manually.
# Ensure the file paths are correct or the functions are in the global environment.

# Source the original st_network_distance function (from the first prompt)
# source("R/st_network_distance.R")
# Source the new st_network_median function
# source("R/st_network_median.R")

# --- Test Suite ---

# --- 1. Setup a predictable test environment ---
# We create a simple, linear graph where the central point is obvious.
# Points are at x = 0, 1, 2, 3. The two middle points (2 and 3) are the most central.
# The function should return the first one it finds (point at x=1, index 2).
#
# Distances from point 1 (x=0): to 1(0), 2(1), 3(2), 4(3). Sum = 6
# Distances from point 2 (x=1): to 1(1), 2(0), 3(1), 4(2). Sum = 4  <- Minimum
# Distances from point 3 (x=2): to 1(2), 2(1), 3(0), 4(1). Sum = 4  <- Minimum
# Distances from point 4 (x=3): to 1(3), 2(2), 3(1), 4(0). Sum = 6
#
# Expected central point: index 2, with total_distance = 4.

# --- 1. Setup a predictable test environment ---
# Load necessary libraries for testing
library(testthat)
library(sf)
library(dodgr)

# --- Sourcing the functions to be tested ---
# (Ensure st_network_distance and find_central_point are loaded)

# --- Test Suite ---
# --- 1. Setup a predictable test environment ---

# Create vertices data frame with ID, longitude (x), and latitude (y)
v_linear <- data.frame(
  id = 1:3,
  x = c(0, 1, 2),
  y = rep(0, 3)
)

# Create edges data frame referencing vertex IDs
edges_linear <- data.frame(
  from_id = c(1, 2),
  to_id = c(2, 3)
)

# --- CORRECT METHOD: Construct an sf object with LINESTRINGs ---

# Create a list to hold the LINESTRING geometries
linestrings <- list()
# For each edge, look up the coordinates of its start and end points
# and create a LINESTRING.
for (i in 1:nrow(edges_linear)) {
  from_node <- edges_linear$from_id[i]
  to_node <- edges_linear$to_id[i]

  # Get coordinates for the start and end of the line
  from_coords <- v_linear[v_linear$id == from_node, c("x", "y")]
  to_coords <- v_linear[v_linear$id == to_node, c("x", "y")]

  # Create a matrix of the two points
  line_matrix <- as.matrix(rbind(from_coords, to_coords))

  # Create the LINESTRING and add it to our list
  linestrings[[i]] <- sf::st_linestring(line_matrix)
}

# Convert the list of linestrings into a simple features geometry column (sfc)
sfc_network <- sf::st_sfc(linestrings, crs = 4326)

# Create the final sf object. This is the object weight_streetnet expects.
sf_network <- sf::st_sf(geometry = sfc_network)

# column "highway" and give it a generic value that is walkable.
sf_network$highway <- "path"

# Now, call weight_streetnet with the correct sf object.
# It will correctly process the LINESTRINGs to build a routable graph.
graph_linear <- dodgr::weight_streetnet(sf_network, wt_profile = "foot")

# Create the corresponding sf points for testing the function (this part remains the same)
pts_linear <- sf::st_as_sf(v_linear, coords = c("x", "y"), crs = 4326)
# Add character ID to match what dodgr produces from vertices.
pts_linear$id <- as.character(pts_linear$id)

# --- 2. Begin Tests ---

test_that("Function correctly identifies the central point in a simple case", {
  result <- st_network_median(data = pts_linear, graph = graph_linear)

  # Check that the central point (ID = 2) was correctly identified
  expect_equal(sf::st_drop_geometry(result)$id, '2')

  # Check that the calculated total distance is correct
  expect_equal(result$total_distance, 222638.982)
})

test_that("Function returns a single-row sf object with correct structure", {
  result <- st_network_median(data = pts_linear, graph = graph_linear)

  # Check class and dimensions
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)

  # Check for the presence and type of the new column
  expect_true("total_distance" %in% names(result))
  expect_true(is.numeric(result$total_distance))

  # Check that original columns are preserved
  expect_true("id" %in% names(result))
})

test_that("Function handles disconnected points gracefully", {
  # Add a 4th point that is not connected to the network
  v_disconnected <- rbind(v_linear, data.frame(id = 4, x = 10, y = 10))
  pts_disconnected <- sf::st_as_sf(v_disconnected, coords = c("x", "y"), crs = 4326)

  # The distances to/from point 4 will be Inf. `rowSums` with `na.rm=T` handles this.
  # The function should ignore point 4 and find the central point of the main component (point 2).
  result <- st_network_median(data = pts_disconnected, graph = graph_linear)

  # The central point should still be the same one from the connected component.
  expect_equal(sf::st_drop_geometry(result)$id, 2)
})

test_that("Function throws errors for invalid inputs", {
  # Test with non-sf object
  expect_error(
    st_network_median(data.frame(x = 1, y = 1)),
    "Input 'data' must be a sf object."
  )

  # Test with less than two points
  expect_error(
    st_network_median(pts_linear[1, ]),
    "Input 'data' must contain at least two points to find a central point."
  )

  # Test with non-POINT geometry
  line <- sf::st_sfc(sf::st_linestring(matrix(1:4, ncol = 2)))
  line_sf <- sf::st_as_sf(data.frame(id=1), geom=line, crs = 4326)
  expect_error(
    st_network_median(line_sf, graph = graph_linear),
    "Input 'data' must contain POINT geometries."
  )
})


test_that("Function finds central points along a real-world graph", {

net <- dodgr_streetnet(bbox = c(-71.12, 42.37, -71.10, 42.38), expand = 0.05)
graph <- weight_streetnet(net, wt_profile = "foot")
#'
# Create a set of random points within that bounding box
# set.seed for reproducibility
set.seed(123)
pts <- st_sample(st_as_sfc(st_bbox(c(xmin=-71.12, ymin=42.37, xmax=-71.10, ymax=42.38))), 10)
pts <- st_as_sf(pts)
st_crs(pts) <- "EPSG:4326" # Set CRS to WGS84
pts$id <- 1:nrow(pts) # Add an ID column

# Find the most central point
central_point <- st_network_median(data = pts, graph = graph)
expect_equal(sf::st_drop_geometry(result)$id, 2)
})
