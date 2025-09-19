#' Find the Most Central Point in a Network
#'
#' This function identifies the point within a sf dataset that has the minimum
#' sum of network distances to all other points in the same dataset. It uses
#' the provided `st_network_distance` function to perform the underlying
#' distance calculations.
#'
#' @param data A sf dataframe containing the points to be analyzed.
#' @param ... Additional arguments to be passed directly to the `st_network_distance`
#'   function. This includes `graph`, `placename`, `transport_mode`, and
#'   `calculate_times`.
#'
#' @return A single-row sf dataframe representing the most central point.
#'   A new column, `total_distance`, is added to this dataframe, containing the
#'   sum of its distances to all other points.
#'
#' @export
#'
#' @examples
#' # First, ensure the st_network_distance function from the prompt is available
#' # in your environment.
#'
#' # Create a sample dodgr graph (as in the original example)
#' # Note: The hampi dataset is often used as an example but may not be installed.
#' # We will create a more self-contained example.
#' library(sf)
#' library(dodgr)
#'
#' # Create an example graph from a small section of a city
#' # Using a bounding box for Cambridge, MA
#' net <- dodgr_streetnet(bbox = c(-71.12, 42.37, -71.10, 42.38), expand = 0.05)
#' graph <- weight_streetnet(net, wt_profile = "foot")
#'
#' # Create a set of random points within that bounding box
#' # set.seed for reproducibility
#' set.seed(123)
#' pts <- st_sample(st_as_sfc(st_bbox(c(xmin=-71.12, ymin=42.37, xmax=-71.10, ymax=42.38))), 10)
#' pts <- st_as_sf(pts)
#' st_crs(pts) <- "EPSG:4326" # Set CRS to WGS84
#' pts$id <- 1:nrow(pts) # Add an ID column
#'
#' # Find the most central point
#' central_point <- st_network_median(data = pts, graph = graph)
#'
#' # Print the result
#' print(central_point)
#'
#' # You can visualize the result
#' # (Requires ggplot2 and ggrepel)
#' # library(ggplot2)
#' # library(ggrepel)
#' #
#' # ggplot() +
#' #   geom_sf(data = dodgr_to_sf(graph), color = "gray80") +
#' #   geom_sf(data = pts, color = "blue", size = 3) +
#' #   geom_sf(data = central_point, color = "red", size = 5, shape = 18) +
#' #   geom_text_repel(data = central_point, aes(label = "Central Point",
#' #                                    geometry = geometry),
#' #                   stat = "sf_coordinates", nudge_x = 0.001) +
#' #   theme_void() +
#' #   labs(title = "Most Central Point (Red) Among a Set of Points (Blue)")

st_network_median <- function(data, return_all = TRUE, ...) {
  # --- 1. Input Validation ---
  if (!inherits(data, "sf")) {
    stop("Input 'data' must be a sf object.")
  }
  if (!"POINT" %in% unique(as.character(sf::st_geometry_type(data)))) {
    stop("Input 'data' must contain POINT geometries.")
  }
  if (nrow(data) <= 1) {
    stop("Input 'data' must contain at least two points to find a central point.")
  }

  # --- 2. Calculate the all-to-all distance matrix ---
  # We call st_network_distance with only the 'data' argument.
  # The function is designed to calculate the all-to-all matrix
  # when 'to_data' is NULL. We pass other arguments like 'graph' via '...'.
  message("Calculating all-to-all network distances...")
  distance_df <- st_network_distance(data = data, ...)


  # --- 3. Isolate the distance columns and calculate sums ---
  # The new columns are expected to start with "distance_to_"
  dist_cols <- grep("^distance_to_", colnames(distance_df), value = TRUE)

  if (length(dist_cols) == 0) {
    stop("Could not find any distance columns in the result. Check st_network_distance output.")
  }

  # Use st_drop_geometry to convert to a regular data.frame for rowSums
  distance_matrix <- sf::st_drop_geometry(distance_df[, dist_cols])

  # Calculate the sum of distances for each point (row).
  # na.rm = TRUE handles cases where a point might be disconnected from the network.
  total_distances <- rowSums(distance_matrix, na.rm = TRUE)


  # --- 4. Find the point with the minimum sum ---
  min_index <- which.min(total_distances)

  # Handle cases where multiple points might have the same minimum sum
  if (length(min_index) > 1) {
    warning("Multiple points have the same minimum total distance. Returning the first one found.")
    min_index <- min_index[1]
  }


  # --- 5. Prepare and return the result ---
  # Get the original sf object for the central point
  central_point <- data[min_index, ]

  # Add the calculated total distance as a new column
  central_point$total_distance <- total_distances[min_index]

  message(paste0("Central point found at index: ", min_index))
  return(central_point)
}
