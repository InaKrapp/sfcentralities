#' @title Calculate Closeness Centrality in a Street Network
#'
#' @description This function determines closeness centrality for either specified points
#' within a street network or for all vertices of a given street network.
#'
#' @param data An `sf` dataframe containing points for which closeness centrality
#'   should be calculated, or a `dodgr_streetnet` graph for which centrality
#'   of all its vertices should be determined.
#' @param graph An optional `dodgr_streetnet` graph. If `data` is an `sf` object,
#'   this graph will be used for distance calculations. If `data` is already a
#'   `dodgr_streetnet`, this parameter is ignored. If neither `graph` nor a
#'   `dodgr_streetnet` `data` is provided, a graph will be built using `placename`
#'   and `transport_mode`.
#' @param placename A string giving the name of a place from which a street network
#'   should be obtained (e.g., "Hampi, India"). This is used only if `data` is
#'   an `sf` object and `graph` is not supplied.
#' @param transport_mode A string specifying the mode of transport (e.g., "foot",
#'   "bicycle", "motorcar"). This is used only if `data` is an `sf` object and
#'   `graph` is not supplied.
#' @param batched_if An integer. If the number of vertices in the graph (or points in `data`)
#'   exceeds this number, distance calculations will be performed in batches using
#'   `st_closeness_centrality_largedata`. Set to `100000` by default. This value
#'   should be lowered if memory allocation errors occur.
#' @param normalized A logical value. If `TRUE` (default), closeness is normalized
#'   by the number of reachable nodes, representing the mean of distances. If `FALSE`,
#'   it is the inverse of the sum of distances.
#'
#' @return An `sf` dataframe of the input `data` points (or the graph vertices
#'   if `data` was a `dodgr_streetnet`), with an additional column 'closeness'
#'   and with CRS "EPSG:4326".
#' @export
#'
#' @examples
#' # The simplest example calculates the closeness of points within a network/graph:
#' library(dodgr)
#' library(sf)
#' library(dplyr)
#'
#' # Example 1: Calculate centrality for all vertices of a dodgr graph
#' graph_hampi <- dodgr::weight_streetnet(dodgr::hampi, wt_profile = "bicycle")
#' closeness_all_vertices <- st_closeness_centrality(graph_hampi)
#' head(closeness_all_vertices)
#'
#' # Example 2: Find centrality values for specific points, providing a pre-built graph
#' pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
#' pts <- sf::st_as_sf(pts)
#' sf::st_crs(pts) <- "EPSG:4326"
#' sf::st_geometry(pts) <- "geom" # Rename geometry column for consistency
#' pts$name = c("city center", "train station")
#'
#' pts_centrality_with_graph <- st_closeness_centrality(pts, graph = graph_hampi)
#' print(pts_centrality_with_graph)
#'
#' # Example 3: Find centrality values for specific points, letting the function build the graph
#' \dontrun{ # This example requires internet access to download OSM data
#' pts_centrality_build_graph <- st_closeness_centrality(
#'   pts,
#'   placename = "Hampi, India",
#'   transport_mode = "bicycle"
#' )
#' print(pts_centrality_build_graph)
#' }
st_closeness_centrality <- function(data, graph = NULL, placename = NULL, transport_mode = NULL, batched_if = 100000, normalized = TRUE) {

  # --- Input Validation and Graph Preparation ---

  # Case 1: 'data' is already a dodgr_streetnet graph
  if (inherits(data, "dodgr_streetnet")) {
    message("Input 'data' is a dodgr_streetnet graph. Calculating closeness for all its vertices.")
    input_graph <- data
    input_is_sf_points <- FALSE # Flag to indicate original input type
  }
  # Case 2: 'data' is an sf object (points), and a 'graph' is supplied
  else if (inherits(data, "sf") && !is.null(graph)) {
    message("Input 'data' is an sf object, and a 'graph' is supplied. Using the supplied graph.")
    input_graph <- graph
    input_is_sf_points <- TRUE
    # Ensure points CRS is compatible with dodgr's expectation (often geographic)
    old_crs <- sf::st_crs(data)
    if (sf::st_is_longlat(data) == FALSE) {
      data <- sf::st_transform(data, crs = "EPSG:4326")
      message("Warning: Input 'data' CRS transformed to EPSG:4326 for distance calculation.")
    }
  }
  # Case 3: 'data' is an sf object (points), and no 'graph' is supplied, so build one
  else if (inherits(data, "sf") && is.null(graph)) {
    message("Input 'data' is an sf object, and no graph is supplied. Building graph using 'placename' and 'transport_mode'.")
    input_is_sf_points <- TRUE
    # Validate placename and transport_mode
    if (is.null(placename) || is.null(transport_mode)) {
      stop("If 'data' is an sf object and no 'graph' is supplied, both 'placename' and 'transport_mode' must be provided.")
    }
    # Ensure points CRS is compatible with dodgr's expectation (often geographic)
    old_crs <- sf::st_crs(data)
    if (sf::st_is_longlat(data) == FALSE) {
      data <- sf::st_transform(data, crs = "EPSG:4326")
      message("Warning: Input 'data' CRS transformed to EPSG:4326 for distance calculation.")
    }
    # Build graph from placename
    message(paste0("Getting street network for '", placename, "' with mode '", transport_mode, "'."))
    bb <- osmdata::getbb(place_name = placename)
    if (is.null(bb)) {
      stop(paste0("Could not find bounding box for placename: ", placename, ". Please check the name."))
    }
    net <- dodgr::dodgr_streetnet(bb)
    input_graph <- dodgr::weight_streetnet(net, wt_profile = transport_mode)
  }
  # Error for unsupported 'data' type
  else {
    stop("Input 'data' must be an 'sf' dataframe of points or a 'dodgr_streetnet' object.")
  }

  # Ensure the graph has a 'component' column and filter to the largest component
  # This helps prevent issues with disconnected graphs where distances might be NA.
  # The dodgr_distances function handles unreachable nodes by returning NA,
  # but focusing on the largest component makes results more robust.
  if (!"component" %in% names(input_graph)) {
    input_graph <- dodgr::dodgr_components(input_graph)
  }
  input_graph <- input_graph[input_graph$component == 1, ]
  message(paste0("Using the largest connected component of the graph (containing ",
                 max(input_graph$component_rank), " vertices)."))

  # --- Closeness Centrality Calculation ---

  # Extract 'from' points for distance calculation
  if (input_is_sf_points) {
    from_points <- sf::st_coordinates(data) %>%
      as.data.frame() %>%
      dplyr::rename(x = X, y = Y) # dodgr expects 'x' and 'y' columns
    n_elements <- nrow(from_points)
  } else { # If data was a dodgr graph, calculate for all its vertices
    from_points <- dodgr::dodgr_vertices(input_graph)
    n_elements <- nrow(from_points)
  }

  message("Starting distance calculation.")

  if (n_elements > batched_if) {
    message(paste0("Number of elements (", n_elements, ") exceeds 'batched_if' (", batched_if, "). Calculating distances in batches."))
    closeness_values <- st_closeness_centrality_largedata(input_graph, normalized, chunk_size = batched_if) # Use batched_if as chunk_size
  } else {
    message(paste0("Number of elements (", n_elements, ") is within 'batched_if' (", batched_if, "). Calculating all-pairs distances."))

    # Calculate distances from 'from_points' to all other accessible nodes in the graph
    # If input_is_sf_points is TRUE, dodgr_dists is appropriate for specified 'from' and 'to' (which are the same set)
    # If input_is_sf_points is FALSE, dodgr_distances calculates all-pairs among graph vertices
    if (input_is_sf_points) {
      testdistances <- dodgr::dodgr_dists(graph = input_graph, from = from_points)
    } else {
      testdistances <- dodgr::dodgr_distances(graph = input_graph)
    }

    # Handle NA values and self-distances
    # dodgr_distances/dists returns NA for unreachable nodes.
    # Find out which nodes were particularily bad to reach - they will be eliminated later.
    points_above_avg_na <- rownames(testdistances)[rowSums(is.na(testdistances)) > mean(rowSums(is.na(testdistances)))]

    # Unnormalized closeness centrality is the inverse of the sum of all distances
    # to a given point in the network.
    # Normalized closeness centrality is normalized by the number of nodes in
    # the network, so the mean can be used to calculate it.
    # Use na.rm = TRUE in rowMeans/rowSums.
    if (normalized) {
      closeness_values <- 1 / rowMeans(testdistances, na.rm = TRUE)
    } else {
      closeness_values <- 1 / rowSums(testdistances, na.rm = TRUE)
    }
  }

  message("Distance calculation finished.")

  # --- Prepare Output ---

  if (input_is_sf_points) {
    # If original input was sf points, add closeness to it
    data$closeness <- closeness_values
    # Remove all values which are unreachable from parts of the network
    data = data %>% filter(! id %in% points_above_avg_na)
    # If CRS was changed, revert it
    if (exists("old_crs")) {
      data <- sf::st_transform(data, crs = old_crs)
    }
    return(data)
  } else {
    # If original input was a dodgr graph, return vertices with closeness
    vertices_with_closeness <- from_points
    vertices_with_closeness$closeness <- closeness_values
    # Remove all values which are unreachable from parts of the network
    vertices_with_closeness = vertices_with_closeness %>% filter(! id %in% points_above_avg_na)
    vertices_with_closeness <- sf::st_as_sf(vertices_with_closeness, coords = c("x", "y"), crs = "EPSG:4326")
    message("Returning graph vertices with calculated closeness.")
    return(vertices_with_closeness)
  }
}

#' A function to find closeness centrality in large dodgr graphs using batch processing
#'
#' This helper function calculates closeness centrality for each vertex in a large
#' `dodgr` graph by processing distances in batches. This approach reduces memory
#' consumption compared to calculating all-pairs distances at once, which can
#' result in a very large distance matrix (n^2).
#'
#' @param graph A `dodgr_streetnet` graph.
#' @param normalized Logical. If `TRUE`, normalized closeness centrality is computed
#'   (mean of distances). If `FALSE`, unnormalized closeness is computed (sum of distances).
#' @param chunk_size The number of vertices for which distances should be calculated
#'   at once in each iteration. Defaults to `1000`. This directly controls memory usage.
#'
#' @return A numeric vector of closeness values, one for each vertex in the graph.
#' @export
#'
#' @examples
#' library(dodgr)
#' library(sf)
#' # Create a sample graph (using hampi, which is small, but demonstrates the function)
#' graph_hampi <- dodgr::weight_streetnet(hampi, wt_profile = "foot")
#'
#' # Calculate closeness using the batch processing function
#' closeness_values_batched <- st_closeness_centrality_largedata(graph_hampi, normalized = TRUE, chunk_size = 50)
#' head(closeness_values_batched)
st_closeness_centrality_largedata <- function(graph, normalized, chunk_size = 1000) {

  # Extract unique vertices from the graph
  nodes <- dodgr::dodgr_vertices(graph)
  n_nodes <- nrow(nodes)

  # Initialize result vector for closeness values
  closeness_normal_dodgr <- numeric(n_nodes)
  # Initialize a logical vector to track which nodes were successfully processed
  processed_nodes <- logical(n_nodes)

  message(paste0("Starting batched calculation for ", n_nodes, " nodes with a chunk size of ", chunk_size, "."))

  # Iterate through nodes in chunks
  for (i in seq(1, n_nodes, chunk_size)) {
    end <- min(i + chunk_size - 1, n_nodes)
    current_chunk_indices <- i:end
    current_nodes_in_chunk <- nodes[current_chunk_indices, ]

    message(paste0("  Processing chunk: nodes ", i, " to ", end, " out of ", n_nodes, "."))

    # Calculate distances from nodes in the current chunk to all other nodes in the graph
    # dodgr_distances takes either 'from' (specific nodes) or no 'from'/'to' (all-pairs for graph vertices)
    # Here, 'from' is current_nodes_in_chunk, 'to' is all graph vertices (implicit).
    testdistances_chunk <- dodgr::dodgr_distances(graph, from = current_nodes_in_chunk)

    # Handle cases where a node in the chunk is unreachable from any other node
    # These will result in all NA values in their respective row in testdistances_chunk.
    # Such nodes will receive an NA closeness value, or 0 if na.rm = TRUE makes it too small.
    # We explicitly remove nodes that are entirely unreachable.
    unreachable_in_chunk <- rowSums(is.na(testdistances_chunk)) == ncol(testdistances_chunk)
    valid_distances_chunk <- testdistances_chunk[!unreachable_in_chunk, ]
    valid_chunk_indices <- current_chunk_indices[!unreachable_in_chunk]

    if (length(valid_chunk_indices) > 0) {
      # Calculate closeness centrality for the valid nodes in the current chunk
      if (normalized) {
        closeness_values_chunk <- 1 / rowMeans(valid_distances_chunk, na.rm = TRUE)
      } else {
        closeness_values_chunk <- 1 / rowSums(valid_distances_chunk, na.rm = TRUE)
      }
      # Assign results back to the main result vector
      closeness_normal_dodgr[valid_chunk_indices] <- closeness_values_chunk
      processed_nodes[valid_chunk_indices] <- TRUE
    }

    if (any(unreachable_in_chunk)) {
      message(paste0("    Warning: ", sum(unreachable_in_chunk), " node(s) in this chunk were unreachable from any other point and will have NA closeness."))
    }
  }

  # For any nodes that were not processed (i.e., had all NA distances), set their closeness to NA
  closeness_normal_dodgr[!processed_nodes] <- NA

  message("Batched calculation finished.")
  return(closeness_normal_dodgr)
}
