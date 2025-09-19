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
#' library(dodgr)
#' library(sf)
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
#'
#' pts_centrality_with_graph <- st_closeness_centrality(pts, graph = graph_hampi)
#' print(pts_centrality_with_graph)
#'
#' # Example 3: Find centrality values for specific points, letting the function build the graph
#' \dontrun{ # This example requires internet access to download OSM data
#' pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
#' pts <- sf::st_as_sf(pts_no_id)
#' sf::st_crs(pts) <- "EPSG:4326"
#'
#' pts_centrality_build_graph <- st_closeness_centrality(
#'   pts,
#'   placename = "Hampi, India",
#'   transport_mode = "bicycle"
#' )
#' print(pts_centrality_build_graph)
#' }
st_closeness_centrality <- function(data = NULL, graph = NULL, placename = NULL, transport_mode = NULL, batched_if = 100000, normalized = TRUE) {

  # --- Input Validation and Graph Preparation ---

  # Case 1: 'data' is already a dodgr_streetnet graph
  if (inherits(data, "dodgr_streetnet") ) {
    message("Input 'data' is a dodgr_streetnet graph. Calculating closeness for all its vertices.")
    input_graph <- data
    input_is_sf_points <- FALSE # Flag to indicate original input type
    # dodgr_vertices naturally has an 'id' column, which we will use as the identifier.
  }
  # Case 2: 'data' is an sf object (points)
  else if (inherits(data, "sf")) {
    input_is_sf_points <- TRUE

    # CRITICAL: Create a copy of the sf object for internal processing.
    # This ensures the original `data` object passed by the user is not modified.
    data_for_processing <- data
    rm(data) # Remove the original 'data' reference to avoid accidental use

    # Ensure points CRS is compatible with dodgr's expectation (often geographic)
    old_crs <- sf::st_crs(data_for_processing)
    if (sf::st_is_longlat(data_for_processing) == FALSE) {
      data_for_processing <- sf::st_transform(data_for_processing, crs = "EPSG:4326")
      message("Warning: Input 'data' CRS transformed to EPSG:4326 for distance calculation.")
    }

    # Decide which graph to use or build
    if (!is.null(graph)) {
      message("Input 'data' is an sf object, and a 'graph' is supplied. Using the supplied graph.")
      input_graph <- graph
    } else { # No 'graph' supplied, so build one
      message("Input 'data' is an sf object, and no graph is supplied. Building graph using 'placename' and 'transport_mode'.")
      # Validate placename and transport_mode
      if (is.null(placename) || is.null(transport_mode)) {
        stop("If 'data' is an sf object and no 'graph' is supplied, both 'placename' and 'transport_mode' must be provided.")
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
  }
  # Case 3: Only the name of a place and the transport mode are supplied
  else if (is.null(data) && is.null(graph)) {
    # Validate placename and transport_mode
    if (is.null(placename) || is.null(transport_mode)) {
      stop("If no 'graph' is supplied, both 'placename' and 'transport_mode' must be provided.")
    }
    # Build graph from placename
    message(paste0("Getting street network for '", placename, "' with mode '", transport_mode, "'."))
    bb <- osmdata::getbb(place_name = placename)
    if (is.null(bb)) {
      stop(paste0("Could not find bounding box for placename: ", placename, ". Please check the name."))
    }
    net <- dodgr::dodgr_streetnet(bb)
    input_graph <- dodgr::weight_streetnet(net, wt_profile = transport_mode)
    input_is_sf_points <- FALSE
  }
  # Case 4: No data is supplied, but a graph.
  else if (is.null(data) && inherits(graph, "dodgr_streetnet")) {
    message("Input 'graph' is a dodgr_streetnet graph. Calculating closeness for all its vertices.")
    input_graph <- graph
    input_is_sf_points <- FALSE # Flag to indicate original input type
  }
  # Error for unsupported 'data' type
  else {
    stop("Input 'data' must be an 'sf' dataframe of points or a 'dodgr_streetnet' object. If 'data' is not supplied, a graph or a placename and a transport_mode have to be supplied.")
  }

  # Ensure the graph has a 'component' column and filter to the largest component
  if (!"component" %in% names(input_graph)) {
    input_graph <- dodgr::dodgr_components(input_graph)
  }
  input_graph <- input_graph[input_graph$component == 1, ]
  message(paste0("Using the largest connected component of the graph (containing ",
                 max(input_graph$component_rank), " vertices)."))

  # --- Closeness Centrality Calculation ---

  # Extract 'from' points for distance calculation
  if (input_is_sf_points) {
    from_points <- as.data.frame(sf::st_coordinates(data_for_processing))
    names(from_points) <- c("x", "y")
    # Assign row numbers as character IDs for dodgr_dists.
    # These will become the rownames of the dodgr_dists output.
    from_points$id <- as.character(seq_len(nrow(data_for_processing)))
    message("Using row numbers as temporary identifiers for sf points.")
    n_elements <- nrow(from_points)
  } else { # If data was a dodgr graph, calculate for all its vertices
    from_points <- dodgr::dodgr_vertices(input_graph)
    # dodgr_vertices naturally has an 'id' column, which serves as the identifier.
    n_elements <- nrow(from_points)
  }

  message("Starting distance calculation.")

  points_above_avg_na <- character(0) # Initialize as empty character vector

  if (n_elements > batched_if) {
    message(paste0("Number of elements (", n_elements, ") exceeds 'batched_if' (", batched_if, "). Calculating distances in batches."))

    # Call the largedata function, passing `from_points` if applicable
    largedata_result <- st_closeness_centrality_largedata(
      graph = input_graph,
      from_points = if (input_is_sf_points) from_points else NULL, # Pass from_points only if input_is_sf_points
      normalized = normalized,
      chunk_size = batched_if
    )
    closeness_values <- largedata_result$closeness_values
    points_above_avg_na <- largedata_result$nodes_to_filter_ids # Store filter IDs

  } else {
    message(paste0("Number of elements (", n_elements, ") is within 'batched_if' (", batched_if, "). Calculating all-pairs distances."))

    if (input_is_sf_points) {
      # dodgr_dists will use from_points$id (our row numbers as characters) as rownames
      testdistances <- dodgr::dodgr_dists(graph = input_graph, from = from_points)
    } else {
      # dodgr_distances will use graph vertex IDs as rownames
      testdistances <- dodgr::dodgr_distances(graph = input_graph)
    }

    # Filtering logic: rownames(testdistances) will be our identifiers
    points_above_avg_na <- rownames(testdistances)[rowSums(is.na(testdistances)) > mean(rowSums(is.na(testdistances)))]

    if (normalized) {
      closeness_values <- 1 / rowMeans(testdistances, na.rm = TRUE)
    } else {
      closeness_values <- 1 / rowSums(testdistances, na.rm = TRUE)
    }
  }

  message("Distance calculation finished.")

  # --- Prepare Output ---

  if (input_is_sf_points) {
    # It's safer to map closeness values to IDs explicitly
    # closeness_values should be named according to the IDs used in calculation (our row numbers)
    if (!is.null(names(closeness_values))) {
      closeness_map <- stats::setNames(closeness_values, names(closeness_values))
      # Assign closeness back to the data_for_processing based on row numbers (as characters)
      # Ensure data_for_processing's rownames are also characters for consistent lookup
      data_for_processing$closeness <- closeness_map[as.character(seq_len(nrow(data_for_processing)))]
    } else {
      # Fallback: if closeness_values are not named, assume order matches. (less robust)
      data_for_processing$closeness <- closeness_values
      warning("Closeness values were not named. Assuming order matches input points for assignment. This might be unreliable.")
    }

    # Apply filtering using the collected identifiers (row numbers as characters)
    # We compare the current row numbers (as character strings) with the list of identifiers to remove.
    data_for_processing <- data_for_processing[!(as.character(seq_len(nrow(data_for_processing))) %in% points_above_avg_na), ]

    # No temporary ID column to remove, as we used row numbers.

    # If CRS was changed, revert it
    if (exists("old_crs")) {
      data_for_processing <- sf::st_transform(data_for_processing, crs = old_crs)
    }
    return(data_for_processing)
  } else {
    # This block handles when 'data' was a dodgr_streetnet.
    # 'from_points' (vertices_with_closeness) is created new here.
    # 'dodgr_vertices' provides an 'id' column by default.
    vertices_with_closeness <- from_points
    if (!is.null(names(closeness_values))) {
      closeness_map <- stats::setNames(closeness_values, names(closeness_values))
      vertices_with_closeness$closeness <- closeness_map[vertices_with_closeness$id]
    } else {
      vertices_with_closeness$closeness <- closeness_values
      warning("Closeness values were not named. Assuming order matches graph vertices for assignment. This might be unreliable.")
    }

    # Apply filtering using the collected IDs (from dodgr_vertices$id)
    vertices_with_closeness <- vertices_with_closeness[!(vertices_with_closeness$id %in% points_above_avg_na), ]
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
#' result in a very large distance matrix (n^2). It can also calculate distances
#' from a large set of specified points (`from_points`) to all graph vertices in batches.
#'
#' @param graph A `dodgr_streetnet` graph.
#' @param from_points An optional `data.frame` of points (with `x`, `y`, `id` columns)
#'   from which distances should be calculated. If `NULL` (default), distances are
#'   calculated for all vertices of `graph`. The `id` column should contain unique
#'   identifiers, typically row numbers as character strings if `from_points` originated
#'   from an `sf` object in `st_closeness_centrality`.
#' @param normalized Logical. If `TRUE`, normalized closeness centrality is computed
#'   (mean of distances). If `FALSE`, unnormalized closeness is computed (sum of distances).
#' @param chunk_size The number of vertices/points for which distances should be calculated
#'   at once in each iteration. Defaults to `1000`. This directly controls memory usage.
#'
#' @return A list containing:
#'   - `closeness_values`: A numeric vector of closeness values, one for each
#'     vertex/point from which distances were calculated. These values are named
#'     with the corresponding IDs (row numbers or graph vertex IDs).
#'   - `nodes_to_filter_ids`: A character vector of node/point IDs that should be filtered
#'     out because they are less reachable than average.
#' @export
#'
#' @examples
#' library(dodgr)
#' library(sf)
#'
#' graph_hampi <- dodgr::weight_streetnet(hampi, wt_profile = "foot")
#'
#' # Example 1: Calculate closeness for all graph vertices using batch processing
#' closeness_values_batched_graph <- st_closeness_centrality_largedata(graph_hampi,
#'  normalized = TRUE, chunk_size = 50)
#' head(closeness_values_batched_graph$closeness_values)
#'
#' # Example 2: Calculate closeness from specific points to graph vertices using batch processing
#' pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
#' pts <- sf::st_as_sf(pts)
#' sf::st_crs(pts) <- "EPSG:4326"
#' pts_df <- as.data.frame(sf::st_coordinates(pts))
#' names(pts_df) <- c("x", "y")
#' pts_df$id <- as.character(seq_len(nrow(pts_df))) # Use row numbers as IDs
#'
#' closeness_values_batched_points <- st_closeness_centrality_largedata(
#'   graph = graph_hampi,
#'   from_points = pts_df,
#'   normalized = TRUE,
#'   chunk_size = 1 # Small chunk_size for example
#' )
#' head(closeness_values_batched_points$closeness_values)
st_closeness_centrality_largedata <- function(graph, from_points = NULL, normalized, chunk_size = 1000) {

  # Determine whether we are processing graph vertices or specific 'from_points'
  if (!is.null(from_points)) {
    # If from_points is provided, we are calculating distances FROM these points.
    # The 'nodes_to_process' are the 'from_points' themselves.
    nodes_to_process <- from_points
    # Ensure from_points has an 'id' column for robust mapping (expected from main function)
    if (!"id" %in% names(nodes_to_process)) {
      stop("`from_points` must contain an 'id' column for batched processing.")
    }
  } else {
    # If from_points is NULL, we are calculating distances for all graph vertices.
    nodes_to_process <- dodgr::dodgr_vertices(graph)
  }

  node_ids <- nodes_to_process$id
  n_nodes_to_process <- nrow(nodes_to_process)

  closeness_normal_dodgr <- numeric(n_nodes_to_process)
  names(closeness_normal_dodgr) <- node_ids # Name the result vector with node/point IDs
  processed_nodes <- logical(n_nodes_to_process)
  na_counts_per_node <- numeric(n_nodes_to_process)
  names(na_counts_per_node) <- node_ids # Name this vector too for consistent filtering

  message(paste0("Starting batched calculation for ", n_nodes_to_process, " nodes with a chunk size of ", chunk_size, "."))

  for (i in seq(1, n_nodes_to_process, chunk_size)) {
    end <- min(i + chunk_size - 1, n_nodes_to_process)
    current_chunk_indices <- i:end
    current_nodes_in_chunk <- nodes_to_process[current_chunk_indices, ]

    message(paste0("  Processing chunk: nodes ", i, " to ", end, " out of ", n_nodes_to_process, "."))

    # Determine which dodgr function to call based on `from_points`
    if (!is.null(from_points)) {
      # Calculate distances FROM current_nodes_in_chunk TO ALL graph vertices
      # dodgr_dists will use current_nodes_in_chunk$id for rownames
      testdistances_chunk <- dodgr::dodgr_dists(graph = graph, from = current_nodes_in_chunk)
    } else {
      # Calculate distances between graph vertices, FROM current_nodes_in_chunk TO ALL graph vertices
      # dodgr_distances will use current_nodes_in_chunk$id for rownames
      testdistances_chunk <- dodgr::dodgr_distances(graph = graph, from = current_nodes_in_chunk)
    }

    current_chunk_na_counts <- rowSums(is.na(testdistances_chunk))
    # Assign by index to the full vector, assuming `nodes_to_process` maintains original order.
    # The names of `current_chunk_na_counts` will be `current_nodes_in_chunk$id`, which is what we need.
    na_counts_per_node[names(current_chunk_na_counts)] <- current_chunk_na_counts


    unreachable_in_chunk <- rowSums(is.na(testdistances_chunk)) == ncol(testdistances_chunk)
    valid_distances_chunk <- testdistances_chunk[!unreachable_in_chunk, ]
    valid_chunk_ids <- rownames(valid_distances_chunk) # Get IDs of valid rows

    if (length(valid_chunk_ids) > 0) {
      if (normalized) {
        closeness_values_chunk <- 1 / rowMeans(valid_distances_chunk, na.rm = TRUE)
      } else {
        closeness_values_chunk <- 1 / rowSums(valid_distances_chunk, na.rm = TRUE)
      }
      # Assign results back to the main result vector using names (IDs) for robustness
      closeness_normal_dodgr[valid_chunk_ids] <- closeness_values_chunk
      # Mark processed using indices corresponding to the IDs
      processed_nodes[match(valid_chunk_ids, node_ids)] <- TRUE
    }

    if (any(unreachable_in_chunk)) {
      message(paste0("    Warning: ", sum(unreachable_in_chunk), " node(s) in this chunk were unreachable from any other point and will have NA closeness."))
    }
  }

  # For any nodes that were not processed (i.e., had all NA distances), set their closeness to NA
  closeness_normal_dodgr[!processed_nodes] <- NA

  mean_global_na_count <- mean(na_counts_per_node, na.rm = TRUE)
  # Filter using the named vector and the mean
  points_above_avg_na_ids <- names(na_counts_per_node)[na_counts_per_node > mean_global_na_count]

  message("Batched calculation finished.")
  return(list(
    closeness_values = closeness_normal_dodgr,
    nodes_to_filter_ids = points_above_avg_na_ids
  ))
}
