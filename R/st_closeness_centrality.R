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
#'   `st_closeness_centrality_largedata`. Set to `10000` by default. This value
#'   should be lowered if memory allocation errors occur.
#' @param normalized A logical value. If `TRUE` (default), closeness is normalized
#'   by the number of reachable nodes, representing the mean of distances. If `FALSE`,
#'   it is the inverse of the sum of distances.
#' @param threshold_reachable A numeric value between 0 and 1. Defines the tolerance
#'   for filtering points based on reachability. Points with NA counts above this
#'   quantile threshold are removed. Default is 0.9, meaning points unreachable from more than
#'   90\% of nodes in the network are discarded. Set to 1 to keep all points.
#'
#' @return An `sf` dataframe of the input `data` points (or the graph vertices
#'   if `data` was a `dodgr_streetnet`), with an additional column 'closeness'
#'   and with CRS "EPSG:4326".
#'
#' @importFrom sf st_coordinates st_as_sf st_crs st_transform st_is_longlat
#' @importFrom dodgr dodgr_dists dodgr_vertices dodgr_components weight_streetnet dodgr_streetnet
#' @importFrom osmdata getbb
#' @importFrom stats setNames quantile
#'
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
#' \dontrun{
#' # This example requires internet access to download OSM data
#' pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
#' pts <- sf::st_as_sf(pts)
#' sf::st_crs(pts) <- "EPSG:4326"
#'
#' pts_centrality_build_graph <- st_closeness_centrality(
#'   pts,
#'   placename = "Hampi, India",
#'   transport_mode = "bicycle"
#' )
#' print(pts_centrality_build_graph)
#' }
#'
st_closeness_centrality <- function(data = NULL,
                                    graph = NULL,
                                    placename = NULL,
                                    transport_mode = NULL,
                                    batched_if = 10000,
                                    normalized = TRUE,
                                    threshold_reachable = 0.9) {
  # --- Input Validation ---
  .validate_closeness_params(threshold_reachable, batched_if, normalized)

  # --- Graph Preparation ---
  prep_result <- .prepare_graph_and_data(
    data = data,
    graph = graph,
    placename = placename,
    transport_mode = transport_mode
  )

  input_graph <- prep_result$input_graph
  input_is_sf_points <- prep_result$input_is_sf_points
  data_internal <- prep_result$data_internal
  original_crs <- prep_result$original_crs

  # --- Filter to Largest Connected Component ---
  input_graph <- .filter_to_largest_component(input_graph)

  # --- Prepare Points for Distance Calculation ---
  if (input_is_sf_points) {
    from_points <- .prepare_sf_points(data_internal)
    n_elements <- nrow(from_points)
    # Store row IDs for later filtering (before any operations modify the data)
    row_ids <- from_points$id
  } else {
    from_points <- dodgr::dodgr_vertices(input_graph)
    n_elements <- nrow(from_points)
    row_ids <- from_points$id
  }

  message("Starting distance calculation.")

  # --- Calculate Closeness Centrality ---
  if (n_elements > batched_if) {
    message(paste0(
      "Number of elements (", n_elements, ") exceeds 'batched_if' (", batched_if,
      "). Calculating distances in batches."
    ))

    calc_result <- st_closeness_centrality_largedata(
      graph = input_graph,
      from_points = if (input_is_sf_points) from_points else NULL,
      normalized = normalized,
      chunk_size = batched_if,
      threshold_reachable = threshold_reachable
    )
    closeness_values <- calc_result$closeness_values
    points_low_reachability <- calc_result$nodes_to_filter_ids
  } else {
    message(paste0(
      "Number of elements (", n_elements, ") is within 'batched_if' (", batched_if,
      "). Calculating all-pairs distances."
    ))

    calc_result <- .calculate_closeness_direct(
      graph = input_graph,
      from_points = if (input_is_sf_points) from_points else NULL,
      normalized = normalized,
      threshold_reachable = threshold_reachable
    )
    closeness_values <- calc_result$closeness_values
    points_low_reachability <- calc_result$nodes_to_filter_ids
  }

  message("Distance calculation finished.")

  # --- Prepare Output ---
  result <- .prepare_output(
    input_is_sf_points = input_is_sf_points,
    data_internal = data_internal,
    from_points = from_points,
    closeness_values = closeness_values,
    points_low_reachability = points_low_reachability,
    row_ids = row_ids,
    original_crs = original_crs
  )

  return(result)
}


#' Calculate Closeness Centrality for Large Networks Using Batch Processing
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
#' @param threshold_reachable A numeric value between 0 and 1. Defines the tolerance
#'   for filtering points based on reachability. Points with NA counts above this
#'   quantile threshold are removed. Default is 0.9, meaning points unreachable from more than
#'   90\% of nodes in the network are discarded. Set to 1 to keep all points.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `closeness_values`: A named numeric vector of closeness values, one for each
#'       vertex/point from which distances were calculated.
#'     \item `nodes_to_filter_ids`: A character vector of node/point IDs that should be
#'       filtered out due to low reachability.
#'   }
#'
#' @export
#'
#' @examples
#' library(dodgr)
#' library(sf)
#'
#' graph_hampi <- dodgr::weight_streetnet(hampi, wt_profile = "foot")
#'
#' # Example 1: Calculate closeness for all graph vertices using batch processing
#' closeness_result <- st_closeness_centrality_largedata(
#'   graph_hampi,
#'   normalized = TRUE,
#'   chunk_size = 50
#' )
#' head(closeness_result$closeness_values)
#'
#' # Example 2: Calculate closeness from specific points using batch processing
#' pts <- sf::st_sfc(
#'   sf::st_point(c(76.47398, 15.330)),
#'   sf::st_point(c(76.47398, 15.150))
#' )
#' pts <- sf::st_as_sf(pts)
#' sf::st_crs(pts) <- "EPSG:4326"
#' pts_df <- as.data.frame(sf::st_coordinates(pts))
#' names(pts_df) <- c("x", "y")
#' pts_df$id <- as.character(seq_len(nrow(pts_df)))
#'
#' closeness_result_pts <- st_closeness_centrality_largedata(
#'   graph = graph_hampi,
#'   from_points = pts_df,
#'   normalized = TRUE,
#'   chunk_size = 1
#' )
#' head(closeness_result_pts$closeness_values)
st_closeness_centrality_largedata <- function(graph,
                                              from_points = NULL,
                                              normalized = TRUE,
                                              chunk_size = 1000,
                                              threshold_reachable = 0.9) {
  # --- Input Validation ---
  if (!inherits(graph, "dodgr_streetnet")) {
    stop("'graph' must be a dodgr_streetnet object.")
  }
  if (!is.numeric(chunk_size) || chunk_size < 1) {
    stop("'chunk_size' must be a positive integer.")
  }
  if (!is.numeric(threshold_reachable) || threshold_reachable < 0 || threshold_reachable > 1) {
    stop("'threshold_reachable' must be a numeric value between 0 and 1.")
  }

  # --- Determine Nodes to Process ---
  if (!is.null(from_points)) {
    if (!"id" %in% names(from_points)) {
      stop("'from_points' must contain an 'id' column for batched processing.")
    }
    nodes_to_process <- from_points
    use_from_points <- TRUE
  } else {
    nodes_to_process <- dodgr::dodgr_vertices(graph)
    use_from_points <- FALSE
  }

  node_ids <- nodes_to_process$id
  n_nodes <- nrow(nodes_to_process)

  # --- Initialize Result Vectors ---
  closeness_values <- numeric(n_nodes)
  names(closeness_values) <- node_ids

  na_counts_per_node <- numeric(n_nodes)
  names(na_counts_per_node) <- node_ids

  processed_nodes <- logical(n_nodes)
  names(processed_nodes) <- node_ids

  message(paste0(
    "Starting batched calculation for ", n_nodes,
    " nodes with chunk size of ", chunk_size, "."
  ))

  # --- Process in Batches ---
  for (i in seq(1, n_nodes, chunk_size)) {
    end_idx <- min(i + chunk_size - 1, n_nodes)
    chunk_indices <- i:end_idx
    current_chunk <- nodes_to_process[chunk_indices, ]

    message(paste0(
      "  Processing chunk: nodes ", i, " to ", end_idx, " of ", n_nodes, "."
    ))

    # Calculate distances for current chunk
    distance_matrix_chunk <- dodgr::dodgr_dists(
      graph = graph,
      from = current_chunk,
      to = current_chunk
    )

    # Track NA counts for reachability filtering
    chunk_na_counts <- rowSums(is.na(distance_matrix_chunk))
    na_counts_per_node[names(chunk_na_counts)] <- chunk_na_counts

    # Identify completely unreachable nodes
    unreachable_mask <- chunk_na_counts == ncol(distance_matrix_chunk)
    n_unreachable <- sum(unreachable_mask)

    if (n_unreachable > 0) {
      message(paste0(
        "    Warning: ", n_unreachable,
        " node(s) in this chunk are completely unreachable."
      ))
    }

    # Calculate closeness for reachable nodes
    valid_rows <- !unreachable_mask
    if (any(valid_rows)) {
      valid_distances <- distance_matrix_chunk[valid_rows, , drop = FALSE]
      valid_ids <- rownames(valid_distances)

      if (normalized) {
        chunk_closeness <- 1 / rowMeans(valid_distances, na.rm = TRUE)
      } else {
        chunk_closeness <- 1 / rowSums(valid_distances, na.rm = TRUE)
      }

      closeness_values[valid_ids] <- chunk_closeness
      processed_nodes[valid_ids] <- TRUE
    }
  }

  # --- Mark Unprocessed Nodes as NA ---
  closeness_values[!processed_nodes] <- NA

  # --- Identify Low Reachability Nodes ---
  threshold_value <- stats::quantile(na_counts_per_node, threshold_reachable, na.rm = TRUE)
  low_reachability_ids <- names(na_counts_per_node)[na_counts_per_node > threshold_value]

  if (length(low_reachability_ids) > 0) {
    message(paste0(
      "Identified ", length(low_reachability_ids),
      " nodes with low reachability for filtering."
    ))
  }

  message("Batched calculation finished.")

  return(list(
    closeness_values = closeness_values,
    nodes_to_filter_ids = low_reachability_ids
  ))
}


# =============================================================================
# INTERNAL HELPER FUNCTIONS
# =============================================================================

#' Validate Common Parameters for Closeness Centrality
#' @noRd
.validate_closeness_params <- function(threshold_reachable, batched_if, normalized) {
  if (!is.numeric(threshold_reachable) ||
    length(threshold_reachable) != 1 ||
    threshold_reachable < 0 ||
    threshold_reachable > 1) {
    stop("'threshold_reachable' must be a single numeric value between 0 and 1.")
  }

  if (!is.numeric(batched_if) ||
    length(batched_if) != 1 ||
    batched_if < 1) {
    stop("'batched_if' must be a single positive integer.")
  }

  if (!is.logical(normalized) || length(normalized) != 1) {
    stop("'normalized' must be TRUE or FALSE.")
  }
}


#' Build a Street Network Graph from a Place Name
#' @noRd
.build_graph_from_placename <- function(placename, transport_mode) {
  if (is.null(placename) || is.null(transport_mode)) {
    stop("Both 'placename' and 'transport_mode' must be provided to build a graph.")
  }

  message(paste0(
    "Getting street network for '", placename,
    "' with mode '", transport_mode, "'."
  ))

  bounding_box <- osmdata::getbb(place_name = placename)

  if (is.null(bounding_box)) {
    stop(paste0(
      "Could not find bounding box for placename: '", placename,
      "'. Please check the name."
    ))
  }

  street_network <- dodgr::dodgr_streetnet(bounding_box)
  weighted_graph <- dodgr::weight_streetnet(street_network, wt_profile = transport_mode)

  return(weighted_graph)
}


#' Prepare Graph and Data Based on Input Types
#' @noRd
.prepare_graph_and_data <- function(data, graph, placename, transport_mode) {
  input_graph <- NULL
  input_is_sf_points <- FALSE
  data_internal <- NULL
  original_crs <- NULL

  # Case 1: 'data' is a dodgr_streetnet graph
  if (inherits(data, "dodgr_streetnet")) {
    message("Input 'data' is a dodgr_streetnet graph. Calculating closeness for all vertices.")
    input_graph <- data
    input_is_sf_points <- FALSE
  }
  # Case 2: 'data' is an sf object
  else if (inherits(data, "sf")) {
    message("Input 'data' is an sf object.")
    input_is_sf_points <- TRUE
    data_internal <- data

    # Handle CRS transformation
    if (!sf::st_is_longlat(data_internal)) {
      original_crs <- sf::st_crs(data_internal)
      data_internal <- sf::st_transform(data_internal, crs = "EPSG:4326")
      warning("Input 'data' CRS transformed to EPSG:4326 for distance calculation.")
    }

    # Determine which graph to use
    if (!is.null(graph)) {
      message("Using supplied 'graph' for calculations.")
      input_graph <- graph
    } else {
      message("No graph supplied. Building graph from 'placename' and 'transport_mode'.")
      input_graph <- .build_graph_from_placename(placename, transport_mode)
    }
  }
  # Case 3: No data, but graph provided
  else if (is.null(data) && inherits(graph, "dodgr_streetnet")) {
    message("Using supplied 'graph'. Calculating closeness for all vertices.")
    input_graph <- graph
    input_is_sf_points <- FALSE
  }
  # Case 4: No data, no graph - build from placename
  else if (is.null(data) && is.null(graph)) {
    message("No data or graph supplied. Building graph from 'placename' and 'transport_mode'.")
    input_graph <- .build_graph_from_placename(placename, transport_mode)
    input_is_sf_points <- FALSE
  }
  # Error case
  else {
    stop(paste0(
      "Invalid input combination. Provide one of:\n",
      "  - 'data' as an sf dataframe or dodgr_streetnet\n",
      "  - 'graph' as a dodgr_streetnet\
",
      "  - 'placename' and 'transport_mode' to build a graph"
    ))
  }

  return(list(
    input_graph = input_graph,
    input_is_sf_points = input_is_sf_points,
    data_internal = data_internal,
    original_crs = original_crs
  ))
}


#' Filter Graph to Largest Connected Component
#' @noRd
.filter_to_largest_component <- function(graph) {
  if (!"component" %in% names(graph)) {
    graph <- dodgr::dodgr_components(graph)
  }

  graph_filtered <- graph[graph$component == 1, ]
  n_vertices <- nrow(dodgr::dodgr_vertices(graph_filtered))

  message(paste0(
    "Using largest connected component (", n_vertices, " vertices)."
  ))

  return(graph_filtered)
}


#' Prepare SF Points for Distance Calculation
#' @noRd
.prepare_sf_points <- function(sf_data) {
  coords <- as.data.frame(sf::st_coordinates(sf_data))
  names(coords) <- c("x", "y")
  coords$id <- as.character(seq_len(nrow(sf_data)))

  message("Assigned row numbers as temporary identifiers for sf points.")

  return(coords)
}


#' Calculate Closeness Centrality Directly (Non-batched)
#' @noRd
.calculate_closeness_direct <- function(graph, from_points, normalized, threshold_reachable) {
  # Calculate distance matrix
  if (!is.null(from_points)) {
    distance_matrix <- dodgr::dodgr_dists(graph = graph, from = from_points, to = from_points)
  } else {
    distance_matrix <- dodgr::dodgr_dists(graph = graph)
  }

  # Calculate NA counts for reachability filtering
  na_counts <- rowSums(is.na(distance_matrix))
  threshold_value <- stats::quantile(na_counts, threshold_reachable)
  low_reachability_ids <- rownames(distance_matrix)[na_counts > threshold_value]

  if (length(low_reachability_ids) > 0) {
    message(paste0(
      "Identified ", length(low_reachability_ids),
      " points with low reachability for filtering."
    ))
  }

  # Calculate closeness values
  if (normalized) {
    closeness_values <- 1 / rowMeans(distance_matrix, na.rm = TRUE)
  } else {
    closeness_values <- 1 / rowSums(distance_matrix, na.rm = TRUE)
  }

  return(list(
    closeness_values = closeness_values,
    nodes_to_filter_ids = low_reachability_ids
  ))
}


#' Prepare Final Output
#' @noRd
.prepare_output <- function(input_is_sf_points,
                            data_internal,
                            from_points,
                            closeness_values,
                            points_low_reachability,
                            row_ids,
                            original_crs) {
  if (input_is_sf_points) {
    # Assign closeness values to the internal data copy
    if (!is.null(names(closeness_values))) {
      closeness_map <- stats::setNames(closeness_values, names(closeness_values))
      data_internal$closeness <- closeness_map[row_ids]
    } else {
      data_internal$closeness <- closeness_values
      warning("Closeness values not named. Assuming order matches input points.")
    }

    # Filter using stored row IDs
    keep_mask <- !(row_ids %in% points_low_reachability)

    if (!any(keep_mask)) {
      warning("All points were filtered due to low reachability. Returning empty sf object.")
    } else if (sum(!keep_mask) > 0) {
      message(paste0("Filtered ", sum(!keep_mask), " points due to low reachability."))
    }

    result <- data_internal[keep_mask, ]

    # Restore original CRS if it was changed
    if (!is.null(original_crs)) {
      result <- sf::st_transform(result, crs = original_crs)
      message("Restored original CRS.")
    }

    return(result)
  } else {
    # Output for graph vertices
    vertices_result <- from_points

    if (!is.null(names(closeness_values))) {
      closeness_map <- stats::setNames(closeness_values, names(closeness_values))
      vertices_result$closeness <- closeness_map[vertices_result$id]
    } else {
      vertices_result$closeness <- closeness_values
      warning("Closeness values not named. Assuming order matches graph vertices.")
    }

    # Filter low reachability vertices
    keep_mask <- !(vertices_result$id %in% points_low_reachability)

    if (!any(keep_mask)) {
      warning("All vertices were filtered due to low reachability. Returning empty sf object.")
    } else if (sum(!keep_mask) > 0) {
      message(paste0("Filtered ", sum(!keep_mask), " vertices due to low reachability."))
    }

    vertices_result <- vertices_result[keep_mask, ]

    # Convert to sf object
    result <- sf::st_as_sf(
      vertices_result,
      coords = c("x", "y"),
      crs = "EPSG:4326"
    )

    message("Returning graph vertices with calculated closeness.")

    return(result)
  }
}
