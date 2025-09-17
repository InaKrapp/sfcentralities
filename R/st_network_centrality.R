#' st_network_centrality
#'
#' This function determines closeness centrality in a street network.
#'
#' @param data A sf dataframe or a dodgr streetnetwork
#' @param placename A string giving the name of a place from which a streetnetwork should be taken. In case the input is a dodgr graph, it will be ignored.
#' @param transport_mode A string. It has to be a valid transport mode. In case the input is a dodgr graph, it will be ignored.
#' @param graph A dodgr graph. It can be supplied and used for distance calculation, or the user supplies a place name and a mode of transport.
#' @param batched_if The number of elements in the graph for which distances should be calculated at once. Set to 1000 by default. It is used as a number of rows: For any graph with more rows than this number, distance is calculated in batches. For smaller graphs, it is created at once. As a rule, this number should be lowered if you receive an 'Error: cannot allocate vector of size ... Gb' error.
#' @param normalized If the result should be normalized. Unnormalized closeness is the inverse of the sum of distances between points in the dataset, normalized closeness is the mean of distances.
#'
#' @return A sf dataframe of the points in 'data', with a column 'closeness' and the crs "EPSG:4326".
#' @export
#'
#' @examples
#' # The simplest example calculates the closeness of points within a network/graph:
#' library(dodgr)
#' graph = dodgr::weight_streetnet(dodgr::hampi, wt_profile = "bicycle")
#' closeness <- st_closeness_centrality(graph)
#' # Probably more common may be to find centrality values for specific points:
#' graph = dodgr::weight_streetnet(dodgr::hampi, wt_profile = "bicycle")
#' pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
#' pts <- sf::st_as_sf(pts)
#' sf::st_crs(pts) <- "EPSG:4326"
#' sf::st_geometry(pts) <- "geom"
#' pts$name = c("city center", "train station")
#' pts <- st_closeness_centrality(pts, graph = graph)
st_closeness_centrality <- function(data, transport_mode = NULL, placename = NULL,graph = NULL, batched_if = 100000, normalized = TRUE) {
  # What exactly should be used as input? A graph?
  # Or even a sf object/sc object, I create the graph here?
  # May make it easier - but only special sf objects can be used to
  # create graphs
  # Also, how do I deal with the transport mode?
  # It only needs to be given if I create the graph in the function.
  # For a sf, should I test the crs like I do for st_network_distance?
  # I should add a separate graph option to supply the graph for uses where
  # other data is on focus.

  # Ensure input is a dodgr graph or sf:
  if (inherits(data, "dodgr_streetnet")) {
    print("Using data as dodgr graph.")
    graph <- data
  if (nrow(graph) > batched_if) { # Calculate distances piecewise
    closeness <- st_closeness_centrality_largedata(graph, normalized)
  } else { # Calculate distances all at once
    testdistances <- dodgr::dodgr_distances(graph)
    # In case a point is not reachable from another point, set distance to NA:
    testdistances[testdistances == 0] <- NA
    # The exception is the distance of a point to itself, set it to 0:
    diag(testdistances) <- 0
    if (normalized == TRUE) {
      closeness <- 1 / rowMeans(testdistances, na.rm = T)
    } else {
      closeness <- 1 / rowSums(testdistances, na.rm = T)
    }}

  # Add calculated points to vertices
  vertices <- dodgr::dodgr_vertices(graph)
  vertices <- cbind(vertices, closeness)
  vertices <- sf::st_as_sf(vertices, coords = c("x", "y"))
  sf::st_crs(vertices) <- "EPSG:4326"
  print("Returning vertices.")
  return(vertices)}

    else if (inherits(data, "sf")) {
      # Check if all geometries are POINT
      if (!all(sf::st_geometry_type(data) == "POINT")) {
        stop("sf object does not contain only POINT objects")
      }
      # data is a sf object, adjust crs:
      # The crs has to be "EPSG:4326", else transform it for distance calculations:
      if (sf::st_crs(data)$input != "EPSG:4326") {
        old_crs <- sf::st_crs(data)$input
        data <- sf::st_transform(data, crs = "EPSG:4326")}}
      else stop("Input must be a sf or dodgr_streetnet object")

    # If the data given is a dodgr graph, use it as graph:

  # If no graph is given (neither as 'data', nor as 'graph')
  if (is.null(graph)){
    if (is.null(transport_mode)) {
      stop("A placename and a mode of transport must be supplied if no graph is supplied to the function.")
    }
    if (is.null(placename)) {
      stop("A placename and a mode of transport must be supplied if no graph is supplied to the function.")
    }
    # Return error if placename and transport mode are not supplied:
      bb <- osmdata::getbb(place_name = placename)
      net <- dodgr::dodgr_streetnet(bb)
      graph <- dodgr::weight_streetnet(net, transport_mode)
    }
  print("Starting distance calculation.")
  # Keep only largest connected part of the graph:
  # graph = graph[graph$component == 1, ]
  # Should I leave that mandatory? It works without this limitation?

  # Once I have data and a graph, calculate closeness:

  # Take coordinates for which closeness centrality should be computed.
  from <- sf::st_coordinates(data)
  to <- sf::st_coordinates(data)
  # Calculate distances:
  testdistances <- dodgr::dodgr_dists(graph = graph, from = from, to = to)
  # In case a point is not reachable from another point, set distance to NA:
  testdistances[testdistances == 0] <- NA
  # The exception is the distance of a point to itself, set it to 0:
  diag(testdistances) <- 0
  # Calculate closeness centrality from distances:
  if (normalized == TRUE) {
    closeness <- 1 / rowMeans(testdistances, na.rm = T)
  } else {
    closeness <- 1 / rowSums(testdistances, na.rm = T)}
  print("Distance calculation finished.")
  #return(testdistances)
  data = cbind(data, closeness)
  # If the crs was changed for distance calculation, reverse the change:
  if (exists("old_crs")) data <- sf::st_transform(data, crs = old_crs)
  return(data)
}

#' A function to find points with minimal sums of distances in large dodgr graphs
#'
#' This function allows to find points whose sum of distances to other
#' points is small in large dodgr graphs. It does so by calculating the sum of distances between
#' points in a batch and all points.
#' and only keeping the point (or points) with minimal sums before continuing
#' to the next batch. This allows to find a point with a minimal sum of distances
#' to other points when the dodgr graph is too large to calculate distances
#' between all points at once.
#' Dodgr is well parallelized, so the calculation itself is not usually
#' the bottleneck. The main reason to calculate distances in batches instead
#' of all at once is that calculation of distances between n points creates
#' a result of the dimension n^2, which may be too large to store in memory.
#' Note that this function therefore only keeps the computed distances to
#' few points.
#'
#' @param graph A dodgr graph
#' @param normalized Compute normalized closeness centrality? Expects TRUE or FALSE
#' @param chunk_size The number of elements in the graph for which distances should be calculated at once. Set to 1000 by default.
#'
#' @return A vector of closeness values, one for each of the vertices.
#' @export
#'
#' @examples
#' library(dodgr)
#' # Create graph of hampi, a dataset within the dodgr package, for demonstration
#' graph <- weight_streetnet(hampi, wt_profile = "foot")
#' point_candidates <- st_closeness_centrality_largedata(graph, normalized = TRUE)
st_closeness_centrality_largedata <- function(graph, normalized, chunk_size = 1000) {
  # Determine the number of nodes
  # Extract unique node IDs from 'from' and 'to' columns
  nodes <- dodgr::dodgr_vertices(graph)
  n_nodes <- nrow(nodes)

  # Initialize result vector
  closeness_normal_dodgr <- numeric(n_nodes)

  # Basic idea: I don't need all the distances at once,
  # only their sums.
  for (i in seq(1, n_nodes, chunk_size)) {
    end <- min(i + chunk_size - 1, n_nodes)
    current_nodes <- nodes[i:end, ]
    # This takes 1000 chunks at once.
    # print(i)

    # 'from' returns 1 row for each from-entry
    # I work along the rows: I use rowSums/rowMeans.
    testdistances <- dodgr::dodgr_distances(graph, from = current_nodes)
    # In case a point is not reachable from another point, set distance to NA:
    testdistances[testdistances == 0] <- NA
    # The exception is the distance of a point to itself, set it to 0:
    diag(testdistances) <- 0
    if (normalized == TRUE) {
      closeness <- 1 / rowMeans(testdistances, na.rm = T)
    } else {
      closeness <- 1 / rowSums(testdistances, na.rm = T)
    }
    # Assign results
    closeness_normal_dodgr[i:end] <- closeness
  }

  return(closeness_normal_dodgr)
}
# Example code to use this with the Berlin streetnetwork, assuming
# it is stored in the same directory in a rds file 'Berlin_streetnetwork'.
# Berlin_streetnetwork <- readRDS("Berlin_streetnetwork.rds")
# graph = weight_streetnet(Berlin_streetnetwork, wt_profile = "motorcar")
# colnames(graph) = c("from_id", "to_id", "edge_id", "from_lon", "from_lat", "to_lon", "to_lat",  "d", "object_", "bicycle" , "foot" , "highway"  ,"oneway_bicycle" ,"lanes",  "junction", "d_weighted" ,"time"  ,"time_weighted" , "component")
# graph = graph[graph$component == 1, ]
# test = st_closeness_centrality(graph)
# saveRDS(test, file = "Berlin_centralpoint.rds")
# Add example with Berlin data using only highways?
