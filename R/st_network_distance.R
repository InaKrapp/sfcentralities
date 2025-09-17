#'
#' This function calculates street distances. The user can enter a dodgr graph, else it will automatically draw one from Openstreetmap.
#'
#' @param data A sf dataframe
#' @param to_data A second sf dataframe. The function calculates the distances from all points in the first to all points in the second dataset.
#' @param index The name of a column of the second dataframe. Its values are used to name the distances if it is supplied. Else, distances will be numbered.
#' @param placename If no graph is supplied, the user has to enter a place whose street net should be used for distance calculations.
#' @param transport_mode If no graph is supplied, the user has to enter a transport mode. Allowed values are “bicycle”, “foot”, “goods”, “hgv”, “horse”, “moped”, “motorcar”, “motorcycle”, “psv”, or “wheelchair”.
#' @param graph A dodgr graph. It can be supplied and used for distance calculation, or the user supplies a place name and a mode of transport.
#' @param calculate_times Decide if travel times should be computed (defaults to FALSE). If FALSE, the function computes spatial distances instead.
#' @return The sf dataframe 'data' with the calculated distances as additional columns.
#' @export
#'
#' @examples
#' # Create example graph:
#' graph <- dodgr::weight_streetnet(dodgr::hampi, "bicycle")
#' pts <- sf::st_sfc(sf::st_point(c(76.47398, 15.330)), sf::st_point(c(76.47398, 15.150)))
#' pts <- sf::st_as_sf(pts)
#' sf::st_crs(pts) <- "EPSG:3857"
#' pts2 <- pts[1, ]
#' result <- st_network_distance(data = pts, to = pts2, graph = graph)
st_network_distance <- function(data, to_data = NULL, index = NULL, placename = NULL, transport_mode = NULL, graph = NULL, calculate_times = FALSE) {
  # Check that data is a sf objects:
  if (!inherits(data, "sf")) {
    stop("data must be a sf object")
  }
  # The crs has to be "EPSG:4326", else transform it for distance calculations:
  if (sf::st_crs(data)$input != "EPSG:4326") {
    old_crs <- sf::st_crs(data)$input
    data <- sf::st_transform(data, crs = "EPSG:4326")
  } # Add check that the sf objects contain only points?
  # If only a single dataset is supplied, copy it and calculate distances from all points in the dataset to each other.
  if (is.null(to_data)) {
    to_data <- data
  }
  # Else, check that to_data is a sf object and has the correct CRS:
  if (!inherits(to_data, "sf")) {
    stop("to_data must be a sf object")
  }
  if (sf::st_crs(to_data)$input != "EPSG:4326") {
    to_data <- sf::st_transform(to_data, crs = "EPSG:4326")
  }

  from <- sf::st_coordinates(data)
  to <- sf::st_coordinates(to_data)
  # If graph is NULL, get the graph from Openstreetmap
  # Add check that placename and transport_mode are added?
  if (is.null(graph)) {
    if (is.null(placename)) {
      stop("A placename must be supplied.")
    }
    if (is.null(transport_mode)) {
      stop("A mode of transport must be supplied.")
    }
    # If graph is NULL, get the graph from Openstreetmap
    bb <- osmdata::getbb(place_name = placename)
    net <- dodgr::dodgr_streetnet(bb)
    graph <- dodgr::weight_streetnet(net, wt_profile = transport_mode)
  }
  if (isTRUE(calculate_times)) {
    distances <- dodgr::dodgr_times(graph = graph, from = from, to = to)
    unit = "time"
    }
  else {
    distances <- dodgr::dodgr_dists(graph = graph, from = from, to = to)
    unit = "distance"
    }
  # Assign results to the data object, naming them after the index supplied.
  # If no index is given: Use row numbers from to_data:
  if (is.null(index)) {
    for (i in 1:ncol(distances)) {
      data[[paste0(unit, "_to_", i)]] <- distances[, i]
    }
  } else if (index %in% colnames(to_data)) {
    to_data_without_geometry <- sf::st_drop_geometry(to_data)
    for (i in 1:ncol(distances)) {
      data[[paste0(unit, "_to_", to_data_without_geometry[[index]][i])]] <- distances[, i]
    }
  } else {
    # print a warning: Index not found.
    print("Index not found in the dataset.")
    print(index %in% colnames(to_data))
    for (i in 1:ncol(distances)) {
      data[[paste0(unit, "_to_", i)]] <- distances[, i]
    }
  }
  # If the crs was changed for distance calculation, reverse the change:
  if (exists("old_crs")) data <- sf::st_transform(data, crs = old_crs)
  return(data)
}
