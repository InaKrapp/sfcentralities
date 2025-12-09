#' Calculate geometric medians from a matrix
#'
#' This function is a vectorized version of the function 'geo_median'
#' from the pracma package. It should give exactly the same geometric
#' median and sum of distances.
#'
#' @param P A matrix of points whose geometric median will be calculated.
#' @param tol The tolerance
#' @param maxiter The maximal number of iterations
#'
#' @return A list containing the geometric median, distance,
#' tolerance and number of iterations before arriving at the point.
#' @export
#'
#' @examples
#' P <- matrix(c(0, 1, 2, 0, 1.5, 2), 3, 2)
#' geometric_median_point <- geo_median_inner(P)
geo_median_inner <- function(P, tol = 1e-07, maxiter = 200) {
  # Get numbers of rows and columns.
  m <- nrow(P)
  n <- ncol(P)

  p0 <- apply(P, 2, mean) # Calculate geometric mean as starting point.
  p1 <- p0 + 1

  iter <- 1
  while (max(abs(p0 - p1)) > tol && iter < maxiter) {
    iter <- iter + 1 # Count iterations
    p0 <- p1 # Set old point to be the new initial geometric median candidate.
    s1 <- s2 <- 0 # Initialize s1 and s2.
    # Calculate distance between each point of P and the candidate point.
    A <- t(t(P) - p0)
    distances <- sqrt(rowSums(A^2))
    d <- sum(distances)
    s1 <- colSums(P / distances)
    s2 <- sum(1 / distances)
    # Calculate new geometric median candidate point.
    p1 <- s1 / s2
  }
  if (iter >= maxiter) {
    warning("Maximum number of iterations reached; may not converge.")
  }

  d <- 0
  A <- t(t(P) - p0)
  distances <- sqrt(rowSums(A^2))
  d <- sum(distances)

  return(list(p = p1, d = d, reltol = max(abs(p0 - p1)), niter = iter))
}

#' Calculate geometric medians from a sf object.
#'
#' @param data A sf object based on which the geometric medians are calculated.
#' @param group If the geometric medians should be calculated
#' grouped by a certain variable. If no group is supplied, a single geometric
#' median is calculated based on all points of the sf object.
#'
#' @return A sf dataframe of the calculated geometric medians.
#' @export
#'
#' @examples
#' library(sf)
#' pts <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2.2)))
#' pts <- st_as_sf(pts)
#' st_crs(pts) <- "EPSG:3857"
#' pts$index <- "I"
#' test <- st_geo_median(pts, "index")
#' test <- st_geo_median(pts)
st_geo_median <- function(data, group = NULL) {
  if (!inherits(data, "sf")) {
    stop("Input must be an sf object")
  }
  # Test that input has a crs attached to it.
  if (is.na(sf::st_crs(data))) {
    stop("The sf object supplied does not have a defined CRS. Please add the crs with st_crs().")
  }
  # Test that the crs is a projected crs, which is required for distance calculations.
  if (sf::st_crs(data)$input == "EPSG:4326") {
    stop("The sf object supplied has a geographical crs. This is invalid for distance calculations.
         Before calculating the geometric median, please transform the data to a projected crs with st_transform().")
  }

  if (is.null(group)) { # If no group variable is set, calculate geometric median for all points at once.
    if (nrow(data) < 3) {
      # If there are less than 3 points in the dataset, apply rules for few points.
      geomedian_list <- st_geo_median_fewpoints(data)
    } else {
      coords <- sf::st_coordinates(data)
      geomedian_list <- geo_median_inner(coords)
    }

    # Initialize an empty dataframe.
    df <- data.frame(matrix(NA, ncol = 0, nrow = 1))

    # Put elements of list in dataframe
    df$x <- geomedian_list$p[["X"]]
    df$y <- geomedian_list$p[["Y"]]
    df$distance <- geomedian_list$d
    df$reltol <- geomedian_list$reltol
    df$niter <- geomedian_list$niter
    df$number_of_points <- nrow(data)

    # Make a sf out of the dataframe
    df <- sf::st_as_sf(df, coords = c("x", "y"))
  } else {
    # Apply geometric median function to subgroups in dataset
    groupcolumn <- data[, group, drop = FALSE]
    groupcolumn <- sf::st_drop_geometry(groupcolumn)
    groupvector <- unique(groupcolumn[[1]])

    geomedian_list <- vector(mode = "list", length = length(groupvector))

    for (i in seq_along(groupvector)) {
      partdata <- data[data[[group]] == groupvector[i], ]
      if (nrow(partdata) < 3) {
        geomedian <- st_geo_median_fewpoints(partdata)
      } else {
        coords <- sf::st_coordinates(partdata)
        geomedian <- geo_median_inner(coords)
      }
      geomedian["pointnumber"] <- nrow(partdata)
      geomedian_list[[i]] <- geomedian
    }
    names(geomedian_list) <- groupvector

    df <- data.frame(
      name = names(geomedian_list),
      stringsAsFactors = FALSE
    )
    # Turn list of geomedians into a sf object
    # Add a control that the list is not empty. Else, return warning.
    df$distance <- sapply(geomedian_list, function(x) x$d)
    df$reltol <- sapply(geomedian_list, function(x) x$reltol)
    df$niter <- sapply(geomedian_list, function(x) x$niter)
    df$number_of_points <- sapply(geomedian_list, function(x) x$pointnumber)

    df$x <- sapply(geomedian_list, function(x) x$p[1])
    df$y <- sapply(geomedian_list, function(x) x$p[2])

    df <- sf::st_as_sf(df, coords = c("x", "y"))
  }
  sf::st_crs(df) <- sf::st_crs(data)
  return(df)
}

st_geo_median_fewpoints <- function(data) {
  # If only a single point is given, return the point.
  if (nrow(data) == 1) {
    p <- as.vector(sf::st_coordinates(data))
    names(p) <- c("X", "Y")
    return(list(p = p, d = 0, reltol = 0, niter = 0))
  } else {
    # If two points are given, return the point in the midth between them.
    p <- sf::st_coordinates(data)
    pointmeans <- colMeans(p)
    d <- sqrt((p[, 1] - pointmeans["X"])^2 + (p[, 2] - pointmeans["Y"])^2)
    d <- d[1] + d[2]
    return(list(p = pointmeans, d = d, reltol = 0, niter = 0))
  }
}
