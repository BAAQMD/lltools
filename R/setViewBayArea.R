#' Recenter and zoom map to fit SF Bay Area
#'
#' @param leaflet_map created with `leaflet()` or `leaflet_SFBA()`
#'
#' @seealso [leaflet_SFBA()]
#'
#' @export
setViewBayArea <- function (leaflet_map, lng = NULL, lat = NULL, zoom = NULL, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[setViewBayArea] ", ...)

  if (is.null(lng)) lng <- -122.2
  if (is.null(lat)) lat <- 37.75
  if (is.null(zoom)) zoom <- 9

  msg("setting view to ", abs(lng), "ºW, ", lat, "ºN", ", zoom = ", zoom)
  setView(leaflet_map, lng = lng, lat = lat, zoom = zoom, ...)

}

#' @describeIn setViewBayArea (alias)
#' @export
#' @note just an alias
setViewSFBA <- setViewBayArea
