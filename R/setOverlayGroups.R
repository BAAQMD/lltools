#' setOverlayGroups
#'
#' @param map_object leaflet map
#' @param value character
#'
#' @seealso
#' - [addOverlayGroup()]
#' - [getOverlayGroups()]
#'
#' @return modified leaflet map
#'
#' @export
setOverlayGroups <- function (map_object, value) {
  attr(map_object, "overlay_groups") <- value
  return(map_object)
}
