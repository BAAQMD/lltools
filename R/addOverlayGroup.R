#' addOverlayGroup
#'
#' @param map_object leaflet map
#' @param group character
#' @param visible `TRUE` or `FALSE`
#'
#' @return modified leaflet map
#'
#' @export
addOverlayGroup <- function (map_object, group, visible = FALSE) {
  overlay_groups <- getOverlayGroups(map_object)
  overlay_groups[group] <- visible
  map_object <- setOverlayGroups(map_object, overlay_groups)
  return(map_object)
}
