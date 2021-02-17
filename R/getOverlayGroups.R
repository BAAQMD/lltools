#' getOverlayGroups
#'
#' @param map_object leaflet map
#'
#' @return character
#'
#' @seealso [setOverlayGroups()]
#'
#' @export
getOverlayGroups <- function (map_object) {

  overlay_groups <-
    attr(map_object, "overlay_groups")

  if (is.null(overlay_groups)) {
    map_object <- setOverlayGroups(map_object, logical(0))
    return(getOverlayGroups(map_object))
  }

  return(overlay_groups)

}
