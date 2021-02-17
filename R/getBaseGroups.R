#' getBaseGroups
#'
#' @param map_object leaflet map
#'
#' @return character
#'
#' @seealso [setBaseGroups()]
#'
#' @export
getBaseGroups <- function (map_object) {
  return(attr(map_object, "base_groups"))
}
