#' setBaseGroups
#'
#' @param map_object leaflet map
#' @param value character
#'
#' @return map_object, with attribute `base_groups` set to `value`
#'
#' @seealso [getBaseGroups()]
#'
#' @export
setBaseGroups <- function (map_object, value) {
  attr(map_object, "base_groups") <- value
  return(map_object)
}
