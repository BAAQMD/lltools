#' addBaseGroup
#'
#' @param map_object leaflet map
#' @param group character
#' @param visible `TRUE` or `FALSE`
#'
#' @return modified leaflet map
#'
#' @export
addBaseGroup <- function (map_object, group, visible = FALSE) {
  base_groups <- getBaseGroups(map_object)
  base_groups[group] <- visible
  map_object <- setBaseGroups(map_object, base_groups)
  return(map_object)
}
