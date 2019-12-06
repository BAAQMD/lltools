#' @export
showGroups <- function (leaflet_map, ...) {
  reduce(list(...), showGroup, .init = leaflet_map)
}
