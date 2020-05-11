#' @export
hideGroups <- function (leaflet_map, ...) {
  reduce(list(...), hideGroup, .init = leaflet_map)
}
