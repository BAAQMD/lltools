addSolidPolygons <- function (leaflet_map, color, opacity, fillOpacity = opacity, stroke = FALSE, ...) {
  addPolygons(leaflet_map, fillColor = color, color = color, stroke = stroke, fillOpacity = fillOpacity, ...)
}
