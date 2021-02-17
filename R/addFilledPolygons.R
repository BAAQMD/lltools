#' @export
addFilledPolygons <- function (
  map_object,
  data,
  stroke = FALSE,
  weight = 1.0,
  color = gray(0.3),
  fill = TRUE,
  fillColor = gray(0.8),
  fillOpacity = 1.0,
  ...,
  verbose = getOption("verbose")
) {

  leaflet::addPolygons(
    map_object,
    stroke = stroke,
    weight = weight,
    color = color,
    fill = fill,
    fillColor = fillColor,
    fillOpacity = fillOpacity,
    data = st_transform(data, WGS84_GPS))

}
