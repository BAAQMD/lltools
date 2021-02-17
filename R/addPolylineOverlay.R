#' @include addOverlay.R
#' @describeIn addOverlay add Polyline layer
#' @export
addPolylineOverlay <- function (
  map_object,
  ...,
  color = "black",
  weight = 2.0,
  opacity = 1.0
) {

  addOverlay(
    map_object,
    fun = addPolylines,
    color = color,
    weight = weight,
    opacity = opacity,
    ...)

}
