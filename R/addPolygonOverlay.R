#' @include addOverlay.R
#' @describeIn addOverlay add Polygon layer
#' @export
addPolygonOverlay <- purrr::partial(addOverlay, fun = addPolygons)
