#' @include addOverlay.R
#' @describeIn addOverlay add Markers layer
#' @export
addMarkerOverlay <- purrr::partial(addOverlay, fun = addMarkers)
