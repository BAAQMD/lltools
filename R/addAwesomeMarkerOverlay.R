#' @include addOverlay.R
#' @describeIn addOverlay add AwesomeMarkers layer
#' @export
addAwesomeMarkerOverlay <- purrr::partial(addOverlay, fun = addAwesomeMarkers)

