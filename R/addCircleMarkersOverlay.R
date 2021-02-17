#' @include addOverlay.R
#' @describeIn addOverlay add CircleMarkers layer
#' @export
addCircleMarkersOverlay <- purrr::partial(addOverlay, fun = addCircleMarkers)
