#' @include addOverlay.R
#' @describeIn addOverlay add Circles layer
#' @export
addCirclesOverlay <- purrr::partial(addOverlay, fun = addCircles)
