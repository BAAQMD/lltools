#' Set bounds of a leaflet map
#'
#' Based on the bounding box of an object you supply.
#'
#' @param map leaflet map object
#' @param obj `sf` or `Spatial` object, or a named list or vector
#'
#' @docType methods
#' @rdname setViewBounds-methods
#' @export
setGeneric("setViewBounds", function (map, obj, ...){
  standardGeneric("setViewBounds")
})

#' @rdname setViewBounds-methods
#' @aliases setViewBounds,leaflet,list-method
setMethod("setViewBounds", c("leaflet", "list"), function (map, obj, ...){
  with(obj, leaflet::fitBounds(map, xmin, ymin, xmax, ymax))
})

#' @rdname setViewBounds-methods
#' @aliases setViewBounds,leaflet,sfc-method
setMethod("setViewBounds", c("leaflet", "sfc"), function (map, obj, ...) {
  bb <- sf::st_bbox(sf::st_transform(obj, WGS84_GPS))
  setViewBounds(map, as.list(bb), ...)
})

#' @rdname setViewBounds-methods
#' @aliases setViewBounds,leaflet,sf-method
setMethod("setViewBounds", c("leaflet", "sf"), function (map, obj, ...) {
  bb <- sf::st_bbox(sf::st_transform(obj, WGS84_GPS))
  setViewBounds(map, as.list(bb), ...)
})

#' @rdname setViewBounds-methods
setMethod("setViewBounds", c("leaflet", "Spatial"), function (map, obj, ...) {
  bb <- sp::bbox(sp::spTransform(obj, st_crs(WGS84_GPS)$proj4string))
  setViewBounds(map, as.list(bb), ...)
})
