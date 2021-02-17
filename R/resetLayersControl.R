#' resetLayersControl
#'
#' @param map_object
#' @param options
#' @param ...
#'
#' @return
#' @export
#'
resetLayersControl <- function (
  map_object,
  tileset = NULL,
  options = layersControlOptions(collapsed = TRUE),
  ...
) {

  map_object <-
    map_object %>%
    removeLayersControl() %>%
    addLayersControl(
      baseGroups = names(getBaseGroups(.)),
      overlayGroups = names(getOverlayGroups(.)),
      options = options,
      ...)

  tileset <-
    (tileset %||% attr(map_object, "tileset"))

  if (isFALSE(is.null(tileset))) {
    map_object <-
      map_object %>%
      showGroup(tileset)
  }

  map_object <-
    map_object %>%
    hideGroups(
      names(keep(getOverlayGroups(.), is_false)))

  return(map_object)

}
