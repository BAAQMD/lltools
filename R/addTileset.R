#' addTileset
#'
#' @param map_object
#' @param tileset
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
addTileset <- function (
  map_object,
  tileset,
  group,
  pane = "tileset",
  zIndex = 0,
  attribution = NULL,
  options = pathOptions(),
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addTileset] ", ...)


  map_object <-
    map_object %>%
    addMapPane(
      pane,
      zIndex = zIndex) %>%
    addTileServerLayer(
      tileset,
      group,
      attribution = attribution,
      options = rlang::exec(pathOptions, !!!options, pane = pane),
      verbose = verbose) %>%
    addBaseGroup(
      group)

  # used by resetLayersControl()
  #attr(map_object, "tileset") <- tileset

  return(map_object)

}
