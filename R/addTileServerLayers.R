#' @export
addTileServerLayer <- function (
  map_object,
  tileset,
  group,
  options = pathOptions(),
  ...,
  verbose = getOption("verbose")
) {
  msg <- function (...) if(isTRUE(verbose)) message("[addTileServerLayer] ", ...)
  msg("adding ", tileset, " as \"", group, "\"")
  leaflet::addTiles(
    map_object,
    urlTemplate = tileset,
    group = group,
    options = options,
    ...)
}

#' @export
addTileServerLayers <- function (
  map_object,
  tilesets,
  ...
) {
  reduce2(
    tilesets,
    names(tilesets),
    .f = addTileServerLayer,
    .init = map_object,
    ...)
}
