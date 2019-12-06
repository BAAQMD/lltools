addTileServerLayer <- function (
  leaflet_map,
  layer_name,
  group_name = layer_name,
  tile_options = tileOptions(),
  ...,
  verbose = getOption("verbose")
) {
  msg <- function (...) if(isTRUE(verbose)) message("[addTileServerLayer] ", ...)
  msg("adding ", layer_name, " as \"", group_name, "\"")
  leaflet::addTiles(
    leaflet_map,
    layer_name,
    group = group_name,
    options = tile_options,
    ...)
}

#' @export
addTileServerLayers <- function (
  leaflet_map,
  tileset_providers,
  tile_options,
  ...
) {
  reduce2(
    tileset_providers,
    names(tileset_providers),
    .f = addTileServerLayer,
    .init = leaflet_map,
    tile_options = tile_options,
    ...)
}
