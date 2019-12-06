addProviderLayer <- function (
  leaflet_map,
  layer_name,
  group_name = layer_name,
  tile_options = providerTileOptions(),
  attribution = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addProviderLayer] ", ...)
  msg("adding ", layer_name, " as \"", group_name, "\"")

  if (!is.null(attribution)) {
    tile_options$attribution <- attribution
  }

  addProviderTiles(
    leaflet_map,
    providers[[layer_name]],
    group = group_name,
    options = tile_options,
    ...)

}

#' @export
addProviderLayers <- function (
  leaflet_map,
  tileset_providers,
  tile_options,
  ...
) {
  reduce2(
    tileset_providers,
    names(tileset_providers),
    .f = addProviderLayer,
    .init = leaflet_map,
    tile_options = tile_options,
    ...)
}
