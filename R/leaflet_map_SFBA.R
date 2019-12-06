#' Leaflet map of the SF Bay Area
#'
#' @param ... arguments to [leaflet::leaflet()]
#' @param lng (numeric) longitude, in decimal degrees, center of map
#' @param lat (numeric) latitude, in decimal degrees, for center of map
#'
#'
#' @examples
#' leaflet_map_SFBA(zoom = 9)
#' leaflet_map_SFBA(zoom = 9, layer_controls = FALSE)
#'
#' @seealso [leaflet_map_facilities()]
#'
#' @export
leaflet_map_SFBA <- function (
  data,
  tileset,
  ...,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  max_zoom = NULL,
  options = NULL,
  layer_controls = TRUE,
  tileset_providers = NULL,
  tile_options = tileOptions(opacity = 1.0),
  attribution = NULL,
  verbose = getOption("verbose")
) {

  require(strtools)

  msg <- function (...) if(isTRUE(verbose)) message("[leaflet_map_SFBA] ", ...)

  if (missing(data)) {
    data <- NULL
  }

  if (is.null(options)) {
    options <- leafletOptions(minZoom = 8, maxZoom = max_zoom)
  }

  if (is.null(tileset_providers)) {

    tileset_providers <- list(
      "Default" = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
      "Unlabeled" = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_nolabels/{z}/{x}/{y}.png",
      #"Satellite" = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{x}/{y}",
      "Satellite" = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}",
      "Terrain" = "http://mt0.google.com/vt/lyrs=p&hl=en&x={x}&y={y}&z={z}",
      #"Roads" = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}",
      "Roads" = "http://mt0.google.com/vt/lyrs=r&hl=en&x={x}&y={y}&z={z}",
      "Census" = "http://demographics.virginia.edu/DotMap/tiles4/{z}/{x}/{y}.png",
      #"GrayCanvas" = "Esri.WorldGrayCanvas",
      "Toner" = "http://stamen-tiles-a.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png",
      "Blank" = "")

    #tileset_providers <- list(
    #  "Default" = "CartoDB.Positron",
    #  "Unlabeled" = "CartoDB.PositronNoLabels",
    #  #"GrayCanvas" = "Esri.WorldGrayCanvas",
    #  "Toner" = "Stamen.TonerLite",
    #  #"Terrain" = "Stamen.TerrainBackground",
    #  "Satellite" = "Esri.WorldImagery")

  }

  msg("names(tileset_providers) is: ", str_csv(names(tileset_providers)))

  if (missing(tileset)) {
    tileset <- first(names(tileset_providers))
    msg("`tileset` not provided; defaulting to '", tileset, "'")
  }

  if (tileset %not_in% names(tileset_providers)) {
    stop_msg <- str_c("'", tileset, "' isn't one of the default tileset providers. ",
                      "But, you can pass in your own with `tileset_providers`. ",
                      "See ?leaflet_map_SFBA() for more.")
    stop(stop_msg)
  }

  # HACK to make sure the first layer named in `tileset_providers` is the one shown by default
  if (!isTRUE(layer_controls)) {
    tileset_providers <- rev(tileset_providers)
  }

  map_crs <- st_crs(4326)

  CARE_plus_boundaries <-
    st_union(CARE::CARE_region_geodata) %>%
    st_transform(map_crs) # FIXME: cache this

  LIGHT_PURPLE <- "#7f7fff"
  CARE_color <- LIGHT_PURPLE

  if (inherits(data, "sf")) {
    if (!isTRUE(st_is_longlat(data))) {
      msg("reprojecting to EPSG 4326 (GPS, WGS84 lng/lat)")
      data <- st_transform(data, map_crs)
    }
  }

  leaflet_map <-
    leaflet(data = data, ..., options = options) %>%
    leaflet.extras::setMapWidgetStyle(list(background = "white")) %>%
    #addProviderLayers(tileset_providers, tile_options = tile_options, attribution = attribution, verbose = verbose) %>%
    addTileServerLayers(tileset_providers, attribution = attribution, tile_options = tile_options, verbose = verbose) %>%
    addPolygons(data = SFAB_WGS84_boundary, group = "AirBasin", fill = FALSE, weight = 1.5, color = gray(0.7)) %>%
    addPolygons(data = CARE_plus_boundaries, group = "CARE+", fillColor = CARE_color, color = scales::muted(CARE_color), opacity = 0.25, weight = 2)

  if (is.null(data)) {
    # If no data, use this default view. (Otherwise, it's assumed that the data
    # will determine the zoom and the bounds.)
    msg("setting view to SF Bay Area")
    leaflet_map <- setViewBayArea(leaflet_map, lng = lng, lat = lat, zoom = zoom, verbose = verbose)
  } else if (is.null(lng) && is.null(lat)) {
    bb <- unname(st_bbox(data))
    leaflet_map <- fitBounds(leaflet_map, lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
  }

  if (is.null(tileset)) {
    base_groups <- names(tileset_providers)
  } else {
    base_groups <- strtools::str_relevel(names(tileset_providers), tileset)
  }

  if (isTRUE(layer_controls)) {

    msg("adding layer controls: base_groups = ", str_csv(base_groups))

    leaflet_map <-
      leaflet_map %>%
      addLayersControl(baseGroups = base_groups,
                       overlayGroups = c("AirBasin", "CARE+"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroups("CARE+")

  }

  return(leaflet_map)

}
