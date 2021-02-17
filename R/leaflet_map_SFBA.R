#' Leaflet map of the SF Bay Area
#'
#' @param ... arguments to [leaflet::leaflet()]
#' @param lng (numeric) longitude, in decimal degrees, center of map
#' @param lat (numeric) latitude, in decimal degrees, for center of map
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
  tileset_providers = list(
    "Light"       = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
    "Dark"        = "https://cartodb-basemaps-a.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png",
    "Unlabeled"   = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_nolabels/{z}/{x}/{y}.png",
    #"World"       = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{x}/{y}",
    "Satellite"   = "http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}",
    "Terrain"     = "http://mt0.google.com/vt/lyrs=p&hl=en&x={x}&y={y}&z={z}",
    "Topo"        = "https://a.tile.opentopomap.org/{z}/{x}/{y}.png",
    "Roads"       = "http://mt0.google.com/vt/lyrs=r&hl=en&x={x}&y={y}&z={z}",
    "Census"      = "http://demographics.virginia.edu/DotMap/tiles4/{z}/{x}/{y}.png",
    "Toner"       = "http://stamen-tiles-a.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png",
    "Blank"       = ""),
  ...,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  max_zoom = NULL,
  options = NULL,
  layer_visibility = list("SFAB" = TRUE),
  layer_controls = TRUE,
  attribution = NULL,
  background = "white",
  crs = 4326,
  verbose = getOption("verbose")
) {

  require(strtools)

  msg <- function (...) if(isTRUE(verbose)) message("[leaflet_map_SFBA] ", ...)

  if (missing(data)) {
    data <- NULL
  } else {
    if (inherits(data, "sf")) {
      if (!isTRUE(st_is_longlat(data))) {
        msg("reprojecting to EPSG 4326 (GPS, WGS84 lng/lat)")
        data <- st_transform(data, crs)
      }
    }
  }

  if (is.null(options)) {
    options <- leafletOptions(
      minZoom = 8,
      maxZoom = max_zoom)
  }

  map_object <-
    leaflet(
      data = data,
      options = options,
      ...) %>%
    leafem::addMouseCoordinates()

  if (missing(tileset)) {
    tileset <- first(names(tileset_providers))
    msg("`tileset` not provided; defaulting to '", tileset, "'")
  }

  if (isFALSE(tileset)) {

    # pass

  } else {

    for (nm in names(tileset_providers)) {
      map_object <-
        map_object %>%
        addTileset(
          tileset_providers[[nm]],
          group = nm,
          attribution = attribution,
          verbose = verbose)
    }

    if (tileset %not_in% names(tileset_providers)) {

      stop_msg <- glue::glue(
        "'{tileset}' isn't a known tileset provider. ",
        "But, you can pass in your own with `tileset`. ",
        "See ?leaflet_map_SFBA() for more.")
      stop(stop_msg)

    }

    attr(map_object, "tileset") <-
      tileset

  }


  #
  # Change the default background from gray to white.
  # This helps the appearance when the baselayer (tileset) is sem-transparent.
  #
  map_object <-
    map_object %>%
    leaflet.extras::setMapWidgetStyle(
      list(background = background))

  if (inherits(data, "sf")) {
    if (!isTRUE(st_is_longlat(data))) {
      msg("reprojecting to EPSG 4326 (GPS, WGS84 lng/lat)")
      data <- st_transform(data, crs)
    }
  }

  #
  # Optional: Add the air basin as a layer. (Default TRUE.)
  #
  if ("SFAB" %in% names(layer_visibility)) {
    map_object <-
      map_object %>%
      addSFABRegion(
        visible = layer_visibility[["SFAB"]],
        verbose = verbose)
  }

  #
  # Add CARE+ regions as a layer. (Default FALSE.)
  #
  if ("CARE+" %in% names(layer_visibility)) {
    map_object <-
      map_object %>%
      addCARERegions(
        visible = layer_visibility[["CARE+"]],
        verbose = verbose)
  }

  #
  # Re-style the attribution text (and links) at the bottom of the map.
  #
  map_object <-
    map_object  %>%
    style_widget(
      "color:gray;",
      "a")

  if (is.null(data)) {
    # If no data, use this default view. (Otherwise, it's assumed that the data
    # will determine the zoom and the bounds.)
    msg("setting view to SF Bay Area")
    map_object <- setViewBayArea(map_object, lng = lng, lat = lat, zoom = zoom, verbose = verbose)
  } else if (is.null(lng) && is.null(lat)) {
    bb <- unname(st_bbox(data))
    map_object <- fitBounds(map_object, lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
  }

  if (isTRUE(layer_controls)) {

    msg("names(base_groups) is: ", str_csv(names(getBaseGroups(map_object))))

    map_object <-
      map_object %>%
      addLayersControl(
        baseGroups = names(getBaseGroups(map_object)),
        overlayGroups = names(getOverlayGroups(map_object)),
        options = layersControlOptions(
          collapsed = TRUE)) %>%
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions())  %>%
      showGroup(
        tileset)

  }

  return(map_object)

}
