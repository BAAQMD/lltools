#' Map facility-specific locations (as pins), or quantities (as circles)
#'
#' @param facility_geodata (geodata) for example: `point_source_facilities(RY(2015))`
#' @param size (expression) if supplied, controls size of circles (area, not radius, will be proportional to this)
#' @param color (expression) if supplied, controls colors of circles
#' @param max_size (numeric) helpful for tweaking the relative sizes of circles
#' @param stroke (logical) see [leaflet::addCircleMarkers()]
#' @param ... further arguments to [leaflet_SFBA()]
#' @param verbose (logical)
#'
#' @seealso [leaflet_map_SFBA()]
#'
#' @export
leaflet_map_facilities <- function (
  facility_geodata,
  size,
  color = "orange",
  opacity = 0.333,
  popup = NULL,
  max_size = 100,
  stroke = FALSE,
  popupOptions = leaflet::popupOptions(),
  ...,
  verbose = getOption("verbose")
) {

  require(lazyeval)

  msg <- function (...) if(isTRUE(verbose)) message("[leaflet_map_facilities] ", ...)

  map_object <-
    leaflet_map_SFBA(
      facility_geodata,
      ...,
      verbose = verbose)

  #
  # Try a reasonable "default" formula for popup HTML
  #
  if (is.null(popup)) {

    popup_vars <- c("fac_id", "fac_name", "fac_address")

    if (all(popup_vars %in% names(facility_geodata))) {
      popup <- ~ str_c(
        str_c("<b>P#", fac_id, "</b>"),
        fac_name,
        fac_address,
        sep = "</br>")
    }

  }

  msg("popup formula is: ", popup)

  size_var <-
    rlang::enquo(size)

  msg("size_var is: ", as.character(size_var))

  #
  # Use non-standard evaluation to compute `sizes`
  #
  if (FALSE) {

    #
    # If no `size` provided: just add Markers
    #
    msg("adding Markers")
    addMarkers(map_object, popup = popup)

  } else {

    circle_sizes <-
      facility_geodata %>%
      drop_geometry() %>%
      mutate(
        .size = !!size_var) %>%
      pull(
        .size)

    circle_radii <-
      circle_sizes %>%
      scales::rescale_max() %>%
      sqrt() %>%
      { . * max_size }

    #
    # Add CircleMarkers, with *area* (not radius!) proportional to `size`
    #

    msg("adding CircleMarkers")
    addCircleMarkers(
      map_object,
      radius = circle_radii,
      fillColor = color,
      fillOpacity = opacity,
      stroke = stroke,
      color = color,
      opacity = opacity,
      popup = popup,
      popupOptions = popupOptions)

  }

}
