#' addGlPolygonOverlay
#'
#' @param map_object leaflet map object
#' @param data sf object
#' @param color formula, like `~ pmin(1.0, value)`; can reference things in `data`
#' @param palette like "inferno"; passed to [colourvalues::colour_values_rgb()]
#' @param popup formula, like `~ GEOID10`; defaults to using [leafpop::popupTable()]
#' @param group group name
#' @param crs probably shouldn't change
#' @param visible show when map loads?
#' @param verbose logical
#'
#' @importFrom sf st_cast st_drop_geometry st_transform st_geometry
#' @importFrom colourvalues colour_values_rgb
#' @importFrom funtools eval_f
#' @importFrom leafgl addGlPolygons
#' @importFrom leafpop popupTable
#'
#' @return modified `map_object`
#' @export
#'
addGlPolygonOverlay <- function (
  map_object,
  data,
  color = NULL,
  stroke = NULL,
  palette = "inferno",
  popup = NULL,
  group = "Polygons",
  crs = WGS84_GPS,
  visible = TRUE,
  ...,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addGlPolygonOverlay] ", ...)

  map_polygons <-
    st_cast(
      st_cast(
        data,
        "MULTIPOLYGON"),
      "POLYGON")

  map_polygon_data <-
    st_drop_geometry(
      map_polygons)

  if (is.null(color)) {
    color <- cbind(0, 0.2, 1) * 255
  } else if (is.character(color)) {
    msg("taking color as-is: ", str_csv(head(color, 3), "..."))
    color <- color
  } else if (is_formula(color)) {
    msg("evaluating colors: ", color)
    color <- funtools::eval_f(color, data = map_polygon_data)
    if (is.numeric(color)) {
      msg("converting color to RGB matrix")
      color <- colourvalues::colour_values_rgb(
        color,
        palette = palette,
        include_alpha = FALSE) / 255
    }
  } else {
    err_msg <- "Sorry, I don't know how to interpret that expression for `color` as a basis for colors."
    stop(err_msg)
  }

  if (is.null(popup)) {
    popup <- NULL # pass
  } else if (isTRUE(popup)) {
    msg("rendering popups: leafpop::popupTable()")
    popup <-
      leafpop::popupTable(
        map_polygon_data,
        row.numbers = FALSE)
  } else if (rlang::is_formula(popup)) {
    msg("rendering popups: ", popup)
    popup <-
      funtools::eval_f(
        popup,
        data = map_polygon_data)
  } else {
    err_msg <- "Sorry, I don't know how to interpret that expression for `popup` as a basis for popups."
    stop(err_msg)
  }

  transformed_polygons <-
    st_transform(
      st_geometry(map_polygons),
      crs)

  map_object <-
    leafgl::addGlPolygons(
      map_object,
      color = color,
      popup = popup,
      data = transformed_polygons,
      ...)

  if (isFALSE(is.null(stroke))) {

    polylines <-
      transformed_polygons %>%
      st_geometry() %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING")

    map_object <-
      leafgl::addGlPolylines(
        map_object,
        color = stroke,
        data = polylines,
        ...)

  }

  if (group %not_in% names(getOverlayGroups(map_object))) {
    map_object <- addOverlayGroup(map_object, group, visible = visible)
  }

  return(map_object)

}
