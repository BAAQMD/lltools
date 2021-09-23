#' addOverlay
#'
#' @param fun leaflet function like [leaflet::addCircleMarkers()]
#' @param map_object leaflet map
#' @param data passed to `fun`
#' @param group passed to `fun`
#' @param layer_id passed to `fun`
#' @param visible `TRUE` or `FALSE` (initial state)
#' @param ... passed to `fun`
#' @param verbose display messages
#'
#' @return
#' @export
addOverlay <- function (
  fun,
  map_object,
  data,
  group,
  color = "black",
  label = NULL,
  layer_id = NULL,
  visible = TRUE,
  weight = 1.0,
  opacity = 1.0,
  ...,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addOverlay] ", ...)
  data <- st_transform(data, geotools::WGS84_GPS)

  CMYK_COLORS <- list(
    cyan = "#00ffff",
    magenta = "#ff00ff",
    yellow = "#ffff00",
    black = "#000000",
    purple = "#7f7fff")

  if (isFALSE(is.null(names(data)))) {
    label <- rlang::eval_tidy(label, data = data)
  }

  arg_list <- list(
    map = map_object,
    data = data,
    group = group,
    layerId = layer_id,
    weight = weight,
    opacity = opacity,
    label = label,
    ...)

  if ("color" %in% names(rlang::fn_fmls(fun))) {
    if (is.null(color)) {
      color <- NA
    } else if (is.character(color)) {
      if (isTRUE(all(color %in% names(CMYK_COLORS)))) {
        msg("interpreting color as CMYK name(s)")
        color <- CMYK_COLORS[color]
      } else {
        msg("taking color as-is: ", str_csv(head(color, 3), "..."))
        color <- color
      }
    } else if (rlang::is_formula(color)) {
      msg("evaluating colors: ", color)
      color <- funtools::eval_f(color, data = data)
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
    arg_list <- append(arg_list, list(color = color))
  } else {
    if (!is.null(color)) {
      msg("warning: color is not recognized by ", deparse(substitute(fun)))
    }
  }

  recognized_args <-
    arg_list[intersect(names(arg_list), names(rlang::fn_fmls(fun)))]

  #msg("names(rlang::fn_fmls(fun)) is: ", str_csv(names(rlang::fn_fmls(fun))))
  #msg("recognized_args is: ", names(recognized_args))

  map_object <-
    do.call(fun, recognized_args)

  if (group %not_in% names(getOverlayGroups(map_object))) {
    map_object <- addOverlayGroup(map_object, group, visible = visible)
  }

  return(map_object)

}
