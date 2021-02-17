#' addTitle
#'
#' @param map_object leaflet map
#' @param title character
#' @param size character
#' @param position character
#'
#' @return modified leaflet map
#' @export
addTitle <- function (
  map_object,
  title,
  group = "Title",
  opacity = 1.0,
  size = "16px",
  align = "center",
  style = list(
    `opacity` = "{opacity}",
    `font-size` = "{size}",
    `text-align` = "{align}",
    `background-color` = "white",
    `position` = "fixed !important",
    `bottom` = "0%",
    `left` = "0%",
    `width` = "100%",
    `padding` = "0",
    `margin` = "0",
    `padding-top` = "6px",
    `padding-bottom` = "6px",
    # `padding-left` = "150px",
    # `padding-right` = "150px",
    `font-weight` = "normal"),
  position = "topleft",
  ...
) {

  require(htmlwidgets)
  require(htmltools)

  title <-
    glue::glue(title)

  style <-
    update_list(style, ...)

  style <-
    glue::glue(
      ".leaflet-control.map-title {{ ",
      rlang::exec(htmltools::css, !!!style),
      "}}")

  tag.map.title <-
    htmltools::tags$style(
      HTML(style))

  map_object <-
    map_object %>%
    leaflet::addControl(
      htmltools::tags$div(
        tag.map.title,
        HTML(title)),
      position = position,
      className = "map-title")

  return(map_object)

}
