#' @export
addRasterLayer <- function (
  map_object,
  raster_layer,
  group = "Raster",
  pane = "Raster",
  opacity = 1.0,
  limits = NULL,
  quantiles = NULL,
  breaks = NULL,
  cut = FALSE,
  palette = NULL,
  legend = TRUE,
  legendOptions = list(title = NULL, digits = 2, size = 16, opacity = 1.0, position = "bottomright"),
  n_colors = NULL,
  crs = 3857,
  options = gridOptions(zIndex = 1),
  visible = TRUE,
  queryOptions = list(digits = NULL, css = NULL, position = "bottomright"),
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addRasterLayer] ", ...)

  if (is.null(group)) {
    group <- first(names(raster_layer))
    msg("setting `group` to: ", group)
  }

  if (is.null(pane)) {
    pane <- group
    msg("setting `pane` to: ", pane)
  }

  map_object <-
    map_object %>%
    addMapPane(
      pane,
      zIndex = options$zIndex)

  if (isFALSE(is.null(crs))) {
    msg("warping to: ", crs)
    if (is.numeric(crs)) crs <- st_crs(crs)
    if (isFALSE(is.character(crs))) crs <- crs$proj4string
    raster_layer <- raster::projectRaster(raster_layer, crs = crs)
  } else {
    msg("crs is NULL; not reprojecting")
  }

  if (is.null(palette)) {
    # Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"),
    # "viridis" (or "D", the default option) and "cividis" (or "E").
    palette <-
      ggtools::scale2palette(
        viridis::scale_fill_viridis(option = "A"))
  }

  # Make a copy so that un-clamped values can be queried even
  # if we clamp the rendered values to `limits`
  query_raster <-
    raster_layer

  if (isTRUE(is.null(limits))) {

    if (isTRUE(is.null(raster_layer))) {
      stop("must supply either (a) `value_limits` or (b) `raster_layer`")
    }

    map_values <- raster::values(raster_layer)
    max_value <- signif(max(map_values, na.rm = TRUE), digits = 2)
    msg("max_value autodetected: ", max_value)
    max_label <- max_value

  } else {

    max_value <- max(limits)
    msg("max_value specified: ", max_value)
    max_label <- str_c(max_value, "+")  # tack on a "+", meaning "and over"

    raster_layer <-
      geotools:::clip_and_trim(
        raster_layer,
        lower = min(limits),
        upper = max(limits))

  }

  format_breaks <- function (x) {
    str_trim(format(x, digits = legendOptions$digits %||% 2))
  }

  if (isTRUE(cut)) {

    print(summary(raster::values(raster_layer)))
    msg("cutting at: ", str_csv(format_breaks(breaks)))

    raster_layer <-
      raster_layer %>%
      cut_raster(
        breaks = breaks,
        include.lowest = TRUE,
        right = FALSE)

    legend_labels <- with(
      raster::levels(raster_layer)[[1]],
      glue::glue("{label} [{format_breaks(lower)}-{format_breaks(upper)})"))

    n_colors <- length(legend_labels)
    raster_colors <- palette(n = n_colors)[1:n_colors]
    legend_colors <- raster_colors
    msg("raster_colors is: ", str_csv(raster_colors))

  } else {

    if (is.null(breaks)) {
      breaks <- pretty(c(0, max_value), n = 5)
    }

    if (isTRUE(is.null(n_colors))) {
      n_colors <- length(breaks)
      msg("setting n_colors to: ", n_colors)
    }

    raster_colors <- palette(n = n_colors)[1:n_colors]
    legend_labels <- format_breaks(breaks)
    legend_labels[length(legend_labels)] <- max_label
    legend_colors <- palette(length(breaks))

  }

  if (isTRUE(legend)) {

    legend_size <- legendOptions$size %||% 16
    legend_title <- legendOptions$title # OK to be NULL
    legend_opacity <- legendOptions$opacity %||% 1.0
    legend_position <- legendOptions$position %||% "bottomright"

    map_object <-
      map_object %>%
      addLegend(
        position = legend_position,
        opacity = 1.0,
        group = group,
        className = "info legend legend0",
        title = glue::glue(legend_title),
        labels = rev(legend_labels),
        colors = rev(legend_colors),
        values = rev(breaks)) %>%
      style_widget(
        glue::glue("font-size:{legend_size}px; line-height:{1.25*legend_size}px;"),
        "div.legend") %>%
      style_widget(
        glue::glue("width:{legend_size}px; height:{1.00*legend_size}px"),
        "div.legend i") %>%
      style_widget(
        glue::glue("opacity:{100*legend_opacity}%;background-color:white"),
        "div.legend") %>%
      style_widget(
        glue::glue("margin-bottom:36px;"),
        "div.legend0")
  }

  options <-
    rlang::exec(
      gridOptions,
      !!!options,
      pane = pane)

  msg("invoking addRasterLayer")
  map_object <-
    map_object %>%
    addRasterImage(
      raster_layer,
      group = group,
      #project = isTRUE(is.null(crs)),
      colors = legend_colors,
      opacity = opacity,
      options = options,
      ...)

  map_object <-
    map_object %>%
    addOverlayGroup(
      group = group,
      visible = visible)

  if (isFALSE(is.null(queryOptions$digits))) {

    msg("adding image query")

    if (isTRUE(is.null(queryOptions$css))) {
      queryOptions$css <- glue::glue("font-size:{legend_size*1.25}px !important")
    }

    if (isTRUE(is.null(queryOptions$position))) {
      queryOptions$position <- (legendOptions$position %||% "bottomright")
    }

    map_object <-
      map_object %>%
      leafem::addImageQuery(
        query_raster,
        position = queryOptions$position,
        type = "mousemove",
        digits = queryOptions$digits,
        prefix = "",
        className = "imageQuery",
        project = TRUE, #project = isTRUE(is.null(crs)),
        group = group) %>%
      style_widget(
        queryOptions$css,
        ".imageQuery")

  }

  return(map_object)

}
