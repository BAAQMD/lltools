#' @export
addSFABRegion <- function (
  map_object,
  color = gray(0.7),
  weight = 1.5,
  group = "AirBasin",
  visible = TRUE,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addSFABRegion] ", ...)

  msg("adding ", group)

  map_object <-
    map_object %>%
    addPolygonOverlay(
      data = SFAB_WGS84_boundary,
      group = group,
      fill = FALSE,
      color = color,
      weight = weight)

  return(map_object)

}
