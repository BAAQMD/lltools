#' @export
addCARERegions <- function (
  map_object,
  color = "#7f7fff",
  opacity = 0.25,
  weight = 2,
  group = "CARE+",
  ...
) {

  msg("adding ", group)

  CARE_plus_boundaries <-
    st_union(
      CARE::CARE_region_geodata) %>%
    st_transform(
      WGS84_GPS)

  map_object <-
    map_object %>%
    addPolygonOverlay(
      data = CARE_plus_boundaries,
      group = group,
      fillColor = color,
      color = scales::muted(color),
      opacity = opacity,
      weight = weight,
      ...)

  return(map_object)

}
