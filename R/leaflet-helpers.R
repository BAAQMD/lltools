# TILESET_PROVIDERS <- list(
#   "Carto" = "CartoDB.PositronNoLabels",
#   "CartoLabeled" = "CartoDB.Positron",
#   "GrayCanvas" = "Esri.WorldGrayCanvas",
#   "TonerLite" = "Stamen.TonerLite",
#   "Terrain" = "Stamen.TerrainBackground",
#   "Satellite" = "Esri.WorldImagery")

noZoomOption <-
  leaflet::leafletOptions(zoomControl = FALSE)

################################################################################

screenshot_map <- function (leaflet_map, file, w, h, z = 1, ..., verbose = getOption("verbose")) {
  msg <- function (...) if(isTRUE(verbose)) message("[screenshot_map] ", ...)
  drop_ext <- function (x) tools::file_path_sans_ext(x)
  ext <- tools::file_ext(file)
  fn <- glue::glue("{drop_ext(file)}-{w}x{h}.{ext}")
  msg(glue::glue("saving to {fn}"))
  mapshot(leaflet_map, file = fn, vwidth = w, vheight = h, zoom = z, ...)
}






