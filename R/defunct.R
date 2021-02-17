screenshot_map <- function (leaflet_map, file, w, h, z = 1, ..., verbose = getOption("verbose")) {

  .Defunct("write_leaflet")

  msg <- function (...) if(isTRUE(verbose)) message("[screenshot_map] ", ...)
  drop_ext <- function (x) tools::file_path_sans_ext(x)
  ext <- tools::file_ext(file)
  fn <- glue::glue("{drop_ext(file)}-{w}x{h}.{ext}")
  msg(glue::glue("saving to {fn}"))
  mapshot(leaflet_map, file = fn, vwidth = w, vheight = h, zoom = z, ...)

}
