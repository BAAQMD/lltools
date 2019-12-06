#' Write a leaflet map to disk
#'
#' @param leaflet_map (object) created with [leaflet::leaflet()], [leaflet_SFBA()], etc.
#' @param file (character) ending in `.html` (but see Details)
#' @param ... further arguments to [mapview::mapshot()]
#' @param remove_url (logical) passed to [mapview::mapshot()] if `file` doesn't end with `.html`
#' @param verbose (logical)
#'
#' @details
#' This function relies on [mapview::mapshot()].
#' If `file` ends with an extensions other than `.html` ---
#' for example, `.png` --- then `mapshot()` will try to handle it.
#' But you may need to pass other arguments, like `width` and `height`.
#' Also, `remove_url` is `TRUE` by default (but you can change that).
#'
#' @export
write_leaflet <- function (leaflet_map, file = NULL, ..., remove_url = TRUE, verbose = getOption("verbose")) {
  msg <- function (...) if(isTRUE(verbose)) message("[write_leaflet] ", ...)
  msg("saving to ", file)

  # Remeber to restore working directory
  old_wd <- setwd(dirname(file))
  on.exit(setwd(old_wd))

  if (str_to_lower(tools::file_ext(file)) == "html") {
    mapshot(leaflet_map, url = basename(file), ...)
  } else {
    mapshot(leaflet_map, file = basename(file), remove_url = remove_url, ...)
  }

}

#' @export
write_map <- function (map, ...) {
  warning("[write_map] write_map() is experimental and subject to change")
  write_leaflet_help
}
