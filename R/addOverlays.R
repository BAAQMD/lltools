#' addOverlays
#'
#' Helper for adding multiple overlays to a leaflet map at the same time, using grouped data.
#' Group names will be passed to `fun`, which should be a function that actually adds an overlay, e.g. [addPolygonOverlay()].
#'
#' @param map_object leaflet map
#' @param data grouped data
#' @param fun function to add a single overlay, taking an argument `group`
#' @param ... passed to `fun`
#' @param verbose logical
#'
#' @importFrom dplyr group_split groups pull
#' @importFrom purrr reduce2
#'
#' @return leaflet map, with overlays added
#' @export
#'
addOverlays <- function (
  map_object,
  data,
  fun,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[addOverlays] ", ...)

  data_list <- dplyr::group_split(data)
  grouping_vars <- as.character(dplyr::groups(data))
  group_names <- as.character(unlist(dplyr::group_keys(data))) # WAS: unique(dplyr::pull(data, grouping_vars))
  names(data_list) <- group_names

  f <- function (map_object, data, group) {
    map_object <- fun(map_object, data = data, group = group, ..., verbose = verbose)
    return(map_object)
  }

  i <- dplyr::pull(data, grouping_vars)

  map_object <-
    purrr::reduce2(
      data_list[i],
      i,
      f,
      .init = map_object)

  return(map_object)

}
