require(tidyverse)
require(sf)

test_data <-
  tibble(
    year =
      "RY2015",
    fac_id = 10L,
    cnty_abbr = "CC",
    geometry = structure(
      list(structure(
        c(553369.995117188, 4199029.78515625),
        class = c("XY", "POINT", "sfg")
      )),
      class = c("sfc_POINT", "sfc"),
      precision = 0,
      bbox = structure(
        c(
          xmin = 553369.995117188,
          ymin = 4199029.78515625,
          xmax = 553369.995117188,
          ymax = 4199029.78515625
        ),
        class = "bbox"
      ),
      crs = structure(
        list(epsg = 26910L, proj4string = "+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"),
        class = "crs"
      ),
      n_empty = 0L
    ),
    fac_name = "Chevron Products Company",
    fac_address = "841 Chevron Way Richmond CA 94802"
  )

test_geodata <-
  test_data %>%
  st_as_sf() %>%
  mutate(
    ems_qty = 55)

test_that("size var is character", {

  leaflet_map_facilities(
    test_geodata,
    size = "ems_qty",
    verbose = TRUE)


})
