palette_impact <- function (pollutant) {

  scale_obj <- scale_color_impact(
    pollutant = pollutant,
    limits = c(0, upper_concentration_for(pollutant)))

  f <- function (x) {
    rescaled <- scale_obj$rescale(x)
    clamped <- scales::oob_squish(rescaled, range = c(0, 1))
    colors <- scale_obj$palette(clamped)
    return(colors)
  }

  return(f)

}
