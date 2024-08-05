wiego_color = function(...) {

  wiego_colors = c("orange" = "#FF671F",
                   "green" = "#8E8C13",
                   "brown" = "#8B5B29",
                   "dark khaki" = "#5E514D",
                   "warm gray" = "#796E65",
                   "nude khaki" = "#EFDBB2")

  cols <- c(...)

  if (is.null(cols))
    return (wiego_colors)

  wiego_colors[cols]

}



wiego_palette <- function(palette = "main", ...) {

  wiego_palettes <- list(
    `main` = wiego_color("orange", "green", "brown", "dark khaki",
                         "warm gray", "nude khaki"),
    `highlight` = wiego_color("orange", "green")
  )

  wiego_palettes[[palette]]

}


wiego_palette_gen <- function(palette = "main", direction = 1) {

  function(n) {

    if (n > length(wiego_palette(palette)))
      warning("Not enough colors in this palette!")

    else {

      all_colors <- wiego_palette(palette)

      all_colors <- unname(unlist(all_colors))

      all_colors <- if (direction >= 0) all_colors else rev(all_colors)

      color_list <- all_colors[1:n]

    }
  }
}

scale_fill_wiego <- function(palette = "main", direction = 1, ...) {

  ggplot2::discrete_scale(
    "fill", "wiego",
    wiego_palette_gen(palette, direction),
    ...
  )
}

scale_colour_wiego <- function(palette = "main", direction = 1, ...) {

  ggplot2::discrete_scale(
    "wiego_palette_gen", "wiego",
    palette_gen(palette, direction),
    ...
  )
}

scale_color_wiego = scale_colour_wiego
