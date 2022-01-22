#' Smoothed conditional means
#'
#' This is a modified version of geom_smooth() from ggplot2.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "smooth")
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param geom,stat Use to override the default connection between
#'   `geom_smooth()` and `stat_smooth()`.
#' @seealso See individual modelling functions for more details:
#'   [lm()] for linear smooths,
#'   [glm()] for generalised linear smooths, and
#'   [loess()] for local smooths.
#' @export
smooth2 <- function(mapping = NULL, data = NULL,
                        stat = "smooth", position = "identity",
                        ...,
                        method = NULL,
                        formula = NULL,
                        se = TRUE,
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    orientation = orientation,
    se = se,
    ...
  )
  if (identical(stat, "smooth")) {
    params$method <- method
    params$formula <- formula
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSmooth,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSmooth <- ggproto("GeomSmooth", Geom,
                      setup_params = function(data, params) {
                        params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE, ambiguous = TRUE)
                        params
                      },

                      extra_params = c("na.rm", "orientation"),

                      setup_data = function(data, params) {
                        GeomLine$setup_data(data, params)
                      },

                      # The `se` argument is set to false here to make sure drawing the
                      # geom and drawing the legend is in synch. If the geom is used by a
                      # stat that doesn't set the `se` argument then `se` will be missing
                      # and the legend key won't be drawn. With `se = FALSE` here the
                      # ribbon won't be drawn either in that case, keeping the overall
                      # behavior predictable and sensible. The user will realize that they
                      # need to set `se = TRUE` to obtain the ribbon and the legend key.
                      draw_group = function(data, panel_params, coord, lineend = "butt", linejoin = "round",
                                            linemitre = 10, se = FALSE, flipped_aes = FALSE) {
                        ribbon <- transform(data, colour = NA)
                        path <- transform(data, alpha = NA)

                        ymin = flipped_names(flipped_aes)$ymin
                        ymax = flipped_names(flipped_aes)$ymax
                        has_ribbon <- se && !is.null(data[[ymax]]) && !is.null(data[[ymin]])

                        gList(
                          if (has_ribbon) GeomRibbon$draw_group(ribbon, panel_params, coord, flipped_aes = flipped_aes),
                          GeomLine$draw_panel(path, panel_params, coord, lineend = lineend, linejoin = linejoin, linemitre = linemitre)
                        )
                      },

                      draw_key = draw_key_smooth,

                      required_aes = c("x", "y"),
                      optional_aes = c("ymin", "ymax"),

                      default_aes = aes(colour = "#0f993d", fill = "grey60", size = 1,
                                        linetype = 1, weight = 1, alpha = 0.4)
)
