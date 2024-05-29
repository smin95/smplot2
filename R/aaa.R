#' This script contains dependencies from gghalves and sdamr packages.
#' All licenses & ownership of the codes here belong to the developers of the packages, not me.
#'
#' Tiedemann F (2022). gghalves: Compose Half-Half Plots Using
#' Your Favourite Geoms_. R package version 0.1.4,
#' <https://CRAN.R-project.org/package=gghalves>.
#'
#' Speekenbrink M (2022). sdamr: Statistics: Data Analysis and
#' Modelling_. R package version 0.2.0,
#' <https://CRAN.R-project.org/package=sdamr>.
#'
#' gghalves: https://github.com/erocoar/gghalves
#' sdamr: https://github.com/mspeekenbrink/sdam-r/
#'
#' Codes have been directly brought here to reduce the potential dependencies of the smplot2 package.
#' -------------------------------------------------------------------------------------------
#' Half Violin plot
#'
#'
#' @inheritParams ggplot2::geom_violin
#' @param side The side on which to draw the half violin plot. "l" for left, "r" for right, defaults to "l".
#' @param nudge Add space between the violinplot and the middle of the space allotted to a given factor on the x-axis.
#' @importFrom ggplot2 layer
#' @keywords internal
#' @noRd
geom_half_violin <- function(
    mapping = NULL, data = NULL,
    stat = "half_ydensity", position = "dodge",
    ...,
    side = "l",
    nudge = 0,
    draw_quantiles = NULL,
    trim = TRUE,
    scale = "area",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      side = side,
      nudge = nudge,
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      ...
    )
  )
}

# -------------------------------------------------------------------------


#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto GeomViolin GeomBoxplot GeomPolygon
#' @keywords internal
#' @noRd
GeomHalfViolin <- ggproto(
  "GeomHalfViolin", GeomViolin,

  default_aes = ggplot2:::modify_list(aes(split = NA), GeomViolin$default_aes),

  setup_data = function(data, params) {
    x_data    <- GeomBoxplot$setup_data(data, NULL)
    data$xmin <- x_data$xmin
    data$xmax <- x_data$xmax
    data
  },

  setup_params = function(data, params) {
    if ("split" %in% colnames(data)) {
      stopifnot(length(unique(data$split)) == 2)
      params$side <- rep(c("l", "r"), max(data$group) / 2)
    } else {
      params$side <- rep(params$side,
                         ceiling(length(unique(data$group))/length(params$side)))
    }
    params
  },

  draw_group = function(self, data, side = "l", nudge = 0, ..., draw_quantiles = NULL) {
    # Find the points for the line to go all the way around
    is_panel <- data$group[1] == -1
    is_group <- FALSE
    if (isFALSE(is_panel)) {
      is_group <- side[data$group[1]] == 'l'
    }

    if ((is_panel & (side[1] == "l")) | is_group) {
      data <- transform(
        data,
        xminv = x + violinwidth * (xmin - x) - nudge,
        xmaxv = x - nudge
      )
    } else {
      data <- transform(
        data,
        xminv = x + nudge,
        xmaxv = x + violinwidth * (xmax - x) + nudge
      )
    }

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      transform(data, x = xminv)[order(data$y), ],
      transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])

    # Draw quantiles if requested, so long as there is non-zero y range
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))

      # Compute the quantile segments and combine with existing aesthetics
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[
        rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y", "group")),
        drop = FALSE
      ]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      both <- both[!is.na(both$group), , drop = FALSE]
      quantile_grob <- if (nrow(both) == 0) {
        zeroGrob()
      } else {
        GeomPath$draw_panel(both, ...)
      }

      ggplot2:::ggname("geom_half_violin", grobTree(
        GeomPolygon$draw_panel(newdata, ...),
        quantile_grob)
      )
    } else {
      ggplot2:::ggname("geom_half_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)


# -------------------------------------------------------------------------
#' Directly from yjr gghalves package
#' All licenses & ownership of the codes here belong to the developers of the packages, not me.
#' gghalves: https://github.com/erocoar/gghalves
#'
#' @inheritParams ggplot2::geom_boxplot
#' @param errorbar.draw Draw horizontal whiskers at the top and bottom (the IQR). Defaults to `TRUE`.
#' @param errorbar.length Length of the horizontal whiskers (errorbar). Defaults to half the width of the half-boxplot
#' @param side The side of the half-geom, "l" for left and "r" for right, defaults to "l".
#' @param center Boolean whether to center the half-boxplot instead of aligning it to its respective side.
#' @param nudge Add space between the boxplot and the middle of the space allotted to a given factor on the x-axis.
#' @importFrom ggplot2 layer position_dodge2 aes GeomSegment GeomCrossbar
#' @importFrom grid grobTree grobName
#' @noRd
#' @keywords internal
geom_half_boxplot <- function(
    mapping = NULL, data = NULL,
    stat = "boxplot", position = "dodge2",
    ...,
    side = "l",
    center = FALSE,
    nudge = 0,
    outlier.colour = NULL,
    outlier.color = NULL,
    outlier.fill = NULL,
    outlier.shape = 19,
    outlier.size = 1.5,
    outlier.stroke = 0.5,
    outlier.alpha = NULL,
    notch = FALSE,
    notchwidth = 0.5,
    varwidth = FALSE,
    errorbar.draw = TRUE,
    errorbar.length = 0.5,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {

  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", call. = FALSE)
      position$preserve <- "single"
    }
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfBoxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      side = side,
      center = center,
      nudge = nudge,
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      errorbar.draw = errorbar.draw,
      errorbar.length = errorbar.length,
      na.rm = na.rm,
      ...
    )
  )
}


#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 alpha ggproto GeomBoxplot aes GeomSegment GeomPoint GeomCrossbar resolution PositionJitter
#' @importFrom grid grobTree
#' @keywords internal
#' @noRd
GeomHalfBoxplot <- ggproto("GeomHalfBoxplot", GeomBoxplot,
                           setup_data = function(data, params) {
                             GeomBoxplot$setup_data(data, params)
                           },

                           draw_group = function(
    data, panel_params, coord, lineend = "butt", linejoin = "mitre",
    fatten = 2, side = "l", center = FALSE, nudge = nudge,
    outlier.colour = NULL, outlier.fill = NULL,
    outlier.shape = 19, outlier.size = 1.5,
    outlier.stroke = 0.5, outlier.alpha = NULL,
    notch = FALSE, notchwidth = 0.5,
    varwidth = FALSE, errorbar.draw = FALSE, errorbar.length = 0.5) {

                             if (nrow(data) != 1) {
                               stop(
                                 "Can't draw more than one boxplot per group. Did you forget aes(group = ...)?",
                                 call. = FALSE
                               )
                             }

                             xrange <- data$xmax - data$xmin

                             common <- data.frame(
                               colour = data$colour,
                               linewidth = data$linewidth,
                               linetype = data$linetype,
                               fill = alpha(data$fill, data$alpha),
                               group = data$group,
                               stringsAsFactors = FALSE
                             )

                             whiskers <- data.frame(
                               x = data$x,
                               xend = data$x,
                               y = c(data$upper, data$lower),
                               yend = c(data$ymax, data$ymin),
                               alpha = NA,
                               common,
                               stringsAsFactors = FALSE
                             )

                             # Adjust whisker position based on nudge (extra spacing between geom and middle)
                             # If side == right, move whisker to right and vice versa
                             if (side == "r") {
                               whiskers$x <- whiskers$x + nudge
                             } else {
                               whiskers$x <- whiskers$x - nudge
                             }
                             whiskers$xend <- whiskers$x

                             # If boxplot is centered, need to adjust whisker that is otherwise always at x
                             # If boxplot is centered, only half of nudge value is added s.t. it remains centered
                             if (isTRUE(center)) {
                               if (side == "r") {
                                 whiskers$x <- data$x + xrange / 4 + nudge / 2
                               } else {
                                 whiskers$x <- data$x - xrange / 4 - nudge / 2
                               }
                               whiskers$xend <- whiskers$x
                             }

                             if (errorbar.draw) {
                               if (errorbar.length > 1 | errorbar.length < 0) {
                                 stop("Error bar length must be between 0 and 1.")
                               }
                               error_length_add <- xrange / 2 #((data$xmin + xrange / 2) - data$xmin)
                               error_length_add <- error_length_add * (1 - errorbar.length)

                               error_whiskers <- data.frame(
                                 x = if (side == "r") (data$xmin + xrange / 2) + nudge else (data$xmin + xrange / 2) - nudge ,
                                 xend = if (side == "r") data$xmax - error_length_add + nudge else data$xmin + error_length_add - nudge,
                                 y = c(data$ymax, data$ymin),
                                 yend = c(data$ymax, data$ymin),
                                 alpha = NA,
                                 common,
                                 stringsAsFactors = FALSE
                               )

                               if (isTRUE(center)) {
                                 error_whiskers$x <- data$x
                                 if (side == "r") {
                                   error_whiskers$xend <- data$xmax
                                 } else {
                                   error_whiskers$xend <- data$xmin
                                 }
                               }

                               error_grob <- GeomSegment$draw_panel(error_whiskers, panel_params, coord)
                             } else {
                               error_grob <- NULL
                             }

                             box <- data.frame(
                               xmin = if (side == "r") data$xmax else data$xmin,
                               xmax = (data$xmin + xrange / 2) + switch((side == "r") + 1, nudge * -1, nudge),
                               ymin = data$lower,
                               y = data$middle,
                               ymax = data$upper,
                               ynotchlower = ifelse(notch, data$notchlower, NA),
                               ynotchupper = ifelse(notch, data$notchupper, NA),
                               notchwidth = notchwidth,
                               alpha = data$alpha,
                               common,
                               stringsAsFactors = FALSE
                             )

                             if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
                               outliers <- ggplot2:::data_frame0(
                                 y = data$outliers[[1]],
                                 x = data$x[1] + switch((side == "r") + 1, nudge * -1, nudge),
                                 colour = outlier.colour %||% data$colour[1],
                                 fill = outlier.fill %||% data$fill[1],
                                 shape = outlier.shape %||% data$shape[1],
                                 size = outlier.size %||% data$size[1],
                                 stroke = outlier.stroke %||% data$stroke[1],
                                 fill = NA,
                                 alpha = outlier.alpha %||% data$alpha[1],
                                 .size = length(data$outliers[[1]])
                               )

                               if (isTRUE(center)) {
                                 if (side == "r") {
                                   outliers$x <- outliers$x + xrange / 4
                                 } else {
                                   outliers$x <- outliers$x - xrange / 4
                                 }
                               }
                               outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
                             } else {
                               outliers_grob <- NULL
                             }

                             tree <- grobTree(
                               outliers_grob,
                               error_grob,
                               GeomSegment$draw_panel(whiskers, panel_params, coord, lineend = lineend),
                               GeomCrossbar$draw_panel(
                                 box,
                                 fatten = fatten,
                                 panel_params,
                                 coord,
                                 lineend = lineend,
                                 linejoin = linejoin
                               )
                             )
                             tree$name <- grid::grobName(tree, "geom_half_boxplot")
                             tree
                           }
)

# -------------------------------------------------------------------------

#' @inheritParams ggplot2::stat_ydensity
#' @importFrom ggplot2 layer
#' @noRd
#' @keywords internal
stat_half_ydensity <- function(
    mapping = NULL, data = NULL,
    geom = "half_violin", position = "dodge",
    ...,
    bw = "nrd0",
    adjust = 1,
    kernel = "gaussian",
    trim = TRUE,
    scale = "area",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {

  scale <- match.arg(scale, c("area", "count", "width"))

  layer(
    data = data,
    mapping = mapping,
    stat = StatHalfYdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}


#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto StatBoxplot StatYdensity
#' @noRd
#' @keywords internal
StatHalfYdensity <- ggproto(
  "StatHalfYdensity", StatBoxplot,
  required_aes = c("x", "y"),
  non_missing_aes = c("weight", "split"),

  compute_group = function(
    data, scales, width = NULL, bw = "nrd0", adjust = 1,
    kernel = "gaussian", trim = TRUE, na.rm = FALSE) {
    StatYdensity$compute_group(
      data, scales, width = width, bw = bw, adjust = adjust,
      kernel = kernel, trim = trim, na.rm = na.rm)
  },

  compute_panel = function(
    self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
    kernel = "gaussian", trim = TRUE, na.rm = FALSE, scale = "area") {
    StatYdensity$compute_panel(
      data, scales, width = width, bw = bw, adjust = adjust,
      kernel = kernel, trim = trim, na.rm = na.rm, scale = scale)
  }
)


# -------------------------------------------------------------------------
#' Directly from the sdamr package.
#' sdamr: https://github.com/mspeekenbrink/sdam-r/
#'
#' Simultaneously nudge and jitter
#'
#' @family position adjustments
#' @param jitter.width degree of jitter in x direction. Defaults to 40% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param nudge.x the amount to nudge in the x direction.
#' @param nudge.y the amount to nudge in the y direction.
#' @param seed Optional seed for the random jitter
#' @return Positions for data in a \code{ggplot2::ggplot} object, similar to e.g. \code{ggplot2::position_jitter}
#' @seealso [ggplot2::position_jitter()], which is the basis of this function.
#' @keywords internal
#' @noRd
position_jitternudge <- function(jitter.width = NULL, jitter.height = 0,
                                 nudge.x = 0, nudge.y = 0, seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggplot2::ggproto(NULL, PositionJitternudge,
                   jitter.width = jitter.width,
                   jitter.height = jitter.height,
                   nudge.x = nudge.x,
                   nudge.y = nudge.y,
                   seed = seed
  )
}

#' @format NULL
#' @usage NULL
#' @keywords internal
#' @importFrom rlang %||%
#' @noRd
PositionJitternudge <- ggplot2::ggproto("PositionJitternudge", ggplot2::Position,
                                        jitter.width = NULL,
                                        jitter.height = NULL,
                                        nudge.x = NULL,
                                        nudge.y = NULL,

                                        required_aes = c("x", "y"),

                                        setup_params = function(self, data) {
                                          flipped_aes <- ggplot2::has_flipped_aes(data)
                                          data <- ggplot2::flip_data(data, flipped_aes)
                                          width <- self$jitter.width %||% (ggplot2::resolution(data$x, zero = FALSE) * 0.4)

                                          list(
                                            nudge.x = self$nudge.x,
                                            nudge.y = self$nudge.y,
                                            jitter.height = self$jitter.height,
                                            jitter.width = width / 2, #(ndodge + 2),
                                            seed = self$seed,
                                            flipped_aes = flipped_aes
                                          )
                                        },

                                        compute_panel = function(data, params, scales) {
                                          data <- ggplot2::flip_data(data, params$flipped_aes)

                                          trans_x <- if(params$jitter.width > 0) function(x) {jitter(x, amount = params$jitter.width) + params$nudge.x}
                                          trans_y <- if(params$jitter.height > 0) function(x) {jitter(x, amount = params$jitter.height)  + params$nudge.y}

                                          data <- ggplot2:::with_seed_null(params$seed, ggplot2::transform_position(data, trans_x, trans_y))
                                          ggplot2::flip_data(data, params$flipped_aes)
                                        }
)

globalVariables(c('%||%'))
