#' Simultaneously nudge and jitter (by: mspeekenbrink)
#'
#' This is directly taken from the sdam-r package by mspeekenbrink.
#'
#' @family position adjustments
#'
#' @param jitter.width degree of jitter in x direction. Defaults to 40% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param nudge.x the amount to nudge in the x direction.
#' @param nudge.y the amount to nudge in the y direction.
#' @param seed Optional seed for the random jitter
#'
#' @return Positions for data in a \code{ggplot2::ggplot} object, similar to e.g. \code{ggplot2::position_jitter}
#' @seealso [ggplot2::position_jitter()], which is the basis of this function.
#' @export
#' @examples
#' library(ggplot2)
#' dsub <- diamonds[ sample(nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
#'   geom_boxplot(outlier.size = 0) +
#'   geom_point(pch = 21, position = position_jitterdodge())
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
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

