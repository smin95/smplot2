StatLm <- ggplot2::ggproto("StatLm", ggplot2::Stat,
                  required_aes = c("x", "y"),
                  default_aes = ggplot2::aes(color = 'black',
                                              size = 1,
                                             linetype = 'dashed'),
                  compute_group = function(data, scales) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = rng)

                    mod <- lm(y ~ x, data = data)
                    grid$y <- predict(mod, newdata = grid)

                    grid
                  }
)

stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatLm, data = data,  geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
