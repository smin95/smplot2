#' Adding a common legend on a combined figure
#'
#' @param combined_plot
#' Combined figure, an output from sm_put_together().
#' @param x
#' Location of the legend along the x-axis of the combined figure. Default is the middle origin (0.5).
#' @param y
#' Location of the legend along the y-axis of the combined figure. Default is the middle origin (0.5).
#' @param sampleplot
#' A variable containing one sample ggplot2 from which the legend can be derived.
#' @param legend
#' Pre-specified layer of legend created with sm_common_legend().
#' @param direction
#' Direction of the legend: 'horizontal' or 'vertical'.
#' @param border
#' If set TRUE, border around the legend will be created.
#' If set FALSE, the border will be removed.
#' @param legend_spacing
#' Spacing within the legend.
#' @param border_color
#' Color of the legend border
#' @param font_size
#' Text size of the legend
#' @importFrom patchwork inset_element
#'
#' @return
#' It prints a legend on a a combined plot. It can be used to create
#' a common legend for subplots.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check Ch7 of smin95.github.io/dataviz
#' }
sm_add_legend <- function(combined_plot, x,y, sampleplot, legend, direction='vertical',
                          border=TRUE, legend_spacing = 0.5, border_color='black',
                          font_size=12) {
  if (missing(legend)) {
    if (missing(sampleplot)) {
      stop('If legend is not provided, sampleplot should be provided so that a legend can be derived.')
    }

    poi <- sampleplot+sm_hgrid(legends=T) + theme(legend.direction = direction) +
      theme(legend.spacing.y = unit(legend_spacing, "mm"),
            legend.spacing.x = unit(legend_spacing, 'mm'),
            aspect.ratio = 1,
            legend.background = element_blank(),
            legend.text=element_text(size=font_size),
            legend.title = element_text(size=font_size))

    if (border==TRUE) {
      poi <- poi + theme(legend.box.background = element_rect(colour = border_color))
    } else {
      poi <- poi
    }

    tmp <- ggplot_gtable(ggplot_build(poi))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
  } else if (missing(sampleplot)) {
    sampleplot <- NULL
  }
  output <- combined_plot + inset_element(legend, x, y, x, y)
  return(output)
}
