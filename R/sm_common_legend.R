#' Creating a common legend for subplots on a separate panel
#'
#' @param ...
#' Additional arguments to adjust the legend within the theme() function.
#' In usual cases, this argument should not be filled.
#' @param x
#' Location of the legend along the x-axis. Default is the middle origin (0.5).
#' @param y
#' Location of the legend along the y-axis. Default is the middle origin (0.5).
#' @param title
#' Title of the legend. Input should be string
#' @param direction
#' Direction of the legend: 'horizontal' or 'vertical'.
#' @param border
#' If set TRUE, border around the legend will be created.
#' If set FALSE, the border will be removed.
#' @param legend_spacing
#' Spacing within the legend.
#' @param border_color
#' Color of the legend border
#'
#' @return
#' It prints a legend on a blank plot. It can be used to create
#' a common legend for subplots.
#' @export
#'
#' @importFrom cowplot theme_nothing
#'
#' @examples
#' \dontrun{
#' df2 <- read_csv('https://www.smin95.com/amblyopia_random.csv')
#' df2 %>% select(Subject, SF, absBP, Condition) %>%
#' ggplot(aes(x = SF, y = absBP, shape = Condition, group = Condition)) +
#'  geom_point(color='black', fill = 'black', size = 2.5) +
#'  sm_common_legend(x = .5, y = 0.5 , title=FALSE) +
#'  scale_shape_manual(values = c(21,22),
#'                     labels = c('Condition 1 ',
#'                                'Condition 2 '))
#' }
sm_common_legend <- function(..., x = 0.5, y = 0.5, title=FALSE, direction='vertical',
                             border=TRUE, legend_spacing = 0.5, border_color='black') {
  params <- list(...)
  blank <- do.call('annotate', list(geom = 'rect', ymin=-Inf,ymax=Inf,xmin=-Inf,xmax=Inf,
                                                       fill = 'white'))
  blank2 <- theme_nothing()
  box <- do.call('theme', modifyList(params, list(legend.spacing.y = unit(legend_spacing, "mm"),
                                                  legend.spacing.x = unit(legend_spacing, 'mm'),
                                                  aspect.ratio = 1,
                                                  legend.background = element_blank(),
                                                  legend.box.background = element_rect(colour = border_color))))

  if (title==TRUE) {
    location <- do.call('theme',  list(legend.position = c(x,y), legend.direction = direction))
  } else {
    location <- do.call('theme', list(legend.position = c(x,y),legend.direction = direction,
                                                         title = element_blank()))
  }

  if (border==FALSE) {
    box <- NULL
  }

  list(blank, blank2, location, box)
}
