# from ggpubr
stat_cor <- function(mapping = NULL, data = NULL,
                     method = "pearson", alternative = "two.sided",
                     cor.coef.name = c("R", "rho", "tau"), label.sep = ", ",
                     label.x.npc = "left", label.y.npc = "top",
                     label.x = NULL, label.y = NULL, output.type = "expression",
                     digits = 2, r.digits = digits, p.digits = digits,
                     r.accuracy = NULL, p.accuracy = NULL,
                     geom = "text", position = "identity",  na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  parse <- ifelse(output.type == "expression", TRUE, FALSE)
  cor.coef.name = cor.coef.name[1]
  layer(
    stat = StatCor, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(label.x.npc  = label.x.npc , label.y.npc  = label.y.npc,
                  label.x = label.x, label.y = label.y, label.sep = label.sep,
                  method = method, alternative = alternative, output.type = output.type, digits = digits,
                  r.digits = r.digits, p.digits = p.digits, r.accuracy = r.accuracy,
                  p.accuracy = p.accuracy, cor.coef.name = cor.coef.name,
                  parse = parse, na.rm = na.rm, ...)
  )
}


StatCor<- ggplot2::ggproto("StatCor", ggplot2::Stat,
                  required_aes = c("x", "y"),
                  default_aes = ggplot2::aes(hjust = ..hjust.., vjust = ..vjust..),

                  compute_group = function(data, scales, method, alternative, label.x.npc, label.y.npc,
                                           label.x, label.y, label.sep, output.type, digits,
                                           r.digits, p.digits, r.accuracy, p.accuracy, cor.coef.name)
                  {
                    if (length(unique(data$x)) < 2) {
                      # Not enough data to perform test
                      return(data.frame())
                    }
                    # Returns a data frame with estimate, p.value, label, method
                    .test <- .cor_test(
                      data$x, data$y, method = method, alternative = alternative,
                      label.sep = label.sep, output.type = output.type, digits = digits,
                      r.digits = r.digits, p.digits = p.digits,
                      r.accuracy = r.accuracy, p.accuracy = p.accuracy,
                      cor.coef.name = cor.coef.name
                    )
                    # Returns a data frame with label: x, y, hjust, vjust
                    .label.pms <- .label_params(data = data, scales = scales,
                                                label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                                                label.x = label.x, label.y = label.y ) %>%
                      mutate(hjust = 0)
                    cbind(.test, .label.pms)
                  }
)





# Correlation test
#::::::::::::::::::::::::::::::::::::::::
# Returns a data frame: estimatel|p.value|method|label
.cor_test <- function(x, y, method = "pearson", alternative = "two.sided",
                      label.sep = ", ", output.type = "expression",
                      digits = 2, r.digits = digits, p.digits = digits,
                      r.accuracy = NULL, p.accuracy = NULL,
                      cor.coef.name = "R"){
  # Overwritting digits by accuracy, if specified
  if(!is.null(p.accuracy)){
    nb_decimal_places <- round(abs(log10(p.accuracy)))
    p.digits <- nb_decimal_places
  }
  if(!is.null(r.accuracy)){
    nb_decimal_places <- round(abs(log10(r.accuracy)))
    r.digits <- nb_decimal_places
  }

  # Correlation analyses
  .cor <- suppressWarnings(stats::cor.test(
    x, y, method = method,  alternative = alternative,
    use = "complete.obs"
  ))
  estimate <- p.value <- p <- r <- rr <-  NULL
  z <- data.frame(estimate = .cor$estimate, p.value = .cor$p.value, method = method) %>%
    mutate(
      r = signif(estimate, r.digits),
      rr = signif(estimate^2, r.digits),
      p = signif(p.value, p.digits)
    )

  # Defining p and r labels
  pval <- .cor$p.value
  z <- z %>%
    dplyr::mutate(
      r.label = get_corcoef_label(
        r, accuracy = r.accuracy, prefix = "R",
        cor.coef.name = cor.coef.name, type = output.type
      ),
      rr.label = get_corcoef_label(
        rr, accuracy = r.accuracy, prefix = "R2",
        cor.coef.name = cor.coef.name, type = output.type
      ),
      p.label = get_p_label(
        p, accuracy = p.accuracy, type = output.type
      )
    )

  # Defining correlation labels
  if(output.type == "expression"){
    if(label.sep == "\n"){
      # Line break at each comma
      cortxt <- paste0("atop(", z$r.label, ",",  z$p.label, ")")
    }
    else{
      label.sep <- trimws(label.sep)
      if(label.sep == "") label.sep <- "~"
      #  Using "*" to avoid the space between the R2 value and comma
      else label.sep <- paste0("*`", label.sep, "`~")
      cortxt <- paste0(z$r.label, label.sep,  z$p.label)
    }
  }
  else if (output.type %in% c("latex", "tex", "text")){
    cortxt <- paste0(z$r.label, label.sep,  z$p.label)
  }
  z$label <- cortxt
  z
}


# Formatting R and P ----------------------
get_p_label <- function(x, accuracy = 0.0001, type = "expression"){
  if(is.null(accuracy)){
    label <- ifelse(x < 2.2e-16, "p < 2.2e-16", paste0("p = ", x))
  }
  else if (!(accuracy < 1)){
    stop(
      "Accuracy should be < 1; For example use 0.01, 0.001, 0.0001, etc.",
      call. = FALSE
    )
  }
  else{
    label <- scales::pvalue(x, accuracy = accuracy, add_p = TRUE)
    # Add space before and after: = or <
    label <- gsub(pattern = "(=|<)", replacement = " \\1 ", x = label)
  }
  if(type == "expression"){
    label <- gsub(pattern = "p = ", replacement = "italic(p)~`=`~", x = label, fixed = TRUE)
    label <- gsub(pattern = "p < ", replacement = "italic(p)~`<`~", x = label, fixed = TRUE)
  }
  label
}

# Prefix can be R or R^2.
# cor.coef.name: R, rho, tau
get_corcoef_label <- function(x, accuracy = 0.01, prefix = "R", cor.coef.name = "R", type = "expression"){
  if(is.null(accuracy)){
    label <- paste0(prefix, " = ", x)
  }
  else if(!(accuracy < 1)){
    stop(
      "Accuracy should be < 1; For example use 0.01, 0.001, 0.0001, etc.",
      call. = FALSE
    )
  }
  else{
    nb_decimal_places <- round(abs(log10(accuracy)))
    label <- formatC(x, digits = nb_decimal_places, format = "f")
    label <- paste0(prefix, " = ", label)
  }
  if(type == "expression"){
    label <- gsub(pattern = "R2 = ", replacement = "italic(R)^2~`=`~", x = label, fixed = TRUE)
    label <- gsub(pattern = "R = ", replacement = "italic(R)~`=`~", x = label, fixed = TRUE)
  }
  label <- gsub(pattern = "R", cor.coef.name, x = label, fixed = TRUE)
  label
}

# from ggpubr

.label_params <- function(data, scales, label.x.npc = "left", label.y.npc = "right",
                          label.x = NULL, label.y = NULL, .by = c("group", "panel"),
                          group.id = NULL, ...)
{

  .by <- match.arg(.by)
  # Check label coordinates for each group
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(is.null(group.id))
    group.id <- group.id <- abs(data$group[1])
  label.x.npc <- .group_coord(label.x.npc, group.id)
  label.y.npc <- .group_coord(label.y.npc, group.id)
  label.x <- .group_coord(label.x, group.id)
  label.y <- .group_coord(label.y, group.id)

  .check_npc_coord(label.x.npc, axis = "x")
  .check_npc_coord(label.y.npc, axis = "y")



  if (length(label.x) > 0) {
    x <- label.x
    hjust <- 0.5
  } else if (length(label.x.npc) > 0) {
    if (is.numeric(label.x.npc)) {
      x <- scales$x$dimension()[1] + label.x.npc *
        diff(scales$x$dimension())
      hjust <- 0.5
    } else if (is.character(label.x.npc)) {
      if (label.x.npc == "right") {
        x <- scales$x$dimension()[2]
        hjust <- 1
      } else if (label.x.npc %in% c("center", "centre", "middle")) {
        x <- mean(scales$x$dimension())
        hjust <- 0.5
      } else if (label.x.npc == "left") {
        x <- scales$x$dimension()[1]
        hjust <- 0
      }
    }
  }

  if (length(label.y) > 0) {
    y <- label.y
    vjust <- 0.5
  } else if (length(label.y.npc) > 0) {
    if (is.numeric(label.y.npc)) {
      y <- scales$y$dimension()[1] + label.y.npc *
        diff(scales$y$dimension())
      vjust <- 1.4 * group.id - (0.7 * length(group.id))
    } else if (is.character(label.y.npc)) {
      if (label.y.npc == "bottom") {
        y <- scales$y$dimension()[1]
        vjust <- -1.4 * group.id
      } else if (label.y.npc %in% c("center", "centre", "middle")) {
        y <- mean(scales$y$dimension())
        vjust <- 1.4 * group.id - (0.7 * length(group.id))
      } else if (label.y.npc == "top") {
        y <- scales$y$dimension()[2]
        vjust <- 1.4 * group.id
      }
    }
  }
  if(.by == "panel"){
    hjust <- 0.5
    vjust = 0.5
  }

  data.frame(x = x, y = y, hjust = hjust, vjust = vjust)
}


# Get label parameters by group
# Useful in boxplot, where group.ids is the index of the group: 1, 2, 3, etc
# Useful only when computation is done by panel
.label_params_by_group <- function(..., group.ids){
  purrr::map(group.ids,
             function(group.id, ...){.label_params(..., group.id = group.id)},
             ...) %>%
    dplyr::bind_rows() #%>%
  #dplyr::mutate(x = group.ids)

}



# Check label coordinates for each group
#:::::::::::::::::::::::::::::::::::::::::
# coord.values: label coordinate for each group. If too short, they are recycled.
# group.id the id of groups as returned by ggplot_build()
.group_coord <- function(coord.values, group.id){
  if(!.is_empty(coord.values)){
    coord.values <- ifelse(length(coord.values) >= group.id,
                           coord.values[group.id], coord.values[1])
  }
  coord.values
}


# Check NPC coord
#:::::::::::::::::::::::::::::::::::::::::
# npc: Normalised Parent Coordinates.
#   The origin of the viewport is (0, 0) and the viewport has a width and height of 1 unit.
#   For example, (0.5, 0.5) is the centre of the viewport.
# coord: should be between 0 and 1
# axis: should be "x" or "y"
.check_npc_coord <- function(.coord, axis = c("x", "y")){

  axis <- match.arg(axis)
  if(axis == "x")
    allowed.values <- c('right', 'left', 'center', 'centre', 'middle')
  else if(axis == "y")
    allowed.values <- c( 'bottom', 'top', 'center', 'centre', 'middle')

  .message <- paste0("'*.npc coord for ", axis, " axis should be either a numeric value in [0-1] ",
                     "or a character strings including one of ",
                     .collapse(allowed.values, sep = ", "))

  if(!is.null(.coord)){

    if(is.numeric(.coord)){
      if (any(.coord < 0 | .coord > 1)) {
        stop(.message)
      }
    }
    else if(is.character(.coord)){
      if(!(.coord %in% allowed.values))
        stop(.message)

    }
    else
      stop(.message)
  }
}

.is_empty <- function(x){
  length(x) == 0
}

.collapse <- function(x, y = NULL, sep = "."){
  if(missing(y))
    paste(x, collapse = sep)
  else if(is.null(x) & is.null(y))
    return(NULL)
  else if(is.null(x))
    return (as.character(y))
  else if(is.null(y))
    return(as.character(x))
  else
    paste0(x, sep, y)
}
