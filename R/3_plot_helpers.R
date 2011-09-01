# plot helper functions
# 
# Author: Andrie
###############################################################################

#' Blank axis titles and no legend
#' 
#' @keywords internal
quiet <- opts(
    legend.position="none",
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank()
)

#' Blank axis titles
#' 
#' @keywords internal
quiet_axes <- opts(
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank()
)

################################################################################

#' Define minimal theme
#' 
#' @keywords internal
theme_minimal <- opts(
    axis.title.x = theme_blank(),
    axis.title.y = theme_blank(),
    axis.text.x  = theme_blank(),
    axis.text.y  = theme_blank(),
    axis.ticks   = theme_blank(),
    axis.ticks.margin = unit(rep(0,4), "lines"),
    axis.ticks.length = unit(0, "cm"),
    panel.border = theme_blank(),
    panel.ticks  = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    plot.margin = unit(rep(0,4), "lines"),
    panel.margin = unit(rep(0,4), "lines"),
    legend.position  = "none"
)

################################################################################

#' Sets up default surveyor theme for use in ggplot. 
#' 
#' @param surveyor A surveyor object
#' @param q_id The question id
#' @param counter The file number
#' @param f Results from code_* function
#' @param g Results from stats_* function
#' @param h Results from plot_* function
#' @param plot_size the plot size in inches
#' @keywords internal
theme_surveyor <- function (base_size = 12, base_family = ""){
  structure(
      list(
          axis.line = theme_blank(), 
          axis.text.x = theme_text(
              family = base_family, 
              size = base_size * 0.8, 
              lineheight = 0.9, 
              colour = "grey20", #"grey50", 
              vjust = 1), 
          axis.text.y = theme_text(
              family = base_family, 
              size = base_size * 0.8, 
              lineheight = 0.9, 
              colour = "grey20", #"grey50",
              hjust = 1), 
          axis.ticks = theme_segment(colour = "grey50"), 
          axis.title.x = theme_text(family = base_family, size = base_size, vjust = 0.5), 
          axis.title.y = theme_text(family = base_family, 
              size = base_size, angle = 90, vjust = 0.5), 
          axis.ticks.length = unit(0.15, "cm"), 
          axis.ticks.margin = unit(0.1, "cm"), legend.background = theme_rect(colour = "white"), 
          legend.key = theme_rect(fill = "grey95", colour = "white"), 
          legend.key.size = unit(1.2, "lines"), legend.key.height = NA, 
          legend.key.width = NA, 
          legend.text = theme_text(family = base_family, size = base_size * 0.8), 
          legend.text.align = NA, 
          legend.title = theme_text(family = base_family, size = base_size * 
                  0.8, face = "bold", hjust = 0), 
          legend.title.align = NA, 
          legend.position = "right", 
          legend.direction = "vertical", 
          legend.box = NA, 
          panel.background = theme_rect(fill = "grey90", 
              colour = NA), 
          panel.border = theme_blank(), panel.grid.major = theme_line(colour = "white"), 
          panel.grid.minor = theme_line(colour = "grey95", size = 0.25), 
          panel.margin = unit(0.25, "lines"), 
          strip.background = theme_rect(fill = "grey80", 
              colour = NA), 
          strip.text.x = theme_text(family = base_family, size = base_size * 0.8), 
          strip.text.y = theme_text(family = base_family, size = base_size * 0.8, angle = -90), 
          plot.background = theme_rect(colour = NA, fill = "white"), 
          plot.title = theme_text(family = base_family, size = base_size * 1.2), 
          plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
      ), 
      class = "options"
  )
}

################################################################################

#' Applies format function to x.
#' 
#' Applies format function (specified by formatter) to x.
#' 
#' @param x Character vector
#' @param formatter Formatting function
#' @keywords internal
format_values <- function(x, formatter){
  match.fun(formatter)(x)
}

################################################################################

#' Applies formatting to labels and calculates justification position.
#' 
#' Takes a surveyor_stats object and adds two additional data columns: value_labels and labels_just.
#' 
#' @param s A surveyor_stats object
#' @keywords internal
format_labels <- function(s){
  stopifnot(class(s)=="surveyor_stats")
  s$data$value_labels <- match.fun(s$formatter)(s$data$value)
  s$data$labels_just <- -0.1 + 1.2 * with(s$data, as.numeric(value >= mean(value)))
  s
}



