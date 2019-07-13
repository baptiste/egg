
#' Theme with minimalistic (and opinionated) defaults suitable for publication
#'
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 theme_bw element_line element_rect element_text
#' @importFrom grid unit
#' @export
#' @examples
#' library(ggplot2)
#' 
#' d = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#' 
#' p <- ggplot(d) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_grid(red ~ blue)
#' tag_facet(p + theme_article())
#' p + theme_presentation()
#' 
#' # example of use with cairo device
#' # ggsave("fig_talk.pdf", p + theme_presentation("Source Sans Pro"), 
#' #          width=14, height=7, device = cairo_pdf, bg='transparent')
theme_article <- function(base_size = 11, base_family = "") {
  gray <- "#464646"
  fg <- "#000000"
  theme_bw(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(colour = gray), 
          rect = element_rect(fill = NA, colour = NA), 
          text = element_text(colour = fg), 
          axis.ticks = element_line(colour = gray), 
          legend.key = element_rect(colour = NA, fill = NA), 
          legend.key.height = unit(3,"mm"),
          panel.border = element_rect(colour = gray, fill = NA), 
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          panel.background=element_blank(), 
          strip.background = element_blank())
}

#' Theme with minimalistic (and opinionated) defaults suitable for presentation
#'
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 theme_bw element_line element_rect element_text
#' @importFrom grid unit
#' @describeIn theme
#' @export
theme_presentation <- function(base_size = 24, base_family = "") {
  gray <- "#464646"
  bg = 'grey98'
  fg <- "#000000"
  theme_bw(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(colour = gray), 
          rect = element_rect(fill = NA, colour = NA), 
          text = element_text(colour = fg), 
          axis.ticks = element_line(colour = gray), 
          legend.key = element_rect(colour = NA, fill = bg), 
          legend.key.height = unit(3,"mm"),
          panel.border = element_rect(colour = gray, fill = NA), 
          panel.grid = element_blank(), 
          panel.background=element_rect(fill=bg),
          strip.background = element_blank())
}
