
#' Theme with minimalistic (and opinionated) defaults suitable for publication
#'
#' Theme based on ggthemes::theme_chew
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 theme_bw element_line element_rect element_text
#' @export
#' @examples
#' library(ggplot2)
#' ggplot() + theme_article()
theme_article <- function(base_size = 11, base_family = "") {
    gray <- "#464646"
    black <- "#000000"
    theme_bw(base_size = base_size, base_family = base_family) + 
      theme(line = element_line(colour = gray), 
        rect = element_rect(fill = NA, colour = NA), 
        text = element_text(colour = black), 
        axis.ticks = element_line(colour = gray), 
        legend.key = element_rect(colour = NA), 
        panel.border = element_rect(colour = gray), 
        panel.grid = element_blank(), 
        strip.background = element_blank())
}
