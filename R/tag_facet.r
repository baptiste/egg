
#' tag_facet
#'
#' @description Adds a dummy text layer to a ggplot to label facets and sets facet strips to blank. 
#' This is the typical formatting for some journals that consider facets as subfigures 
#' and want to minimise margins around figures.
#' @param p ggplot
#' @param open opening character, default: (
#' @param close  closing character, default: )
#' @param tag_pool character vector to pick tags from
#' @param x x position within panel, default: -Inf
#' @param y y position within panel, default: Inf
#' @param hjust hjust
#' @param vjust vjust
#' @param fontface fontface
#' @param family font family
#' @param ... further arguments passed to geom_text layer
#'
#' @return plot with facet strips removed and replaced by in-panel tags 
#' @importFrom ggplot2 geom_text ggplot_build theme element_blank aes
#' @export
#' @examples
#' library(ggplot2)
#' mydf = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#' 
#' p <- ggplot(mydf) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_wrap(
#'     ~ red + blue)
#' tag_facet(p)

tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
    hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
    
    gb <- ggplot_build(p)
    lay <- gb$layout$layout
    tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
    p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
        vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) + theme(strip.text = element_blank(), 
        strip.background = element_blank())
}
