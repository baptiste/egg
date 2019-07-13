
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
#' @importFrom ggplot2 geom_text ggplot_build theme element_blank aes_string
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


#' tag_facet_outside
#'
#' @description Adds a dummy text layer to a ggplot to label facets and sets facet strips to blank. 
#' This is the typical formatting for some journals that consider facets as subfigures 
#' and want to minimise margins around figures.
#' @param p ggplot
#' @param open opening character, default: (
#' @param close  closing character, default: )
#' @param tag_fun_top labelling function 
#' @param tag_fun_right labelling function
#' @param x x position within cell
#' @param y y position within cell
#' @param hjust hjust
#' @param vjust vjust
#' @param fontface fontface
#' @param family font family
#' @param draw logical: draw the resulting gtable
#' @param ... further arguments passed to geom_text layer
#' @return plot with facet strips removed and replaced by in-panel tags 
#' @importFrom ggplot2 ggplot_gtable geom_text ggplot_build theme element_blank 
#' @importFrom utils as.roman
#' @export
#' @examples
#' library(ggplot2)
#' d = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#' 
#' p <- ggplot(d) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_grid(red ~ blue)
#'   
#' tag_facet_outside(p)
#' 
tag_facet_outside <-  function(p, open=c("(",""), close = c(")","."),
                               tag_fun_top = function(i) letters[i],
                               tag_fun_right = utils::as.roman,
                               x = c(0,0), y = c(0.5, 1),
                               hjust = c(0,0), vjust = c(0.5,1), 
                               fontface = c(2,2), family="", draw = TRUE, ...){
  
  gb <- ggplot_build(p + theme(strip.text = element_blank(), 
                             strip.background = element_blank()))
  lay <- gb$layout$layout
  
  tags_top <- paste0(open[1],tag_fun_top(unique(lay$COL)),close[1])
  tags_right <- paste0(open[2],tag_fun_right(unique(lay$ROW)),close[2])
  
  tl <- lapply(tags_top, grid::textGrob, x=x[1], y=y[1],
               hjust=hjust[1], vjust=vjust[1], 
               gp=grid::gpar(fontface=fontface[1], fontfamily = family, ...))
  rl <- lapply(tags_right, grid::textGrob, x=x[2], y=y[2],
               hjust=hjust[2], vjust=vjust[2], 
               gp=grid::gpar(fontface=fontface[2], fontfamily = family, ...))
  
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  l <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=l)
  
  wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
  g <- gtable::gtable_add_cols(g, wm, pos = max(l))
  t <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  g <- gtable::gtable_add_grob(g, grobs = rl, t=t, l=max(l) + 1)
  g <- gtable::gtable_add_cols(g, unit(2,"mm"), pos = max(l))
  
  if(draw){
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  invisible(g)
}
