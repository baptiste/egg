
#' gtable_frame
#'
#' @param g gtable
#' @param width requested width
#' @param height requested height
#'
#' @importFrom gtable gtable_matrix gtable_add_grob
#' @return 3x3 gtable wrapping the plot
#' @export
#' @examples 
#' library(grid)
#' library(gtable)
#' p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() 
#' 
#' p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() + facet_wrap( ~ cyl, ncol=2, scales = "free") +
#'   guides(colour="none") +
#'   theme()
#' 
#' p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() + facet_grid(. ~ cyl, scales = "free")
#' 
#' g1 <- ggplotGrob(p1);
#' g2 <- ggplotGrob(p2);
#' g3 <- ggplotGrob(p3);
#' fg1 <- gtable_frame(g1)
#' fg2 <- gtable_frame(g2)
#' fg12 <- gtable_frame(rbind(fg1,fg2), width=unit(2,"null"), height=unit(1,"null"))
#' fg3 <- gtable_frame(g3, width=unit(1,"null"), height=unit(1,"null"))
#' grid.newpage()
#' combined <- cbind(fg12, fg3)
#' combined <- gtable_add_grob(combined, rectGrob(gp=gpar(fill="grey98", alpha=0.5, lty=2, lwd=1.5)),
#'                             l=2, r=5, t=2, b=2, z=Inf, name="debug")
#' grid.draw(combined)
gtable_frame <- function(g, width=unit(1,"null"), height=unit(1,"null")){
  panels <- g[["layout"]][grepl("panel", g[["layout"]][["name"]]), ]
  ll <- unique(panels$l)
  tt <- unique(panels$t)
  
  fixed_ar <- g$respect
  if(fixed_ar) { # there lies madness, we want to align with aspect ratio constraints
    ar <- as.numeric(g$heights[tt[1]]) / as.numeric(g$widths[ll[1]])
    print(ar)
    height <- width * ar
    g$respect <- FALSE
  }
  
  core <- g[seq(min(tt), max(tt)), seq(min(ll), max(ll))]
  top <- g[seq(1, min(tt)-1), ]
  bottom <- g[seq(max(tt)+1, nrow(g)), ]
  left <- g[, seq(1, min(ll)-1)]
  right <- g[, seq(max(ll)+1, ncol(g))]
  
  fg <- nullGrob()
  lg <-  if(length(left))  g[seq(min(tt), max(tt)), seq(1, min(ll)-1)] else fg
  rg <- if(length(right)) g[seq(min(tt), max(tt)), seq(max(ll)+1,ncol(g))] else fg
  grobs = list(fg, g[seq(1, min(tt)-1), seq(min(ll), max(ll))], fg, 
               lg, g[seq(min(tt), max(tt)), seq(min(ll), max(ll))], rg, 
               fg, g[seq(max(tt)+1, nrow(g)), seq(min(ll), max(ll))], fg)
  widths <- unit.c(sum(left$widths), width, sum(right$widths))
  heights <- unit.c(sum(top$heights), height, sum(bottom$heights))
  all <- gtable_matrix("all", grobs = matrix(grobs, ncol=3, nrow=3, byrow = TRUE), 
                       widths = widths, heights = heights)
  all[["layout"]][5,"name"] <- "panel" # make sure knows where the panel is
  if(fixed_ar)  all$respect <- TRUE
  all
}

#' @export
.dummy_plot <- gtable::gtable_matrix("placeholder", matrix(replicate(9, grid::nullGrob(), simplify = FALSE), 3, 3), 
                                     widths=rep(unit(1,"null"), 3), 
                                     heights = rep(unit(1,"null"), 3))