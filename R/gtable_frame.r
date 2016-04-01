
#' gtable_frame
#'
#' @param g gtable
#' @param width requested width
#' @param height requested height
#'
#' @return 3x3 gtable wrapping the plot
#' @export
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