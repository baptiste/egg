
#' gtable_frame
#'
#' @param g gtable
#' @param width requested width
#' @param height requested height
#' @param debug logical draw gtable cells
#'
#' @importFrom gtable gtable_matrix gtable_add_grob gtable_add_cols gtable_add_rows
#' @importFrom grid unit unit.c nullGrob rectGrob grid.newpage grid.draw
#' @importFrom gridExtra gtable_rbind gtable_cbind
#' @return 3x3 gtable wrapping the plot
#' @export
#' @examples 
#' library(grid)
#' library(gridExtra)
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
#' fg12 <- gtable_frame(gtable_rbind(fg1,fg2), width=unit(2,"null"), height=unit(1,"null"))
#' fg3 <- gtable_frame(g3, width=unit(1,"null"), height=unit(1,"null"))
#' grid.newpage()
#' combined <- gtable_cbind(fg12, fg3)
#' grid.draw(combined)
gtable_frame <- function(g, width=unit(1,"null"), height=unit(1,"null"), debug=FALSE){
  panels <- g[["layout"]][grepl("panel", g[["layout"]][["name"]]), ]
  ll <- unique(panels$l)
  tt <- unique(panels$t)
  
  fixed_ar <- g$respect
  if(fixed_ar) { # there lies madness, we want to align with aspect ratio constraints
    ar <- as.numeric(g$heights[tt[1]]) / as.numeric(g$widths[ll[1]])
    height <- width * ar
    g$respect <- FALSE
  }
  
  core <-   g[seq(min(tt), max(tt)),   seq(min(ll), max(ll))]
  top <-    g[seq(1, min(tt)-1),       seq(min(ll), max(ll))]
  bottom <- g[seq(max(tt)+1, nrow(g)), seq(min(ll), max(ll))]
  left <-   g[seq(min(tt), max(tt)),   seq(1, min(ll)-1)]
  right <-  g[seq(min(tt), max(tt)),   seq(max(ll)+1,ncol(g))]
  
  fg <- nullGrob()
  
  if(length(left))  {
    # add a dummy grob to make sure things stick to the panel
    lg <- gtable::gtable_add_cols(left, unit(1,"null"), 0)
    lg <- gtable::gtable_add_grob(lg, fg, 1, l=1)
  } else {
    lg <- fg
  }
  
  if(length(right))  {
    # add a dummy grob to make sure things stick to the panel
    rg <- gtable::gtable_add_cols(right, unit(1,"null"))
    rg <- gtable::gtable_add_grob(rg, fg, 1, l=ncol(rg))
  } else {
    rg <- fg
  }
  
  if(length(top))  {
    # add a dummy grob to make sure things stick to the panel
    tg <- gtable::gtable_add_rows(top, unit(1,"null"), 0)
    tg <- gtable::gtable_add_grob(tg, fg, t = 1, l = 1)
  } else {
    tg <- fg
  }
  
  if(length(bottom))  {
    # add a dummy grob to make sure things stick to the panel
    bg <- gtable::gtable_add_rows(bottom, unit(1,"null"), -1)
    bg <- gtable::gtable_add_grob(bg, fg, t = nrow(bg), l = 1)
  } else {
    bg <- fg
  }
  
  ## 3x3 cells (corners contain nullGrob)
  grobs = list(fg, tg,   fg, 
               lg, core, rg, 
               fg, bg,   fg)
  
  widths <- unit.c(sum(left$widths), width, sum(right$widths))
  heights <- unit.c(sum(top$heights), height, sum(bottom$heights))
  all <- gtable::gtable_matrix("all", grobs = matrix(grobs, ncol=3, nrow=3, byrow = TRUE), 
                               widths = widths, heights = heights)
  
  if(debug){
    hints <- grid::rectGrob(gp=gpar(fill=NA, lty=2, lwd=0.2))
    tl <- expand.grid(t=1:3, l=1:3)
    all <- gtable::gtable_add_grob(all, replicate(9, hints, simplify = FALSE), 
                                   t=tl$t, l=tl$l, z=Inf, name="debug")
  } 
  all[["layout"]][5,"name"] <- "panel" # make sure knows where the panel is
  if(fixed_ar)  all$respect <- TRUE
  all
}

.tmp <- gtable::gtable_matrix("placeholder", matrix(replicate(9, grid::nullGrob(), simplify = FALSE), 3, 3), 
                              widths=rep(unit(1,"null"), 3), 
                              heights = rep(unit(1,"null"), 3))
.tmp$layout$name[5] <- "panel"

#' @export
.dummy_gtable <- .tmp

#' @importFrom ggplot2 ggplot theme_void
#' @export
.dummy_ggplot <- ggplot() + theme_void()


# stolen from grid (because unexported)
as.unit.list <- function (unit) 
{
  if (inherits(unit, "unit.list")) 
    unit
  else {
    l <- length(unit)
    result <- vector("list", l)
    for (i in seq_len(l)) result[[i]] <- unit[i]
    class(result) <- c("unit.list", "unit")
    result
  }
}



#' ggarrange
#'
#' @param ... ggplot objects
#' @param plots list of ggplots
#' @param nrow number of rows
#' @param ncol number of columns
#' @param heights list of requested heights
#' @param widths list of requested widths
#' @param byrow logical, fill by rows
#' @param top optional string, or grob
#' @param bottom optional string, or grob
#' @param left optional string, or grob
#' @param right optional string, or grob
#' @param padding unit of length one, margin around annotations
#' @param clip argument of gtable
#' @param newpage logical: draw on a new page
#' @param draw logical: draw or return a grob
#' @param debug logical, show layout with thin lines
#' @importFrom grid is.unit is.grob
#' @importFrom grDevices n2mfrow
#' @importFrom gridExtra gtable_cbind gtable_rbind
#' @return gtable of aligned plots
#' @export
#' @examples 
#' library(grid)
#' p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() 
#' p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
#'   geom_point() + facet_wrap( ~ cyl, ncol=2, scales = "free") +
#'   guides(colour="none") +
#'   theme()
#' grid.newpage()
#' grid.draw(ggarrange(p1, p2, widths = c(2,1)))
ggarrange <- function(..., plots = list(...), 
                      nrow = NULL, ncol = NULL, 
                      widths = NULL, heights = NULL,
                      byrow = TRUE, 
                      top = NULL, bottom = NULL, 
                      left = NULL, right = NULL,
                      padding = unit(0.5,"line"),
                      clip = "on",
                      draw = TRUE, newpage = TRUE,
                      debug = FALSE){
  
  n <- length(plots)
  grobs <- lapply(plots, ggplot2::ggplotGrob)
  
  
  ## logic for the layout
  # if nrow/ncol supplied, honour this
  # if not, use length of widths/heights, if supplied
  # if nothing supplied, work out sensible defaults
  
  ## nothing to be done but check inconsistency
  if (!is.null(ncol) && !is.null(widths)){
    stopifnot(length(widths) == ncol)
  }
  if (!is.null(nrow) && !is.null(heights)){
    stopifnot(length(heights) == nrow)
  }
  ## use widths/heights if supplied
  if (is.null(ncol) && !is.null(widths)){
    ncol <- length(widths)
  }
  if (is.null(nrow) && !is.null(heights)){
    nrow <- length(heights)
  }
  ## work out the missing one
  if(is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(n/ncol)
  }
  if(is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(n/nrow)
  }
  
  ## it may happen that sufficient info was passed,
  ## but incompatible with number of grobs (fewer cells)
  stopifnot(nrow*ncol >= n)
  
  ## last case: nothing exists
  if(is.null(nrow) && is.null(ncol) && 
     is.null(widths) && is.null(heights)) 
  {
    nm <- grDevices::n2mfrow(n)
    nrow = nm[1]
    ncol = nm[2]
  }
  
  if(n %/% nrow) { # trouble, we need to add dummy grobs to fill the layout
    grobs <- c(grobs, rep(list(.dummy_gtable), nrow*ncol - n))
  }
  
  ## case numeric
  if(is.numeric(widths)) widths <- lapply(widths, unit, "null")
  if(is.numeric(heights)) heights <- lapply(heights, unit, "null")
  
  ## sizes
  if(is.null(widths)) widths <- lapply(rep(1, n), unit, "null")
  if(is.null(heights)) heights <- lapply(rep(1, n), unit, "null")
  
  # user may naively have passed grid units, but
  # only unit.list units work well with `[` so convert to this class
  if(grid::is.unit(widths)) widths <- as.unit.list(widths)
  if(grid::is.unit(heights)) widths <- as.unit.list(heights)
  
  ## split the list into rows/cols
  nrc <- if(byrow) nrow else ncol
  if(nrc==1) {
    splits <- rep(1, n) 
  } else {
    
    seqgrobs <- seq_along(grobs)
    splits <- cut(seqgrobs, nrc, labels = seq_len(nrc))
    ## widths and heights refer to the layout
    # repeat for corresponding grobs
    
    seqw <- splits[c(matrix(seqgrobs, nrow = nrow, byrow=byrow))]
    seqh <- splits[c(matrix(seqgrobs, nrow = nrow, byrow=!byrow))]
    
    widths <- widths[seqw]
    heights <- heights[seqh]
  }
  
  
  fg <- mapply(gtable_frame, g=grobs,  width = widths, height=heights, 
               MoreArgs = list(debug=debug), SIMPLIFY = FALSE)
  
  spl <- split(fg, splits)
  if(byrow){
    rows <- lapply(spl, function(.r) do.call(gridExtra::gtable_cbind, .r))
    gt <- do.call(gridExtra::gtable_rbind, rows)
  } else { # fill colwise
    cols <- lapply(spl, function(.c) do.call(gridExtra::gtable_rbind, .c))
    gt <- do.call(gridExtra::gtable_cbind, cols)
  }
  
  
  ## titles given as strings are converted to text grobs
  if(is.character(top)){
    top <- textGrob(top)
  }
  if(is.grob(top)){
    h <- grobHeight(top) + padding
    gt <- gtable_add_rows(gt, heights=h, 0)
    gt <- gtable_add_grob(gt, top, t=1, l=1, r=ncol(gt), z=Inf,
                          clip = clip)
  }
  if(is.character(bottom)){    
    bottom <- textGrob(bottom)
  }
  if(is.grob(bottom)){
    h <- grobHeight(bottom) + padding
    gt <- gtable_add_rows(gt, heights = h, -1)
    gt <- gtable_add_grob(gt, bottom, 
                          t=nrow(gt), l=1, r=ncol(gt), z=Inf,
                          clip = clip)
  }
  if(is.character(left)){
    left <- textGrob(left, rot = 90)
  }
  if(is.grob(left)){
    w <- grobWidth(left) + padding
    gt <- gtable_add_cols(gt, widths=w, 0)
    gt <- gtable_add_grob(gt, left, t=1, b=nrow(gt), 
                          l=1, r=1, z=Inf,
                          clip = clip)
  }
  if(is.character(right)){
    right <- textGrob(right, rot = -90)
  }
  if(is.grob(right)){
    w <- grobWidth(right) + padding
    gt <- gtable_add_cols(gt, widths=w, -1)
    gt <- gtable_add_grob(gt, right, 
                          t=1, b=nrow(gt), 
                          l=ncol(gt), r=ncol(gt), z=Inf,
                          clip = clip)
  }
  
  if(draw) {
    if(newpage) grid.newpage()
    grid.draw(gt)
  }
  class(gt) <- c("egg", class(gt))
  invisible(gt) # return the full gtable
}


##' @noRd
##' @importFrom grDevices dev.interactive dev.new
##' @export
print.egg = function(x, ...) {
  grid.newpage()
  grid.draw(x)
}
