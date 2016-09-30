
#' gtable_frame
#'
#' @param g gtable
#' @param width requested width
#' @param height requested height
#' @param debug logical draw gtable cells
#'
#' @importFrom gtable gtable_matrix gtable_add_grob gtable_add_cols gtable_add_rows
#' @importFrom grid unit unit.c nullGrob rectGrob grid.newpage grid.draw
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
#' fg12 <- gtable_frame(rbind(fg1,fg2), width=unit(2,"null"), height=unit(1,"null"))
#' fg3 <- gtable_frame(g3, width=unit(1,"null"), height=unit(1,"null"))
#' grid.newpage()
#' combined <- cbind(fg12, fg3)
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



#' gtable_frame
#'
#' @param ... ggplot objects
#' @param plots list of ggplots
#' @param heights list of requested heights
#' @param widths list of requested widths
#' @param nrow number of rows
#' @param ncol number of columns
#' @param byrow logical, fill by rows
#' @param debug logical, show layout with thin lines
#'
#'
#' @importFrom grid is.unit
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
                      draw = TRUE, newpage = TRUE,
                      debug = FALSE){
  
  n <- length(plots)
  grobs <- lapply(plots, ggplotGrob)
  
  
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
    rows <- lapply(spl, function(.r) do.call(gridExtra::cbind.gtable, .r))
    all <- do.call(gridExtra::rbind.gtable, rows)
  } else { # fill colwise
    cols <- lapply(spl, function(.c) do.call(gridExtra::rbind.gtable, .c))
    all <- do.call(gridExtra::cbind.gtable, cols)
  }
  
  if(draw) {
    if(newpage) grid.newpage()
    grid.draw(all)
  }
  
  invisible(all) # return the full gtable
}

