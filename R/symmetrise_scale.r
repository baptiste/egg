
#' symmetrise_scale
#'
#' @param p ggplot2
#' @param axis axis
#'
#' @return plot with scales adjusted
#' @export
#' @examples 
#' p1 <- qplot(mpg, wt, data=mtcars, colour=cyl) + facet_wrap(~carb, nrow=1, scales="free") 
#' symmetrise_scale(p1, "y")
symmetrise_scale <- function(p, axis = "x"){
  gb <- ggplot_build(p)
  type <- switch(axis, "x" = "x.range", "y" = "y.range")
  
  fname <- setdiff(names(gb$layout$layout), c("PANEL", "ROW", "COL",  "SCALE_X", "SCALE_Y"))  
  facets <- gb$layout$layout[ ,fname, drop=FALSE]
  
  lims <- do.call(cbind, lapply(gb$layout$panel_params, "[[", type))
  lims2 <- as.vector(t(tcrossprod(apply(abs(lims), 2, max), c(-1,1))))
  
  dummy <- setNames(data.frame(facets[rep(seq_len(nrow(facets)), each=2),], lims2), c(fname, axis))
  
  switch(axis, 
         "x" = p + geom_blank(data=dummy, aes(x=x, y=Inf), inherit.aes = FALSE), 
         "y" = p + geom_blank(data=dummy, aes(x=Inf, y=y), inherit.aes = FALSE))
}
