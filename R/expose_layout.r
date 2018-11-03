#' expose_layout
#'
#' @description Schematic view of a ggplot object's layout.
#' @param p ggplot
#' @param draw logical, draw the gtable
#' @param newpage logical
#'
#' @return gtable
#' @export
#'
#' @examples
#' p1 <- qplot(mpg, wt, data=mtcars, colour=cyl)
#' p2 <- qplot(mpg, data = mtcars) + ggtitle('title')
#' p3 <- qplot(mpg, data = mtcars, geom = 'dotplot')
#' p4 <- p1 + facet_wrap(~carb, nrow=1) + theme(legend.position='none') +
#'   ggtitle('facetted plot')
#' pl <- lapply(list(p1,p2, p3, p4), expose_layout, FALSE, FALSE)
#' grid.arrange(grobs=pl, widths=c(1.2,1,1),
#'              layout_matrix = rbind(c(1, 2, 3),
#'                                    c(4, 4, 4)))
expose_layout <- function(p, draw = TRUE, newpage = TRUE) {
    g <- ggplotGrob(p)
    lay <- g[["layout"]]
    gt <- g
    
    ids <- c("background", "panel", "axis", "lab", "guide", "strip", "title")
    
    replace_grob <- function(idname, col) {
        id <- grepl(idname, lay$name)
        pos <- lay[id, ]
        newgrob <- rectGrob(gp = gpar(col = "white", lwd = 1.2, fill = col))
        gt$grobs[id] <- replicate(nrow(pos), newgrob, simplify = FALSE)
    }
    
    cols <- c("grey95", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", 
        "#E5D8BD", "#FDDAEC")
    
    for (ii in seq_along(ids)) {
        id <- grepl(ids[ii], lay$name)
        pos <- lay[id, ]
        newgrob <- rectGrob(gp = gpar(col = "white", lwd = 1.2, fill = cols[ii]))
        gt$grobs[id] <- replicate(nrow(pos), newgrob, simplify = FALSE)
    }
    
    
    if (draw) {
        if (newpage) 
            grid.newpage()
        grid.draw(gt)
    }
    gt
}
