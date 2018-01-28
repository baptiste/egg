
#' symmetrise_scale
#'
#' @param p ggplot2
#' @param axis axis
#'
#' @return plot with scales adjusted
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom ggplot2 geom_blank ggplot_build aes_string
#' @export
#' @examples
#' library(ggplot2)
#' p1 <- qplot(mpg, wt, data=mtcars, colour=cyl) + facet_wrap(~carb, nrow=1, scales="free")
#' symmetrise_scale(p1, "y")

ggggr <- function(g, nmax=100) {
  p <- ggplot2:::`+.gg`(
    ggplot2::ggplot(),
    ggplot2::annotation_custom(g)
  )
  Recall(grid.draw(ggplot2::ggplotGrob(p)))
}
