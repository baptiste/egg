#' symmetric_range
#'
#' @description Function to ensure that a position scale is symmetric about 0
#' @param range range of the data
#'
#' @return symmetric range
#' @importFrom ggplot2 scale_x_continuous
#' @export
#' @examples
#' library(ggplot2)
#' df = data.frame(x = c(1, 2),
#'                 y = c(5, 0.2),
#'                 group = c(1, 2))
#' p <- ggplot(df, aes(x = x, y = y)) + 
#'   geom_point() + 
#'   facet_wrap( ~ group, scale =
#'                 "free")
#' p + scale_y_continuous(limits = symmetric_range)
symmetric_range <- function(range) {
  max_abs <- max(abs(range))
  c(-max_abs, max_abs)
}


