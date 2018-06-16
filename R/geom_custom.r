
#' geom_custom
#'
#' @description Draw user-defined grobs, typically annotations, at specific locations.
#' @param mapping mapping
#' @param data data
#' @param inherit.aes inherit.aes
#' @param ... arguments passed to the geom's draw_group method
#'
#' @importFrom gtable gtable_matrix gtable_add_grob gtable_add_cols gtable_add_rows
#' @importFrom grid nullGrob unit grobTree editGrob grobName
#' @importFrom ggplot2 ggproto ggproto_parent layer
#' @return layer
#' @export
#' @examples
#' library(grid)
#' d <- data.frame(x=rep(1:3, 4), f=rep(letters[1:4], each=3))
#' gl <- replicate(4, matrix(sample(palette(), 9, TRUE), 3, 3), FALSE)
#' dummy <- data.frame(f=letters[1:4], data = I(gl))
#'
#' ggplot(d, aes(f,x)) +
#'   facet_wrap(~f)+
#'   theme_bw() +
#'   geom_point()+
#'   geom_custom(data = dummy, aes(data = data, y = 2),
#'               grob_fun = function(x) rasterGrob(x, interpolate = FALSE,
#'                                                 width=unit(1,"cm"),
#'                                                 height=unit(1,"cm")))

geom_custom <- function(mapping = NULL,
                        data = NULL,
                        inherit.aes = TRUE,
                        ...) {
  layer(
    geom = GeomCustom,
    mapping = mapping,
    data = data,
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomCustom <- ggproto(
  "GeomCustom",
  Geom,
  
  handle_na = function(self, data, params) {
    data # do nothing
  },
  
  setup_data = function(self, data, params) {
    data <- ggproto_parent(Geom, self)$setup_data(data, params)
    data
  },


  draw_group = function(data, panel_scales, coord, grob_fun, fun_params=list()) {
    coords <- coord$transform(data, panel_scales)
    gl <- lapply(
      seq_along(data$data),
      function(i) {
        .g <- do.call(grob_fun, c(list(data$data[[i]]), fun_params))
        grid::editGrob(
          .g,
          x = unit(coords$x[i], "native"),
          y = unit(coords$y[i], "native")
        )
      }
    )
    # grid::grobName(do.call(grobTree, gl), "geom_custom")    
    do.call(grobTree, gl)

  },

  required_aes = c("data", "x", "y")
)
