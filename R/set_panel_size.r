
#' set_panel_size
#'
#' @param p ggplot2
#' @param g gtable
#' @param file optional output filename
#' @param margin grid unit
#' @param width grid unit, requested panel width
#' @param height grid unit, requested panel height
#'
#' @return gtable with fixed panel sizes
#' @export
set_panel_size <- function(p=NULL, g=ggplotGrob(p), file=NULL, 
                           margin = unit(1,"mm"),
                           width=unit(4, "cm"), 
                           height=unit(4, "cm")){
  
  panels <- grep("panel", g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)
  
  if(getRversion() < "3.3.0"){
    
    # the following conversion is necessary
    # because there is no `[<-`.unit method
    # so promoting to unit.list allows standard list indexing
    g$widths <- grid:::unit.list(g$widths)
    g$heights <- grid:::unit.list(g$heights)
    
    g$widths[panel_index_w] <-  rep(list(width),  nw)
    g$heights[panel_index_h] <- rep(list(height), nh)
    
  } else {
    
    g$widths[panel_index_w] <-  rep(width,  nw)
    g$heights[panel_index_h] <- rep(height, nh)
    
  }
  
  if(!is.null(file))
    ggsave(file, g, 
           width = convertWidth(sum(g$widths) + margin, 
                                unitTo = "in", valueOnly = TRUE),
           height = convertHeight(sum(g$heights) + margin,  
                                  unitTo = "in", valueOnly = TRUE))
  
  g
}
