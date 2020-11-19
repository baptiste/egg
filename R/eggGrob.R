

nog <- function(size=1, ...){
  
  theta <- seq(0,2*pi,length=360)
  
  b <- 2.02
  k <- 0.97
  x <- 1 /(2* sqrt(1+k^2)) * ((k-1)/k*b + (k^2+1)/k * 
                                sqrt(b^2 - 4*k*cos(theta)) ) 
  y <- 2*sin(theta) / (b + sqrt(b^2 - 4*k*cos(theta)))
  
  data.frame(x = size*x, y = size*y)
}

# a <- nog()
# library(ggplot2)
# ggplot(a, aes(x,y))+
#   geom_polygon(fill='bisque2') +
#   coord_equal() +
#   theme_void()
