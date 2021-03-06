## ----setup, echo=FALSE, results='hide', message=FALSE--------------------
library(egg)
library(grid)
library(gridExtra)
library(gtable)
library(knitr)
opts_chunk$set(message = FALSE,
               fig.width = 7,
               fig.height = 3)

## ----layout--------------------------------------------------------------
p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- qplot(mpg, data = mtcars) + ggtitle("title")
p3 <- qplot(mpg, data = mtcars, geom = "dotplot")
p4 <-
  p1 + facet_wrap( ~ carb, nrow = 1) + theme(legend.position = "none") +
  ggtitle("facetted plot")
pl <- lapply(list(p1, p2, p3, p4), expose_layout, FALSE, FALSE)
grid.arrange(
  grobs = pl,
  widths = c(1.2, 1, 1),
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 4, 4))
)

## ----panel, fig.height=3.5-----------------------------------------------
p1 <- qplot(mpg, wt, data = mtcars, colour = cyl)
p2 <- p1 + facet_wrap( ~ carb, nrow = 1)
grid.arrange(grobs = lapply(
  list(p1, p2),
  set_panel_size,
  width = unit(2, "cm"),
  height = unit(1, "in")
))

## ----frame---------------------------------------------------------------
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()

p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme()

p3 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_grid(. ~ cyl, scales = "free")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)

fg1 <- gtable_frame(g1, debug = TRUE)
fg2 <- gtable_frame(g2, debug = TRUE)
fg12 <-
  gtable_frame(gtable_rbind(fg1, fg2),
               width = unit(2, "null"),
               height = unit(1, "null"))
fg3 <-
  gtable_frame(
    g3,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = TRUE
  )
grid.newpage()
combined <- gtable_cbind(fg12, fg3)
grid.draw(combined)

## ----ggarrange-----------------------------------------------------------
p1 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point()
p2 <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() + facet_wrap(~ cyl, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme()

ggarrange(p1, p2, widths = 1:2)


## ----ggarrangelayout-----------------------------------------------------
p <- ggplot()
ggarrange(p, p, p, widths = c(3, 1), heights = c(5, 1))

## ----symmetrise----------------------------------------------------------
df = data.frame(x = c(1, 2),
                y = c(5, 0.2),
                group = c(1, 2))
p <- ggplot(df, aes(x = x, y = y)) + 
  geom_point() + 
  facet_wrap( ~ group, scale =
                "free")
symmetrise_scale(p, "y")

## ----custompics----------------------------------------------------------
codes <- data.frame(country = c("nz","ar","fr","gb","es"))
codes$y <- runif(nrow(codes))
codes$url <- sapply(codes$country, sprintf, fmt="https://github.com/hjnilsson/country-flags/raw/master/png250px/%s.png")

gl <- Map(function(x, y) {
  destfile <- paste0(y, '.png')
  if(!file.exists(destfile))
  download.file(x, destfile = destfile)
  png::readPNG(destfile)
}, x = codes$url, y = codes$country)

codes$raster <- I(gl)

ggplot(codes, aes(x = country, y = y)) + 
  geom_point() +
  geom_custom(data = codes, aes(data=raster), 
              grob_fun = rasterGrob, 
              fun_params = list(height=unit(1,"cm")))

## ----customgrob----------------------------------------------------------
custom_grob <- function(data, x=0.5,y=0.5){
  grob(data=data,x=x,y=y, cl="custom")
}
preDrawDetails.custom <- function(x){
  pushViewport(viewport(x=x$x,y=x$y))
}
postDrawDetails.custom <- function(x){
  upViewport()
}
drawDetails.custom <- function(x, recording=FALSE, ...){
  grid.lines(x$data$x, x$data$y, gp=gpar(col=x$data$col,lwd=2), default.units = "native")
}


d <- data.frame(x=rep(1:3, 4), f=rep(letters[1:4], each=3))
gl <- replicate(4, data.frame(x=seq(0.4,0.6,length=10),
                              y = runif(10,0.45,0.55),
                              col = sample(palette())[1]), FALSE)
dummy <- data.frame(f=letters[1:4], data = I(gl))

ggplot(d, aes(f,x)) +
  facet_wrap(~f)+
  coord_polar() +
  theme_bw() +
  geom_point()+
  geom_custom(data = dummy, aes(data = data, x = f, y = 2), 
              grob_fun = custom_grob) +
  theme_minimal()

