## ----Load packages, message=FALSE,warning=FALSE--------------------------
library(kohonen.hexagons)
library(kohonen)
library(pander)

## ----options1, echo = FALSE, message = FALSE, warning = FALSE------------
optDEF <- knitr::opts_chunk$get()
knitr::opts_chunk$set(comment = "#>",
                      fig.align = 'center')
op <- par(mar=c(1,1,1,1))

## ------------------------------------------------------------------------
plot.new()
hexagonPT(.5,.5, r = .3)

## ------------------------------------------------------------------------
 data <- class::somgrid(8,6,"hexagonal")$pts
 head(data)
 x <- data[,"x"]
 y <- data[,"y"]

 hexagonMap(x, y)

# Add color
 hexagonMap(x, y, col = "yellow")

# Add colors and text
 hexagonMap(x, y,
      col = c("tomato1","skyblue","orange"),
      border = c("red", "black"))

 text(x,y,1:length(x), cex = .6)
 title("Hexagonal cells \n with cell ID number")


## ----hexagonPT_verticles,  fig.show='hold'-------------------------------
hexagonPT_verticles(1,1)

## ----hexagonPT_verticles 2, echo = FALSE---------------------------------
verticles <- hexagonPT_verticles(1,1)  %>% t

plot(verticles, type = "n", main = "Names of polygon verticles")  # initialize a plot
polygon(verticles, border = "grey")
text(verticles,labels = LETTERS[1:6], col = "blue")


## ----hex_neighbourhood_line_pos------------------------------------------
hex_neighbourhood_line_pos(x, y)  %>% head

## ------------------------------------------------------------------------
library(kohonen)
data("Spectra2", package = "spHelper")

som_model <- som(Spectra2$spc, grid = somgrid(7,7,"hexagonal"))
hex_neighbour_dist(som_model)  %>% head

## ----hexagon_default_params----------------------------------------------
hexagon_default_params()      %>% pander

hexagon_default_params(r = 1) %>% pander

hexagon_default_params(l = 1) %>% pander

data <- class::somgrid(8,6,"hexagonal")$pts
hexagon_default_params(x = data[,"x"], y = data[,"y"]) %>% pander

hexagon_default_params(x = data[,"x"], y = data[,"y"]*2) %>% pander

hexagon_default_params(x = data[,"x"]*2, y = data[,"y"]) %>% pander

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

