#' Plot a pointy toped hexagon and a map of hexagons
#'
#' \code{hexagonPT} plots a pointy topped hexagon. \cr
#' \code{hexagonMap} plots a map of hexagons.
#'
#'
#' @param x,y hexagon center coordinates.
#' @param r radius of circle inscribed in hexagon. Default value is 0.5.
#' @param l distance from the center to the upper/lower vertices of hexagon.
#' @param add logical. If \code{FALSE}, a new plot is drawn.
#'                     If \code{TRUE}, plot is drawn on a current graph.
#'
#' @inheritParams  graphics::polygon
#'
#' @return Plotted hexagon
#' @export
#'
#' @examples
#'
#' data <- class::somgrid(8,6,"hexagonal")$pts
#' head(data)
#' x <- data[,"x"]
#' y <- data[,"y"]
#'
#' hexagonMap(x, y)
#'
#'
#' hexagonMap(x, y, col = "yellow")
#'
#'
#' hexagonMap(x, y,
#'      col = c("tomato1","skyblue","orange"),
#'      border = c("red", "black"))
#'
#' text(x,y,1:length(x))
#' title("Hexagonal cells \n with cell ID number")
#'
#'
#' #====================================================================
#' plot.new()
#' hexagonPT(.5,.5, r = .25)
#'
#' #====================================================================
#'
#' @author Vilmantas Gegzna
#'
hexagonMap <- function(x, y,  col = NA, border="black",
                            r = NULL, l = NULL,
                            add = FALSE, ...) {

    # Get/Adjust parameter values
    params <- hexagon_default_params(r,l,x,y)
    r <- params$r
    l <- params$l

    x <- as.vector(x)
    y <- as.vector(y)

    n <- length(x) # Number of cells

    col    <- rep_len(col, n)
    border <- rep_len(border, n)

    # Initialize a new plot
    if (add == FALSE) {
        xlim <- range(x) + c(-1, +1) * r
        ylim <- range(y) + c(-1, +1) * l

        MASS::eqscplot(xlim, ylim, axes = FALSE,
                       type = "n", xlab = "", ylab = "")
    }

    # Plot Hexagons
    for (i in 1:n) {
        hexagonPT(x[i], y[i],
                  col = col[i],
                  border = border[i],
                  r = r,
                  l = l)
    }
}


# Additional lines --------------------------------------------------------


        #     A <- c(x[i],     y[i] + l)
        #     B <- c(x[i] - r, y[i] + r/2)
        #     C <- c(x[i] - r, y[i] - r/2)
        #     D <- c(x[i],     y[i] - l)
        #     E <- c(x[i] + r, y[i] - r/2)
        #     G <- c(x[i] + r, y[i] + r/2)
        #
        #     verticles <- c("A","B","C","D","E","G","A")
        #
        #     for(i in 1:6){
        #         v <- verticles[i:(i+1)]
        #         eval_(sprintf("segments(%s[1], %s[2], %s[1], %s[2], lwd = 5, col = i)",
        #                       v[1],v[1],v[2],v[2]))
        #         points(v[1])
        #
        #         for(u in 1:2)    eval_(sprintf("points(%s[1],%s[2], col = 'black', pch = 20)",
        #                                        v[u],v[u]))
        #     }
