#' Plot a pointy toped hexagon and a map of hexagons
#'
#'
#' @details
#' Relation between parameters \code{l} and \code{r}: \cr
#' \code{l = r / cos(pi/6)} \cr
#' \code{r = l * cos(pi/6)} \cr
#'
#'
#' @param x,y hexagon center coordinates.
#' @param r radius of inscribed circle.
#' @param l distance from the center to the upper/lower vertices of hexagon.
#' @inheritParams  graphics::polygon
#'
#' @return Ploted hexagon
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
#' hexagonPT(1,1)
#'
#' #====================================================================
#'
#' @family SOM and hexagon related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
hexagonMap <- function(x, y,  col = NA, border="black",
                            r = NULL, l = NULL,
                            add = FALSE, ...) {

    if (is.null(r)) r <- x %>% diff  %>% abs  %>% min/2         # 0.5
    if (is.null(l)) l <- y %>% diff  %>% abs  %>% max() - r/sqrt(3)   # 0.616

    # Initialize a new plot
    if (add == FALSE) {
        xlim <- range(x) + c(-1, +1) * r
        ylim <- range(y) + c(-1, +1) * l

        MASS::eqscplot(xlim, ylim, axes = FALSE,
                       type = "n", xlab = "", ylab = "")
    }

    n <- length(x) # Number of cells

    x <- as.vector(x)
    y <- as.vector(y)

    col    <- rep_len(col, n)
    border <- rep_len(border, n)

    for (i in 1:n) {
        hexagonPT(x[i], y[i],
                  col = col[i], border = border[i],
                  r = r, l = l)
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
