#' [!] Calculate coordinates of pointy top (PT) hexagon verticles
#'
#' Calculate coordinates of each pointy top (PT) hexagon verticle
#' (A, B, C, D, E, F) in respect to center coordinates O(x,y).
#'
#' @inheritParams hexagonPT
#'
#' @return Data frame with coordinates x and y (rows) of 6 verticles as
#'  variables: A (top) ,           B (left upper),
#'             C (left lower),     D (bottom),
#'             E (right) lower and F (right upper).
#' @export
#'
#' @examples
#'
#' hexagonPT_verticles(1,1)
#'
#' hexagonPT_verticles(1,1, r = 1)
#'
#' hexagonPT_verticles(1,1, l = 1)
#'
#' @family SOM and hexagon related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
hexagonPT_verticles <- function(x, y, r = NULL, l = NULL){

    # Get/Adjust parameters
    param <- hexagon_default_params(r = r, l = l)
    r <- param$r
    l <- param$l

    # Define verticles
    A <- c(x,     y + l)
    B <- c(x - r, y + l/2)
    C <- c(x - r, y - l/2)
    D <- c(x,     y - l)
    E <- c(x + r, y - l/2)
    F <- c(x + r, y + l/2)

    # Make a dataframe
    verticle <- data.frame(A,B,C,D,E,F)
    rownames(verticle) <- c("x","y")

    return(verticle)
}

# Additional lines --------------------------------------------------------

# xVerticles <- c(  x,
#           x - r,
#           x - r,
#           x,
#           x + r,
#           x + r)
#
# yVerticles <- c(  y + l,
#           y + l/2,
#           y - l/2,
#           y - l,
#           y - l/2,
#           y + l/2)
