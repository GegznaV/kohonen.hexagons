#'
#' [!] Calculate parameters for (regular) hexagon
#'
#'
#' Function calculates parameters \code{l} and \code{r} for hexagon.
#'
#' @details
#' Function assumes regular pointy topped hexagon. (More on geometry of hexagons
#'  can be found \href{http://www.redblobgames.com/grids/hexagons/}{here}).
#' Relation between parameters \code{l} and \code{r} in this kind of hexagon: \cr
#' \code{l = r / cos(pi/6)} \cr
#' \code{r = l * cos(pi/6)} \cr \cr
#'
#' These proportions can be distorted to mach grid of x and y values so that
#' hexahons vere plotted without gaps.
#'
#' @param x,y numeric vectors with hexagon center coordinates in hexagon grid.
#' @param r radius of circle inscribed in hexagon. Default value is \code{0.5}.
#' @param l distance from the center to the upper/lower vertices of hexagon.
#'
#' @return List with calculated values of parameters \code{r} and \code{l}.
#' @export
#'
#' @examples
#'
#' hexagon_default_params()
#'
#' hexagon_default_params(r = 1)
#'
#' hexagon_default_params(l = 1)
#'
#' data <- class::somgrid(8,6,"hexagonal")$pts
#' hexagon_default_params(x = data[,"x"], y = data[,"y"])
#'
#' hexagon_default_params(x = data[,"x"], y = data[,"y"]*2)
#'
#' hexagon_default_params(x = data[,"x"]*2, y = data[,"y"])
#'
#' @author Vilmantas Gegzna
#'


hexagon_default_params <- function(r = NULL, l = NULL, x = NULL, y = NULL){
    # Default values r = 0.5, l = 0.616

    # If x values exist
    if (!is.null(x)) {
        if (length(x) <= 1) stop("Number of elements in `x` must be more than 1.")
        r <- x %>% diff  %>% abs  %>% min/2
    }

    # Get default r and l values
    cond <- c(is.null(r), is.null(l))
    if (all(cond == TRUE)) {r = .5;l = r / cos(pi/6)}
    if ((cond[1] == FALSE & cond[2] == TRUE))  {l = r / cos(pi/6)}
    if ((cond[1] == TRUE  & cond[2]  == FALSE)){r = l * cos(pi/6)}

    # If y values exist
    if (!is.null(y)) {
        if (length(y) <= 1) stop("Number of elements in `y` must be more than 1.")

        # l <- y %>% diff %>% abs %>% max() * 2/3

        # ATTENTION!!! Might be problem in this case when `r` is large, as it depends on `r`:
        l <- y %>% diff  %>% abs  %>% max() - r/sqrt(3)
    }

    # Return result
    return(list(r = r, l = l))
}
