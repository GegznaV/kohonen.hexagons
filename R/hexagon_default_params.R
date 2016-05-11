
#' [!] Calculate parameters for (regular) hexagon
#'
#' @param x
#' @param y
#' @param r
#' @param l
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
hexagon_default_params <- function(r= NULL,l= NULL,x = NULL,y= NULL){
    # If x,y, values grid exist

    # ...

    # Get default r and l values
    cond <- c(is.null(r), is.null(l))
    if (all(cond == TRUE)) {r = .5;l = r / cos(pi/6)}
    if ((cond[1] == FALSE & cond[2] == TRUE))  {l = r / cos(pi/6)}
    if ((cond[1] == TRUE  & cond[2]  == FALSE)){r = l * cos(pi/6)}

    return(list(r = r, l = l))
}
