#' [!] Neighbourhood line positions of hexagonal cells in map of hexagons
#'
#' @inheritParams hexagonMap
#' @param ... further parameters to methods.
#'
#' @export
#'
#' @return dataframe with variables \cr
#'       \code{Cell_ID} - ID number of SOM cell;\cr
#'       \code{x1, y1, x2, y2} - coordinates of starting and end points of
#'       neighbourhood boundary lines;\cr
#'       \code{x_center, y_center} - coordinates of neighbourhood
#'                                   boundary line centers.\cr
#'
#' @examples
#' #====================================================================
#'
#' data <- class::somgrid(8,6,"hexagonal")$pts
#' head(data)
#' x <- data[,"x"]
#' y <- data[,"y"]
#'
#'
#' hex_neighbourhood_line_pos(x, y)  %>% head
#'
#' #====================================================================
#'
#' @family SOM and hexagon related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
hex_neighbourhood_line_pos <- function(x, y, r = NULL, l = NULL, ...) {

    # Get/Adjust parameter values
    params <- hexagon_default_params(r,l,x,y)
    r <- params$r
    l <- params$l


    n <- length(x)

    x <- as.vector(x)
    y <- as.vector(y)

    # col    <- rep_len(col, n)
    # border <- rep_len(border, n)

    df <- data.frame()

    for (i in 1:n) {
        # Verticle coordinates
        v_pos <- hexagonPT_verticles(x[i],y[i],r=r,l=l)

        # Vector of verticle names ("closed corcle")
        verticles <- "v_pos$" %++% c("A","B","C","D","E","F","A")

        for(ii in 1:6){
            v <- verticles[ii:(ii+1)]
            eval_(sprintf("tmp <- data.frame(cell = i,"   %++%
                          " x1 = %s[1], y1 = %s[2],"      %++%
                          " x2 = %s[1], y2 = %s[2])",
                          v[1],v[1],v[2],v[2]))

            df <- rbind(df, tmp)
        }
    }

    # x_center, y_center - coordinates of neighbourhood line centers
    df <- dplyr::mutate(df, x_center = (x1+x2)/2, y_center = (y1+y2)/2)

    return(df)
}


# Additional lines --------------------------------------------------------

# if (is.null(r)) r <- x %>% diff  %>% abs  %>% min/2         # 0.5
#  if (is.null(l)) l <- y %>% diff  %>% abs  %>% max() - r/2   # 0.616
#
#
#  # if (add == FALSE) {
#  #     xlim <- range(x) + c(-1, +1) * r
#  #     ylim <- range(y) + c(-1, +1) * l
#  #
#  #     MASS::eqscplot(xlim, ylim, axes = FALSE,
#  #                    type = "n", xlab = "", ylab = "")
#  # }



# for (i in 1:n) {
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
# }


