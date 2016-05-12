#' [!] Calculate distances between neighbour SOM cells
#'
#' @param X object of class \code{kohonen} (from package \pkg{kohonen}).
#' @inheritParams  stats::dist
#' @param ... Further parameters to be passed to \code{\link[stats]{dist}}.
#'
#' @return Data frame with distances between neighbour cells. Data frame has
#'  these variables: \cr
#'  \code{Cell_ID1, Cell_ID2} ID codes of neigbour cells. IThese variables are
#'  sorted so that condition \code{Cell_ID1 > Cell_ID2} is always true. \cr
#'  \code{Distance} distance between indicated neighbours. \cr
#'  Other variables are coordinates between neigbourhood boundaries: bedinning,
#'  end and center points.
#'
#' @export
#'
#' @examples
#' library(kohonen)
#' data("Spectra2", package = "spHelper")
#'
#' som_model <- som(Spectra2$spc, grid = somgrid(7,7,"hexagonal"))
#' hex_neighbour_dist(som_model)  %>% head
#'
#'
#' @family SOM and hexagon related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'

# X <- som_model

hex_neighbour_dist <- function(X, method = "euclidean", ...){

    # =========================================================================
    # Find distances between neighbour cells
    # =========================================================================

    # [Modified code from package `kohonen`]  --------------------------------
    #  needs review and update for some types of soms (xyf, supersom)

    nhbrdist <- unit.distances(X$grid, X$toroidal)
    nhbrdist[nhbrdist > 1.05] <- NA

    for (i in 2:nrow(nhbrdist)) {
        for (j in 1:(i - 1)) {
            if (!is.na(nhbrdist[i,j]))
                nhbrdist[i,j] <- nhbrdist[j,i] <- dist(X$codes[c(i,j),], method = method, ...)

            # nhbrdist[i,j] <- nhbrdist[j,i] <- dist(X$codes[c(i,j),])
        }
    }

    # [Piece of new code] ---------------------------------------------------

    # Reshape
    neigh_dist <- reshape2::melt(nhbrdist)

    # Remove zero distances (assume distance from cell to the same cell)
    # and NA distances (non neighbour distances)
    neigh_dist <- neigh_dist[!(neigh_dist[,3] %in% c(NA,0)),]

    # For every row: shift values (cell IDs) of Var1 and Var2, if value
    # of Var1 is larger:
    FUN <- function(x) {x[1:3] <- if (x[1]>x[2]) x[c(2,1,3)] else x}
    neigh_dist <- apply(neigh_dist, 1, FUN) %>% t

    # Order rows and remove dublicates
    neigh_dist <- neigh_dist[order(neigh_dist[,1],neigh_dist[,2]),] # line may be removed
    neigh_dist <- neigh_dist[!duplicated(neigh_dist),]

    # Rename rows and colums
    neigh_dist <- data.frame(neigh_dist)
    names(neigh_dist) <- c("Cell_ID1", "Cell_ID2", "Distance")

    rownames(neigh_dist) <- NULL                               # line may be removed

    # =========================================================================
    # Indentify boundaries between neighbour cells and their coordinates
    # =========================================================================

    # `line_pos` are boundaries between heighbours

    line_pos <- hex_neighbourhood_line_pos(X$grid$pts[,"x"], X$grid$pts[,"y"]) %>%
        dplyr::mutate(cell,
                      line_centers = interaction((x1+x2)/2,
                                                 round((y1+y2)/2, 7),
                                                 sep = "; "))

    # ddd - temporary dataframe

    # Select data to identify cells that share boundaries
    ddd <- line_pos  %>% dplyr::select(cell, line_centers)  %>% tidyr::nest(cell)

    # Convert nested data frames to row-wise pasted vectors
    ddd$data %<>% lapply(FUN = function(x)paste(x[,1],collapse=", ")) %>% unlist

    # Split cell numbers to two variables
    ddd %<>%
        tidyr::separate(data, c("Cell_ID1", "Cell_ID2"),
                        sep =",",
                        extra = "merge",
                        fill = "left")

    # Merge and process dataframes
    line_pos <- base::merge(ddd, line_pos)   %>%
        dplyr::mutate(Cell_ID1 = as.numeric(Cell_ID1),
                      Cell_ID2 = as.numeric(Cell_ID2))  %>%
        dplyr::arrange(Cell_ID1, Cell_ID2) %>%
        dplyr::select(-c(cell))

    # Remove duplicated
    non_duplicate <- !duplicated(line_pos[,c("Cell_ID1", "Cell_ID2", "line_centers")])
    line_pos <- line_pos[non_duplicate,]

    # =========================================================================
    # Merge coordinates of boundaries and information about neighbour distances
    # =========================================================================
    neigh_dist <- base::merge(neigh_dist, line_pos, all=TRUE)
    neigh_dist$line_centers <- NULL
    rownames(neigh_dist)    <- NULL

    # Return result
    return(neigh_dist)
}


