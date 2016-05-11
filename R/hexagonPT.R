#' @rdname hexagonMap
#' @export
#'
hexagonPT <- function(x, y, col = NA, border="black", r = .5, l = r/cos(pi/6), ...) {
    # Coordinates of vertices
    coords <- hexagonPT_verticles(x,y,r,l)

    # print(data.frame(x0, y0))
    graphics::polygon(coords["x",],  coords["y",],
            col    = col,
            border = border,
            ...)
}

