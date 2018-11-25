geom_bar <- function(mapping = NULL, data = NULL,
                     stat = "count", position = "stack",
                     ...,
                     width = NULL,
                     binwidth = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
    
    if (!is.null(binwidth)) {
        warning("`geom_bar()` no longer has a `binwidth` parameter. ",
                "Please use `geom_histogram()` instead.", call. = "FALSE")
        return(geom_histogram(mapping = mapping, data = data,
                              position = position, width = width, binwidth = binwidth, ...,
                              na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
    }
    
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomBar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            width = width,
            na.rm = na.rm,
            ...
        )
    )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.r
GeomBar <- ggproto("GeomBar", GeomRect,
                   required_aes = c("x", "y"),
                   
                   # These aes columns are created by setup_data(). They need to be listed here so
                   # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                   # limits, not just those for which x and y are outside the limits
                   non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                   
                   setup_data = function(data, params) {
                       data$width <- data$width %||%
                           params$width %||% (resolution(data$x, FALSE) * 0.5)
                       transform(data,
                                 ymin = pmin(y, 0), ymax = pmax(y, 0),
                                 xmin = x - width / 2, xmax = x + width / 2, width = 0.5
                       )
                   },
                   
                   draw_panel = function(self, data, panel_params, coord, width = 0.5) {
                       # Hack to ensure that width is detected as a parameter
                       ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
                   }
)



geom_col <- function(mapping = NULL, data = NULL,
                     position = "stack",
                     ...,
                     width = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
    
    layer(
        data = data,
        mapping = mapping,
        stat = "identity",
        geom = GeomCol,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            width = width,
            na.rm = na.rm,
            ...
        )
    )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.r
GeomCol <- ggproto("GeomCol", GeomRect,
                   required_aes = c("x", "y"),
                   
                   # These aes columns are created by setup_data(). They need to be listed here so
                   # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                   # limits, not just those for which x and y are outside the limits
                   non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                   
                   setup_data = function(data, params) {
                       data$width <- data$width %||%
                           params$width %||% (resolution(data$x, FALSE) * 0.5)
                       transform(data,
                                 ymin = pmin(y, 0), ymax = pmax(y, 0),
                                 xmin = x - width / 2, xmax = x + width / 2, width = 0.5
                       )
                   },
                   
                   draw_panel = function(self, data, panel_params, coord, width = 0.5) {
                       # Hack to ensure that width is detected as a parameter
                       ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
                   }
)

