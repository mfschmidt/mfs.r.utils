#' A Layered Histogram Function
#'
#' This function uses ggplot2 to create a histogram with a background normal
#'     curve and a foreground density curve.
#' @param d A data.frame containing the column of interest
#' @param whichcol The name of the column of interest
#' @param xlab Optionally, provide a label for the x axis
#' @param title Optionally, provide a title to override the default
#' @param color_scheme Optionally, a color scheme for the plot, "blackish" "bluish" "reddish" "greenish"
#' @keywords histogram normality density distribution
#' @export
#' @examples
#' mfs_histo_density(d=my_dataframe, whichcol="colname")
#' mfs_histo_density(my_dataframe, "colname", title="Look, it's normal!")


mfs_histo_density <- function(d, whichcol, xlab="__", title="__", color_scheme="blackish") {
    use_colors <- mfs.r.utils::mfs_color_scheme(color_scheme)
    
    # Determine actual labels to use
    usable_x_lab <- if(xlab=="__") whichcol else xlab
    usable_title <- if(title=="__") paste("Distribution of", usable_x_lab) else title

    # Determine skewness and prepare it as an annotation
    s = moments::skewness(d[[whichcol]])
    hjust = if(s<0) 1.0 else 0.0
    xpos = mean(d[[whichcol]]) + (sd(d[[whichcol]]) / 2)
    s <- sprintf("skew = %0.2f", s)
    
    # Generate the histogram
    p <- ggplot2::ggplot(data=d, aes_string(whichcol)) +
        ggplot2::stat_function(
            col = use_colors[3],
            lwd = 2,
            fun=dnorm,
            args=list(
                mean = mean(d[[whichcol]]),
                sd = sd(d[[whichcol]])
            )
        ) +
        ggplot2::geom_histogram(
            aes(y=..density..),
            col=use_colors[2],
            fill=use_colors[3],
            bins=50
        ) +
        ggplot2::geom_density(col=use_colors[1]) +
        ggplot2::theme_bw()
    # We have to have a p first, to query it for its max y-axis, then add the annotation there.
    return(p + ggplot2::annotate("text", label=s, x=xpos, y=layer_scales(p)$y$range$range[2], hjust=hjust, vjust=1.0))
}
