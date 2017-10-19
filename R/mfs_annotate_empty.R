#' Place an annotation on the least busy corner of a plot
#'
#' This function returns a ggplot2 annotation grob for addition to an existing
#'     ggplot object. The annotation should have coordinates and text
#'     justification appropriate for placing it in the least crowded corner
#'     of the existing plot, based on x and y lists provided.
#' @param s A text string for use as an annotation
#' @param xs The list of x values used in the plot
#' @param ys The list of y values used in the plot
#' @param opp If True, indicates to put the annotation in the opposite corner of the least crowded
#' @param text_color The color of the text
#' @keywords ggplot2 annotate text
#' @export
#' @examples
#' ggplot2(...) + mfs_annotate_empty("a comment", data[,"y"], data[,"x"])


mfs_annotate_empty <- function(s, xs, ys, opp=FALSE, text_color="black") {
    
    # If this is a two-factor distribution, just center it between the values and don't bother counting
    if(length(levels(as.factor(ys))) == 2) {
        yval = if(is.numeric(ys)) mean(unique(ys)) else 1.5
        return( ggplot2::annotate("text", label=s, color=text_color, hjust=0.5, vjust=0.5, x=mean(xs), y=yval) )
    }

    # Determine how crowded corners of the plot will be
    x_thresholds <- c(
        min(xs),
        (max(xs)-min(xs))/3 + min(xs),
        (max(xs)-min(xs))/2 + min(xs),
        2*(max(xs)-min(xs))/3 + min(xs),
        max(xs)
    )
    y_thresholds <- c(
        min(ys),
        (max(ys)-min(ys))/5 + min(ys),
        (max(xs)-min(xs))/2 + min(xs),
        4*(max(ys)-min(ys))/5 + min(ys),
        max(ys)
    )
    point_density = matrix(c(
        length(xs[xs<x_thresholds[2] & ys>y_thresholds[4]]),
        length(xs[xs<x_thresholds[2] & ys<y_thresholds[2]]),
        length(xs[xs>x_thresholds[4] & ys>y_thresholds[4]]),
        length(xs[xs>x_thresholds[4] & ys<y_thresholds[2]])
    ), nrow=2, ncol=2)
    
    # Provide the annotation, justified and positioned according to crowdedness
    if( point_density[1, 1] == min(point_density) ) {
        if(opp) {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=1.0, vjust=0.0, x=x_thresholds[5], y=y_thresholds[1]))
        } else {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=0.0, vjust=1.0, x=x_thresholds[1], y=y_thresholds[5]))
        }
    } else if ( point_density[2, 1] == min(point_density) ) {
        if(opp) {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=1.0, vjust=1.0, x=x_thresholds[5], y=y_thresholds[5]))
        } else {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=0.0, vjust=0.0, x=x_thresholds[1], y=y_thresholds[1]))
        }
    } else if ( point_density[1, 2] == min(point_density) ) {
        if(opp) {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=0.0, vjust=0.0, x=x_thresholds[1], y=y_thresholds[1]))
        } else {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=1.0, vjust=1.0, x=x_thresholds[5], y=y_thresholds[5]))
        }
    } else if ( point_density[2, 2] == min(point_density) ) {
        if(opp) {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=0.0, vjust=1.0, x=x_thresholds[1], y=y_thresholds[5]))
        } else {
            return( ggplot2::annotate("text", label=s, color=text_color, hjust=1.0, vjust=0.0, x=x_thresholds[5], y=y_thresholds[1]))
        }
    }
    
    # This should never happen, but will "work" and give an obvious indication if I've overlooked something.
    return( ggplot2::annotate("text", label="No corner appears empty to my failed logic in mfs_annotate_empty",
        hjust=0.5, vjust=0.5, x=x_thresholds[3], y=y_thresholds[3]))
}
