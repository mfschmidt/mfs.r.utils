#' A Lined Scatterplot Function
#'
#' This function uses ggplot2 to create a scatterplot with a line of best fit
#'     and a loess smoother overlaid, based on an existing linear model.
#' @param model A linear model, returned from lm() or glm()
#' @param xlab Optionally, provide a label for the x axis
#' @param ylab Optionally, provide a label for the y axis
#' @param title Optionally, provide a title to override the default
#' @keywords scatter loess plot lm glm
#' @export
#' @examples
#' mfs_scatter_from_model(glm(y~x,data=z))
#' mfs_scatter_from_model(glm(y~x,data=z), title="A great plot")
#' mfs_scatter_from_model(glm(y~x,data=z), xlab="Independent variable")


mfs_scatter_from_model <- function(model, xlab="", ylab="", title="") {
    
    # Find our bearings
    xmin <- min(model$model[,2])
    xmax <- max(model$model[,2])
    ymin <- min(model$model[,1])
    ymax <- max(model$model[,1])
    
    # Determine actual labels to use
    main_dep <- names(model$model[1])
    main_ind <- names(model$model[2])
    usable_x_lab <- if(length(xlab)>0) xlab else main_ind
    usable_y_lab <- if(length(ylab)>0) ylab else main_dep
    usable_title <- if(length(title)>0) title else paste(main_dep,"vs",main_ind)
    
    # Prepare the quantitative data for presentation
    suppressMessages(confint(model))
    quant_string <- sprintf("%0.2f (%0.2f,%0.2f)",
                            coef(summary(model))[main_ind,"Estimate"],
                            confint(profile(model), level = 0.95)[main_ind,"2.5 %"],
                            confint(profile(model), level = 0.95)[main_ind,"97.5 %"] )
    
    # Build the plot
    p <- ggplot2::ggplot(data=model$data, aes_string(x=main_ind, y=main_dep))
    p <- p + ggplot2::geom_point()
    p <- p + ggplot2::geom_smooth(method="loess")
    p <- p + ggplot2::ggtitle(usable_title)
    p <- p + ggplot2::theme_bw()
    p <- p + mfs_annotate_empty(s=quant_string, xs=model$model[,2], ys=model$model[,1])
    
    return(p)
}
