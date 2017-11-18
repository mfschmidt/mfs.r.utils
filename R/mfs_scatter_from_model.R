#' A Lined Scatterplot Function
#'
#' This function uses ggplot2 to create a scatterplot with a line of best fit
#'     and a loess smoother overlaid, based on an existing generalized linear model.
#' @param model A linear model, returned from glm() or lm()
#' @param xlab Optionally, provide an alternative label for the x axis
#' @param ylab Optionally, provide an alternative label for the y axis
#' @param title Optionally, provide a title to override the default
#' @param color_scheme Optionally, a color scheme for the plot, "blackish" "bluish" "reddish" "greenish"
#' @keywords scatter loess plot glm
#' @export
#' @examples
#' mfs_scatter_from_model(glm(y~x,data=z))
#' mfs_scatter_from_model(glm(y~x,data=z), title="A great plot")
#' mfs_scatter_from_model(glm(y~x,data=z), xlab="Independent variable")


mfs_scatter_from_model <- function(model, xlab="__", ylab="__", title="__", color_scheme="blackish") {
    use_colors <- mfs.r.utils::mfs_color_scheme(color_scheme)
    
    # Find our bearings
    xmin <- min(model$model[,2])
    xmax <- max(model$model[,2])
    ymin <- min(model$model[,1])
    ymax <- max(model$model[,1])
    
    # Determine actual labels to use
    main_dep <- names(model$model)[1]
    main_ind <- names(model$model)[2]
    usable_x_lab <- if(xlab=="__") main_ind else xlab
    usable_y_lab <- if(ylab=="__") main_dep else ylab
    usable_title <- if(title=="__") paste(usable_y_lab, "vs", usable_x_lab) else title
    # Diagnostics, if necessary
    # print(paste0("Given ", title, "(", nchar(title), "), ", xlab, "(", nchar(xlab), "), ", ylab, "(", nchar(ylab), ")"))
    # print(paste0("\"", main_dep, " vs ", main_ind, "\" or the final \"", usable_title, "\""), quote=FALSE)
    
    # Build the plot
    p <- ggplot2::ggplot(
        data=model$model,
        ggplot2::aes_string(x=main_ind, y=main_dep)
    )
    p <- p + ggplot2::geom_point(color=use_colors[3], shape=3)
    p <- p + ggplot2::geom_smooth(
        method="loess",
        linetype="dotted",
        color=use_colors[2])
    p <- p + ggplot2::geom_abline(
        intercept=coef(summary(model))["(Intercept)","Estimate"],
        slope=coef(summary(model))[main_ind,"Estimate"],
        color=use_colors[1])
    p <- p + ggplot2::ggtitle(usable_title)
    p <- p + ggplot2::labs(x=usable_x_lab, y=usable_y_lab)
    p <- p + ggplot2::theme_bw()
    p <- p + mfs.r.utils::mfs_annotate_empty(
        s=mfs_glm_string(model),
        xs=model$model[,2],
        ys=model$model[,1],
        text_color=use_colors[1]
    )
    
    return(p)
}
