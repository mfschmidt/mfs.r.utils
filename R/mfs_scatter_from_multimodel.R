#' A Lined Scatterplot Function
#'
#' This function uses ggplot2 to create a scatterplot with a line of best fit
#'     and a loess smoother overlaid, based on an existing linear model.
#' @param model A linear model, returned from glm()
#' @param xlab Optionally, provide a label for the x axis
#' @param ylab Optionally, provide a label for the y axis
#' @param title Optionally, provide a title to override the default
#' @param color_scheme Optionally, a color scheme for the plot, "blackish" "bluish" "reddish" "greenish"
#' @keywords scatter loess plot glm
#' @export
#' @examples
#' mfs_scatter_from_model(glm(y~x,data=z))
#' mfs_scatter_from_model(glm(y~x,data=z), title="A great plot")
#' mfs_scatter_from_model(glm(y~x,data=z), xlab="Independent variable")


mfs_scatter_from_multimodel <- function(model, xlab="__", ylab="__", title="__", color_scheme="blackish") {
    
    # Find our bearings
    xmin <- min(model$model[,2])
    xmax <- max(model$model[,2])
    ymin <- min(model$model[,1])
    ymax <- max(model$model[,1])
    
    # Determine actual labels to use
    main_dep <- names(model$model[1])
    main_ind <- names(model$model[2])
    secd_ind <- names(model$model[3])
    usable_x_lab <- if(xlab=="__") main_ind else xlab
    usable_y_lab <- if(ylab=="__") main_dep else ylab
    usable_title <- if(title=="__") paste(usable_y_lab, "vs", usable_x_lab) else title
    # Diagnostics, if necessary
    # print(paste0("Given ", title, "(", nchar(title), "), ", xlab, "(", nchar(xlab), "), ", ylab, "(", nchar(ylab), ")"))
    # print(paste0("\"", main_dep, " vs ", main_ind, "\" or the final \"", usable_title, "\""), quote=FALSE)
    
    # Color schemes for multiple shades of a single hue
    color_schemes <- list()
    color_schemes[["blackish"]] <- c("black", "darkgrey", "lightgrey")
    color_schemes[["bluish"]] <- c("blue", "royalblue", "skyblue")
    color_schemes[["reddish"]] <- c("firebrick", "tomato", "pink")
    color_schemes[["greenish"]] <- c("darkgreen", "green", "lightgreen")
    
    # Build the plot
    p <- ggplot2::ggplot(data=model$data, ggplot2::aes_string(x=main_ind, y=main_dep))
    p <- p + ggplot2::geom_point(color=color_schemes[[color_scheme]][3], shape=3)
    p <- p + ggplot2::geom_smooth(method="loess", linetype="dotted", color=color_schemes[[color_scheme]][2])
    # draw the raw line of ONLY main independent variable
    main_slope = coef(summary(model))[main_ind,"Estimate"]
    added_slope = coef(summary(model))[paste0(main_ind,":",secd_ind,levels(model$model[[secd_ind]])[2]),"Estimate"]
    main_intercept = coef(summary(model))["(Intercept)","Estimate"]
    p <- p + ggplot2::geom_abline(intercept=main_intercept,
                                  slope=main_slope,
                                  color=color_schemes[[color_scheme]][2],
                                  linetype="solid")
    # draw the adjusted lines of main independent variable as secondary variable changes
    p <- p + ggplot2::geom_abline(intercept=main_intercept,
                                  slope=main_slope + added_slope * min(as.numeric(model$model[[secd_ind]])),
                                  color=color_schemes[[color_scheme]][1],
                                  linetype="dotted")
    p <- p + ggplot2::geom_abline(intercept=main_intercept,
                                  slope=main_slope + added_slope * mean(as.numeric(model$model[[secd_ind]])),
                                  color=color_schemes[[color_scheme]][1],
                                  linetype="dotted")
    p <- p + ggplot2::geom_abline(intercept=main_intercept,
                                  slope=main_slope + added_slope * max(as.numeric(model$model[[secd_ind]])),
                                  color=color_schemes[[color_scheme]][1],
                                  linetype="dotted")
    # annotate the lines for min and max
    if(main_slope + added_slope * min(as.numeric(model$model[[secd_ind]])) < main_slope + added_slope * max(as.numeric(model$model[[secd_ind]])) ) {
        # increasing secd_ind increases slope
        min_slope = main_slope + added_slope * min(as.numeric(model$model[[secd_ind]]))
        max_slope = main_slope + added_slope * max(as.numeric(model$model[[secd_ind]]))
        min_y = main_intercept + min_slope * min(as.numeric(model$model[[main_ind]]))
        max_y = main_intercept + max_slope * max(as.numeric(model$model[[main_ind]]))
        p <- p + ggplot2::annotate("text", label=paste("min",secd_ind,sprintf("%0.2f",min_slope)),
                                   x=min(as.numeric(model$model[[main_ind]])), y=min_y,
                                   hjust = 0.0, vjust = 0.0)
        p <- p + ggplot2::annotate("text", label=paste("max",secd_ind,sprintf("%0.2f",max_slope)),
                                   x=max(as.numeric(model$model[[main_ind]])), y=max_y,
                                   hjust = 1.0, vjust = 0.0)
    }
    p <- p + ggplot2::ggtitle(usable_title)
    p <- p + ggplot2::labs(x=usable_x_lab, y=usable_y_lab)
    p <- p + ggplot2::theme_bw()
    p <- p + mfs_annotate_empty(s=mfs_glm_string(model), xs=model$model[,2], ys=model$model[,1],
                                text_color=color_schemes[[color_scheme]][1])
    
    return(p)
}
