#' A Lined Scatterplot Function
#'
#' This function uses ggplot2 to create a multi-scatterplot with a line of best fit
#'     and a loess smoother overlaid, based on two existing linear models. It is
#'     often discouraged in strong terms to do this, so this library requires that
#'     the x axes in both models are identical before proceeding. With identical x
#'     axes, I don't see the problem in viewing two dependent variables together.
#' @param model1 A generalized linear model, returned from glm()
#' @param model2 A generalized linear model, returned from glm()
#' @param xlab Optionally, provide a label for the x axis
#' @param y1lab Optionally, provide a label for the first y axis
#' @param y2lab Optionally, provide a label for the second y axis
#' @param title Optionally, provide a title to override the default
#' @param scheme1 override the default "bluish" colors for plotting model1
#' @param scheme2 override the default "reddish" colors for plotting model2
#' @keywords scatter loess plot glm
#' @export
#' @examples
#' mfs_scatter_from_models(glm(y~x,data=d), glm(z~x,data=d))
#' mfs_scatter_from_models(glm(y~x,data=d), glm(z~x,data=d), title="A great plot")
#' mfs_scatter_from_models(glm(y~x,data=d), glm(z~x,data=d), xlab="Independent variable")


mfs_scatter_from_models <- function(model1, model2, xlab="", y1lab="", y2lab="", title="", color_scheme_1="bluish", color_scheme_2="reddish") {
    use_colors_1 <- mfs.r.utils::mfs_color_scheme(color_scheme_1)
    use_colors_2 <- mfs.r.utils::mfs_color_scheme(color_scheme_2)
    
    # Ensure we have identical x-axes in each model
    if (length(model1$model[,2]) != length(model2$model[,2])) {
        s="Multiple y-axes are almost always a bad idea, but can be useful to plot two dependent variables against the same independent variable. "
        print(paste0(
            s,
            "In this case, independent axes are not even the same length between model1 and model2."
        ))
        return(NA)
    }
    if (range(model1$model[,2][1]) != range(model2$model[,2][1]) || 
        range(model1$model[,2][2]) != range(model2$model[,2][2])) {
        s="Multiple y-axes are almost always a bad idea, but can be useful to plot two dependent variables against the same independent variable. "
        print(paste0(
            s,
            "In this case, different ranges [(",
            range(model1$model[,2])[1],
            "-",
            range(model1$model[,2])[2],
            ") vs (",
            range(model2$model[,2])[1],
            "-",
            range(model2$model[,2])[2],
            ")] are covered by model1 and model2."
        ))
        return(NA)
    }
    
    # Find our bearings
    xmin <- min(min(model1$model[,2]), min(model2$model[,2]))
    xmax <- max(max(model1$model[,2]), max(model2$model[,2]))
    y1min <- min(model1$model[,1])
    y1max <- max(model1$model[,1])
    y2min <- min(model2$model[,1])
    y2max <- max(model2$model[,1])
    
    # Determine actual labels to use
    main_dep <- names(model1$model[1])
    secd_dep <- names(model2$model[1])
    main_ind <- names(model1$model[2]) # identical to model2, either would be the same
    if(names(model1$model[2]) != names(model2$model[2])) {
        main_ind <- paste(names(model1$model[2]), "or", names(model2$model[2]))
        # This indicates a problem that we may have different things on the x-axis. D'Oh!
    }
    usable_x_lab <- if(length(xlab)>0) xlab else main_ind
    usable_y1_lab <- if(length(y1lab)>0) y1lab else main_dep
    usable_y2_lab <- if(length(y2lab)>0) y2lab else secd_dep
    usable_title <- if(length(title)>0) title else paste(main_dep,"vs",main_ind)
    
    # Build the plot
    p <- ggplot2::ggplot(data=model1$data, ggplot2::aes_string(x=main_ind, y=main_dep))
    p <- p + ggplot2::geom_point(data=model1$data, ggplot2::aes_string(x=main_ind, y=main_dep),
                                 color=use_colors_1[3], shape=3)
    p <- p + ggplot2::geom_point(data=model2$data, ggplot2::aes_string(x=main_ind, y=secd_dep),
                                 color=use_colors_2[3], shape=4)
    p <- p + ggplot2::geom_smooth(data=model1$data, ggplot2::aes_string(x=main_ind, y=main_dep),
                                  method="loess", linetype="dotted", color=use_colors_1[2])
    p <- p + ggplot2::geom_smooth(data=model2$data, ggplot2::aes_string(x=main_ind, y=secd_dep),
                                  method="loess", linetype="dotted", color=use_colors_2[2])
    p <- p + ggplot2::geom_abline(intercept=coef(summary(model1))["(Intercept)","Estimate"],
                                  slope=coef(summary(model1))[main_ind,"Estimate"],
                                  color=use_colors_1[1])
    p <- p + ggplot2::geom_abline(intercept=coef(summary(model2))["(Intercept)","Estimate"],
                                  slope=coef(summary(model2))[main_ind,"Estimate"],
                                  color=use_colors_2[1])
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::ggtitle(usable_title)
    p <- p + mfs.r.utils::mfs_annotate_empty(s=mfs_glm_string(model1), xs=model1$model[,2], ys=model1$model[,1],
                                opp=FALSE, text_color=use_colors_1[1])
    p <- p + mfs.r.utils::mfs_annotate_empty(s=mfs_glm_string(model2), xs=model2$model[,2], ys=model2$model[,1],
                                opp=TRUE, text_color=use_colors_2[1])
    p <- p + ggplot2::theme(
        axis.text.y = ggplot2::element_text(color=use_colors_1[1]),
        axis.text.y.right = ggplot2::element_text(color=use_colors_2[1])
    )
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.))
    
    return(p)
}
