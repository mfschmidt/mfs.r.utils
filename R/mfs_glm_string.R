#' Return a concise string description of a linear model
#'
#' This function queries a generalized linear model and produces a string
#'     to succinctly quantify the results. Useful for annotating plots.
#' @param model A linear model, returned from glm()
#' @param s.term Optionally, specify which variable in your model you are reporting. It must match the model contents.
#' @param s.units Optionally, report units as a string, like "mm" or "lbs/in"
#' @param s.form a sprintf string, default "\%0.2f", to indicate the format of the numeric output
#' @param p.level If you require a p-threshold other than 0.05, set it here
#' @param do.p If you don't want p-value reported, set this to FALSE
#' @keywords string p-value confidence interval summary
#' @export
#' @examples
#' mfs_glm_string(glm(y~x,data=z), p.level=0.01)


mfs_glm_string <- function(model, s.term="", s.units="", s.form="%0.2f", p.level=0.05, do.p=TRUE) {
    
    # Prime the confint pump
    suppressMessages(confint(model))
    
    # Derive some substrings
    if(s.term=="") s.term = names(model$model[2])
    s.ci <- sprintf(" (%d%%CI: ", 100 * (1.00 - p.level))
    s.lower <- sprintf("%0.1f %%", 100 * p.level/2.0)
    s.upper <- sprintf("%0.1f %%", 100 * (1.00 - (p.level/2.0)))
    mag <- abs(model$coefficients[[s.term]])
    if(mag > 100.0) {
        s.form="%0.0f"
    } else if(mag > 10.0) {
        s.form="%0.1f"
    } else if(mag < 0.001) {
        s.form="%0.4f"
    } else if(mag < 0.01) {
        s.form="%0.3f"
    }
    if(do.p) {
        s.p <- paste0(" ", p.string(summary(model)$coefficients[s.term,4]))
    } else {
        s.p <- ""
    }
    
    # Prepare the quantitative data for presentation
    return(paste0(
        sprintf(s.form, model$coefficients[[s.term]]),
        s.units,
        s.ci,
        sprintf(s.form, confint(model)[s.term, s.lower]),
        ",",
        sprintf(s.form, confint(model)[s.term, s.upper]),
        ") ",
        s.p
    ))
}
