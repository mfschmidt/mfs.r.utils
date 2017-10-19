#' Return a string representing a p-value
#'
#' This function just stringifies a p-value as "p=#", but if p is very
#'     low, it reports "p<0.0001" instead.
#' @param pval A numeric p-value between 0.0 and 1.0
#' @keywords p-value string
#' @export
#' @examples
#' print(mfs_p_string(0.0035))


mfs_p_string <- function(pval) {
    if (pval <= 0.0001) {
        return("p<0.0001")
    } else {
        return(sprintf("p=%0.3f", pval))
    }
}

