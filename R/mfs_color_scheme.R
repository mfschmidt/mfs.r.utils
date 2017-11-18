#' A function that returns vectors of pre-selected color gradients
#'
#' This function simply returns canned vector of colors
#' @param color_scheme Optionally, a color scheme for the plot, "blackish" "bluish" "reddish" "greenish"
#' @keywords colors
#' @export
#' @examples
#' my_colors = mfs_color_scheme("bluish")


mfs_color_scheme <- function(color_scheme="blackish") {
    
    if(color_scheme == "bluish") {
        return(c("blue", "royalblue", "skyblue"))
    } else if(color_scheme == "reddish") {
        return(c("firebrick", "tomato", "pink"))
    } else if(color_scheme == "greenish") {
        return(c("darkgreen", "green", "lightgreen"))
    } else {
        return(c("black", "darkgrey", "lightgrey"))
    }
}
