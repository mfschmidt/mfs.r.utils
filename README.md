# mfs.r.utils

A collection of reusable R functions that keep me consistent, concise, and bug-free. These are created for my personal use, but are offered to anyone for any purpose.

## mfs_annotate_empty

    ggplot2(...) + mfs_annotate_empty("my comment", data[,"x"], data[,"y"])

To annotate a plot, created by ggplot2, you typically add a ggplot2::annotate() object to it. This function does that, but it determines the emptiest corner of the plot, based on the lists of x- and y-values provided, and places the annotation in the appropriate corner. This can save a lot of time if I want to kick out several plots, some with rising trends that leave the top-left and bottom-right corners bare, and some with declining trends that leave the top-right and bottom-left corners bare. I just annotate with this and let the function figure out where to stick my notes.

## mfs_scatter_from_model

    mfs_scatter_from_model(glm(y~x,data=d), xlab="Time")

I regularly generate a linear model, generate a string the reflects the slope of the line with confidence intervals, plot the points, plot the line, and add an annotation with the quantitative results. This is a nice workflow to view the relation between any two variables related to each observation in a data set. It can eat up a lot of lines of code, though, so this function just takes in the linear model and looks inside of it to get the list of dependent and independent variables, their names (which can be overridden for labeling axes), and the resultant slope from the model, complete with 95% confidence intervals. It will generate x, y, and title labels based on names in your data.frame. But each can be overridden by specifying xlab, ylab, or title in the parameters. An example of xlab is given here so the plot will specify the generic "Time" label rather than whatever the independent variable was named in the data.frame.

