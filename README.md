# mfs.r.utils

A collection of reusable R functions that keep me consistent, concise, and bug-free. These are created for my personal use, but are offered to anyone for any purpose. All come with documentation so you can see available options in R using the help utilities.

## mfs_p_string

    print(mfs_p_string(0.035))
    [1] "p=0.035"
    
    print(mfs_p_string(0.000000000000035))
    [1] "p<0.0001"
    
If you need to report p-values for publication, this simple function always gives you a string you can use. It's helpful in automating reports of model outcomes that can be pasted elsewhere or annotated onto a plot.

## mfs_glm_string

    print(mfs_glm_string(glm(y~x+z, data=d))
    [1] "0.12 (95%CI: 0.05,0.20) p<0.0001"
    
    print(mfs_glm_string(glm(y~x+z, data=d), s.units="mm", p.level=0.01, s.term="%0.1f")
    [1] "0.1mm (99%CI: 0.0,0.3) p<0.0001"
   
Create a string with the most relevant numerics from a glm. You can drop this right into the results section of a paper. It will intelligently expand decimal places for very small numbers, or accept an sprintf specification (s.term="%0.4f") to override it. You can omit the p-value with (do.p=FALSE) or set a different confidence interval with (p.level=0.01). If you want units reported, just say so (s.units="mm").

## mfs_annotate_empty

    ggplot2(...) + mfs_annotate_empty("my comment", data[,"x"], data[,"y"])

To annotate a plot, created by ggplot2, you typically add a ggplot2::annotate() or ggplot2::geom_text() object to it. This function does that, but it determines whether the dependent variable is numeric first, putting the annotation in the center for binomial variables like TRUE/FALSE, or in the emptiest corner of the plot for numeric data, based on the lists of x- and y-values provided. This can save a lot of time if I want to kick out several plots, some with rising trends that leave the top-left and bottom-right corners bare, and some with declining trends that leave the top-right and bottom-left corners bare. I just annotate with this and let the function figure out where to stick my notes.

If I'm putting two annotations on a plot, I don't want to put them on top of each other, so I specify opp=TRUE for one of them and it will annotate in the corner opposite the emptiest, usually the next best when plotting linear relationships.

## mfs_scatter_from_model

    mfs_scatter_from_model(glm(y~x,data=d), xlab="Time")

I regularly generate a linear model, generate a string the reflects the slope of the line with confidence intervals, plot the points, plot the line, and add an annotation with the quantitative results. This is a nice workflow to view the relation between any two variables related to each observation in a data set. It can eat up a lot of lines of code, though, so this function just takes in the linear model and looks inside of it to get the list of dependent and independent variables, their names (which can be overridden for labeling axes), and the resultant slope from the model, complete with 95% confidence intervals. It will generate x, y, and title labels based on names in your data.frame. But each can be overridden by specifying xlab, ylab, or title in the parameters. An example of xlab is given here so the plot will specify the generic "Time" label rather than whatever the independent variable was named in the data.frame. With complex multi-variable models, this function will not fail, but will only plot the first dependent variable, ignoring adjusters and interaction terms.

## mfs_scatter_from_models

    mfs_scatter_from_models(glm(y~x,data=d), glm(y~x,data=e), xlab="Time")

Plot two models over each other. This is generally discouraged, but can be helpful to compare multiple y-values to a single independent variable. This function will only plot if the x-vectors in each model are identical. So you can plot whatever two variables you like against the same x without doing the data melt and aes(factor) dance, although there's nothing wrong with that approach. For now, model 1 is plotted in shades of blue and model 2 in red. Customized color scheming is on the list for future improvements.

## mfs_scatter_from_multimodel

    mfs_scatter_from_multimodel(glm(y~x*z,data=d), xlab="Time")

Try this one out, but it's the most finicky, and the least tested function in the package. When all goes well, it will plot multiple lines with multiple annotations specifying the relationship of y~x and its variation over changing z. Points plotted are raw, but dotted lines over the top of the plot represent the full model, including interaction.

