# ggsurvey: A Package to Visualize Survey Data using ggplot2
This package is presently in early development. Functions are not fully documented but are functional. More examples and a vignette are coming. I appreciate any feedback.

This package builds upon ggplot2 but is designed for survey data with a goal of taking less code than using base ggplot2 by having a series of shortcut functions.  The base functions support data.frames as input and a class of functions for svy.design objects. All functions have an option for weights, if you do not have weights use ggplot2.  The following geoms will be supported: geom_bar, geom_histogram, 
geom_boxplot, geom_hex.  Options for 2d and 3d faceting exist for all functions. The geom_bar functions are design to make bar charts of histograms in crosstab style. The goal of this package is to simplify the syntax used to make plots of survey data 

This package is currently located on github.  Run the following code to install. 
``` r
# install.packages("devtools")  #if devtools is not installed on your machine
devtools::install_github("balexanderstats/ggsurvey")
```

Known issues: 
1. Import of required pacakges: ggplot2, survey, hexbin are not automatic.
