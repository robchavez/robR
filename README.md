# robR

My R package of useful (and useless) functions and first foray into GitHub. This is a work in progress and none of the functions are optimized, well-coded, or well maintained yet. I hope to change that practice soon and add more to come. 

The most useful function for most will be ggbar.maker(): a simple helper function for making quick and nice looking mean/error bar graphs in ggplot2 without much data aggregation.

To install, you will need the "devtools" package installed before downloading from GitHub.
```{r,warning=FALSE,message=FALSE}

# Requires devtools version 1.7.0 or greater
install.packages("devtools")
devtools::install_github("robchavez/robR")
```

