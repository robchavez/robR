############################################################
############ Easy bargraph  maker for ggplot2  #############
ggbar.maker <- function (dv, groups, factor = NULL, id = NULL, groups_between=FALSE, palette="Set1"){
  require(ggplot2)
  require(RColorBrewer)
  if (is.null(id)){
    if (groups_between==TRUE){        
      warning("No within subject variable. Using between-subject SEM error bars")
    }
    print("Using between-subject SEM error bars")
    if (is.null(factor)) {
      mean <- as.numeric(tapply(dv, groups, mean, na.rm = TRUE))
      se <- as.numeric(tapply(dv, groups, se.rob, na.rm = TRUE))
      lev <- as.factor(groups)
      fact <- levels(lev)
      eb_h <- mean + se
      eb_l <- mean - se
      df <- data.frame(mean, se, fact, eb_h, eb_l)
      labx <- deparse(substitute(groups))
      laby <- deparse(substitute(dv))
      ggplot(df, aes(fact, mean, fill = fact)) +
      geom_bar(position = position_dodge(0.9),
               stat = "identity", colour = "black", size = 0.5) +
      theme(legend.position = "none") + scale_fill_brewer(palette = palette) +
      geom_errorbar(ymax = eb_h,
                    ymin = eb_l,
                    size = 0.5,
                    width = 0.3) +
      xlab(labx) + ylab(laby)
    }
    else {
      factor2 <- as.factor(factor)
      df <- aggregate(dv, by = list(groups, factor2), FUN = mean)
      df$se <- aggregate(dv, by = list(groups, factor2), FUN = se.rob)[[3]]
      df$eb_h <- as.numeric(df$x + df$se)
      df$eb_l <- as.numeric(df$x - df$se)
      labx <- deparse(substitute(groups))
      laby <- deparse(substitute(dv))
      ggplot(df, aes(Group.1, x, fill = Group.2)) +
      geom_bar(position = position_dodge(0.9),
               stat = "identity", size = 0.5) +
      geom_bar(position = position_dodge(0.9),
               stat = "identity", colour = "black", size = 0.5,
               show_guide = FALSE) +
      scale_fill_brewer(palette = palette) +
      geom_errorbar(aes(ymin = eb_l, ymax = eb_h),
                    size = 0.5,
                    width = 0.3,
                    position = position_dodge(0.9)) +
      xlab(labx) +
      ylab(laby) +
      guides(fill = guide_legend(title = deparse(substitute(factor))))
    }
  }
  else {
    if (is.null(factor)) {
      if(groups_between==TRUE){
        warning("No within subject variable. Using between-subject SEM error bars")
        mean <- as.numeric(tapply(dv, groups, mean, na.rm = TRUE))
        se <- as.numeric(tapply(dv, groups, se.rob, na.rm = TRUE))
        lev <- as.factor(groups)
        fact <- levels(lev)
        eb_h <- mean + se
        eb_l <- mean - se
        df <- data.frame(mean, se, fact, eb_h, eb_l)
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        ggplot(df, aes(fact, mean, fill = fact)) +
        geom_bar(position = position_dodge(0.9),
                 stat = "identity",
                 colour = "black",
                 size = 0.5) +
          theme(legend.position = "none") +
          scale_fill_brewer(palette = palette) +
          geom_errorbar(ymax = eb_h, ymin = eb_l, size = 0.5, width = 0.3) +
          xlab(labx) +
          ylab(laby)
      }
      else{
        print("Using within-subject 95% confidence interval error bars")
        require(plyr)
        xydf <- data.frame(dv,groups,id)
        dfwc <- summarySEwithin(data=xydf, measurevar = "dv", withinvars = "groups", idvar = "id")
        
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        
        ggplot(dfwc,aes(groups, dv, fill=groups)) +  
          geom_bar(position= position_dodge(), stat="identity",color="black", size= 0.5) +
          theme(legend.position = "none") + scale_fill_brewer(palette = palette) +
          geom_errorbar(aes(ymin=dv-ci, ymax=dv+ci),width=.3, size = .3, position=position_dodge(.9)) +
          xlab(labx) + ylab(laby)
      }
    }
    else {
      print("Using within-subject 95% confidence interval error bars")
      if(groups_between==FALSE){
        require(plyr)
        xydf <- data.frame(dv,groups,factor, id)
        dfwc <- summarySEwithin(data=xydf, measurevar = "dv", withinvars = c("groups", "factor"), idvar = "id")
        
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        lab_fac <- deparse(substitute(factor))
        
        ggplot(dfwc,aes(groups, dv, fill=factor)) +
          geom_bar(position = position_dodge(),
                   stat = "identity",
                   size = 0.5) +
          geom_bar(position = position_dodge(),
                   stat = "identity",
                   color = "black",
                   size = 0.5,
                   show_guide = FALSE) +
          scale_fill_brewer(palette = palette, name =lab_fac) +
          geom_errorbar(aes(ymin = dv-ci, ymax = dv + ci),
                        width=.3,
                        size = .3,
                        position=position_dodge(.9)) +
          xlab(labx) +
          ylab(laby)
      }
      else {
        require(plyr)
        xydf <- data.frame(dv,groups,factor, id)
        dfwc <- summarySEwithin(data=xydf, measurevar = "dv", withinvars = "factor", betweenvars = "groups", idvar = "id")
        
        labx <- deparse(substitute(groups))
        laby <- deparse(substitute(dv))
        lab_fac <- deparse(substitute(factor))
        
        ggplot(dfwc, aes(groups, dv, fill=factor)) +
          geom_bar(position = position_dodge(),
                   stat = "identity",
                   size = 0.5) +
          geom_bar(position= position_dodge(),
                   stat = "identity",
                   color = "black",
                   size = 0.5,
                   show_guide = FALSE) +
          scale_fill_brewer(palette = palette, name = lab_fac) +
          geom_errorbar(aes(ymin = dv-ci, ymax = dv+ci),
                        width =.3,
                        size = .3,
                        position = position_dodge(.9)) +
          xlab(labx) +
          ylab(laby)
      }
    }
  }
}

########################################################
### Good Simpsons episode random suggestion.
simpson <- function(){
  sea <- sample(3:10,1)
  ep <- sample(1:22,1)
  se<-paste("Season",sea,"Episode",ep)
  print(se)
}

########################################################
# Sobel test calculator
sobel.lovez <- function (pred, med, out, plab="", mlab="", olab="", sbeta=FALSE)
{
  require(QuantPsyc)
  
  NEWDAT <- data.frame(pred = pred, med = med, out = out)
  NEWDAT <- na.exclude(NEWDAT)
  model1 <- lm(out ~ pred, data = NEWDAT)
  model2 <- lm(out ~ pred + med, data = NEWDAT)
  model3 <- lm(med ~ pred, data = NEWDAT)
  model4 <- lm(out ~ med, data = NEWDAT)
  mod1.out <- summary(model1)$coef
  mod2.out <- summary(model2)$coef
  mod3.out <- summary(model3)$coef
  mod4.out <- summary(model4)$coef
  indir <- mod3.out[2, 1] * mod2.out[3, 1]
  effvar <- (mod3.out[2, 1])^2 * (mod2.out[3, 2])^2 
          + (mod2.out[3, 1])^2 * (mod3.out[2, 2])^2
  serr <- sqrt(effvar)
  zvalue = indir/serr
  pvalue = (1- pnorm(abs(zvalue))) * 2
  pvalue3 <- format(pvalue, digits=2)
  out <- list(Model.1.out.pred = mod1.out, 
              Model.2.out.pred.med = mod2.out,
              Model.3.med.pred = mod3.out,
              Model.4.med.out = mod4.out,
              Indirect.Effect = indir, 
              SE = serr,
              z.value = zvalue,
              p.value = pvalue,
              N = nrow(NEWDAT))
  
  #Unstandardized beta coefficients
  c <- summary(model1)$coefficients[2, 1]
  a <- summary(model3)$coefficients[2, 1]
  b <- summary(model2)$coefficients[3, 1]
  cp <- summary(model2)$coefficients[2, 1]
  
  #Standardized beta coefficients (using QuantPsyc's lm.beta function)
  cbeta <- lm.beta(model1)
  cbeta <- cbeta[c(1)]
  abeta <- lm.beta(model3)
  abeta <- abeta[c(1)]
  bbeta <- lm.beta(model2)
  bbeta <- bbeta[c(2)]
  cpbeta <- lm.beta(model2)
  cpbeta <- cpbeta[c(1)]
  
  #P-values for all regression models
  pvalue.c <- summary(model1)$coefficients[2,4]
  pvalue.a <- summary(model3)$coefficients[2,4]
  pvalue.b <- summary(model2)$coefficients[3,4]
  pvalue.cp <- summary(model2)$coefficients[2,4]
  
  if (pvalue.c <= 0.05 && pvalue.c >= 0.01) pvalue.c.sig = paste("*") else pvalue.c.sig = paste("")       
  if (pvalue.c <= 0.01 && pvalue.c >= 0.001) pvalue.c.sig = paste("**") else pvalue.c.sig = pvalue.c.sig
  if (pvalue.c <= 0.001) pvalue.c.sig = paste("***") else pvalue.c.sig = pvalue.c.sig
  
  if (pvalue.a <= 0.05 && pvalue.a >= 0.01) pvalue.a.sig = paste("*") else pvalue.a.sig = paste("")       
  if (pvalue.a <= 0.01 && pvalue.a >= 0.001) pvalue.a.sig = paste("**") else pvalue.a.sig = pvalue.a.sig
  if (pvalue.a <= 0.001) pvalue.a.sig = paste("***") else pvalue.a.sig = pvalue.a.sig
  
  if (pvalue.b <= 0.05 && pvalue.b >= 0.01) pvalue.b.sig = paste("*") else pvalue.b.sig = paste("")       
  if (pvalue.b <= 0.01 && pvalue.b >= 0.001) pvalue.b.sig = paste("**") else pvalue.b.sig = pvalue.b.sig
  if (pvalue.b <= 0.001) pvalue.b.sig = paste("***") else pvalue.b.sig = pvalue.b.sig
  
  if (pvalue.cp <= 0.05 && pvalue.cp >= 0.01) pvalue.cp.sig = paste("*") else pvalue.cp.sig = paste("")       
  if (pvalue.cp <= 0.01 && pvalue.cp >= 0.001) pvalue.cp.sig = paste("**") else pvalue.cp.sig = pvalue.cp.sig
  if (pvalue.cp <= 0.001) pvalue.cp.sig = paste("***") else pvalue.cp.sig = pvalue.cp.sig
  
  #Plotting mediation w/standardized betas (if sbeta=TRUE)
  
  if (sbeta == TRUE) {  
    
    plot(c(0, 100), c(0, 110), col = "white", ann = FALSE, tck = 0, 
         col.axis = "white")
    rect(c(10, 10, 70, 70, 40), c(10, 50, 10, 50, 80), 
         c(30, 30, 90, 90, 60), c(30, 70, 30, 70, 100))
    arrows(c(30, 30, 20, 60), c(20, 60, 70, 90), 
           c(70, 70, 40, 80), c(20, 60, 90, 70), length = 0.15)
    text(c(20, 20, 80, 80, 50), c(20, 60, 20, 60, 90), 
         c(as.character(plab), as.character(plab), as.character(olab), as.character(olab), as.character(mlab)), cex = 1)
    text(30, 80, paste("ß=", format(abeta, digits = 2, nsmall = 2), pvalue.a.sig), pos = 2, 
         cex = 1.3)
    text(71, 80, paste("ß=", format(bbeta, digits = 2, nsmall = 2), pvalue.b.sig), pos = 4, 
         cex = 1.3)
    text(50, 20, paste("ß=", format(cbeta, digits = 2, nsmall = 2), pvalue.c.sig), pos = 3, 
         cex = 1.3)
    text(50, 60, paste("ß=", format(cpbeta, digits = 2, nsmall = 2), pvalue.cp.sig), pos = 3, 
         cex = 1.3)
    text(50, 2, paste("Sobel z = ", format(zvalue, digits = 2, nsmall = 2), "; p = ", pvalue3), cex = 1.3)
    print(paste("Sobel z =", format(zvalue, digits = 3, nsmall = 2), ", p =", pvalue))  
    return(out)
  }
  
  else {
    
    #Plotting mediation with unstandardized betas
    
    plot(c(0, 100), c(0, 110), col = "white", ann = FALSE, tck = 0, 
         col.axis = "white")
    rect(c(10, 10, 70, 70, 40), c(10, 50, 10, 50, 80), 
         c(30, 30, 90, 90, 60), c(30, 70, 30, 70, 100))
    arrows(c(30, 30, 20, 60), c(20, 60, 70, 90), 
           c(70, 70, 40, 80), c(20, 60, 90, 70), length = 0.15)
    text(c(20, 20, 80, 80, 50), c(20, 60, 20, 60, 90), 
         c(as.character(plab), as.character(plab), as.character(olab), as.character(olab), as.character(mlab)), cex = 1)
    text(30, 80, paste("B=",format(a, digits = 2, nsmall = 2), pvalue.a.sig), pos = 2, 
         cex = 1.3)
    text(71, 80, paste("B=",format(b, digits = 2, nsmall = 2), pvalue.b.sig), pos = 4, 
         cex = 1.3)
    text(50, 20, paste("B=",format(c, digits = 2, nsmall = 2), pvalue.c.sig), pos = 3, 
         cex = 1.3)
    text(50, 60, paste("B=",format(cp, digits = 2, nsmall = 2), pvalue.cp.sig), pos = 3, 
         cex = 1.3)
    text(50, 2, paste("Sobel z = ", format(zvalue, digits = 2, nsmall = 2), "; p = ", pvalue3), cex = 1.3)
    print(paste("Sobel z =", format(zvalue, digits = 3, nsmall = 2), ", p =", pvalue))  
    return(out)
    
  }
  
}

#######################################################
##### Standard error calcualtion ####
se.rob <- function(x, na.rm = FALSE){
  if (na.rm) 
    x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}


#########################################################################
### ggplot2 multiplot ###
# From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#############################################################################################
# Prints scatterplot with trend line, simple regression model summary, and Pearson R for correlations.
qcor.rob <- function (x, y)
{
  # Quick correlation with plot
  library(ggplot2)
  labx <- deparse(substitute(x))
  laby <- deparse(substitute(y))
  NEWDAT <- data.frame(x=x, y=y)
  NEWDAT <- na.exclude(NEWDAT)
  model <- lm(y~x, data=NEWDAT)
  pl <- qplot(x,y, data=NEWDAT) + stat_smooth(method='lm') + xlab(labx) + ylab(laby) + theme_classic()
  modelout <- summary(model)
  corout <- cor(x,y,use="complete.obs")
  out <- list(modelout, corout, pl)
  print(out)
  
}

#################################################################
#### z-score calculator for vectors #####
zscore.rob <- function(x){
  (x-mean(x))/sd(x)
}


#################################################################
#### Helper function for between subject designs ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


######################################################################
#### Helper function for within subject designs ####
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

############################################################
## normalizing helper function
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
    
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

###########################################################
myjekyllsite = c("http://somethingsomething.com")
KnitJekyll <- function(input, base.url = myjekyllsite) {
  # tells knitr to play nice when compiling Jekyll posts
  # Args: 
  #   input: an .Rmd file
  #   base.url: the url for your jekyll site. Defaults to variable set above
  #             function.
  require(knitr)
  opts_knit$set(base.url = base.url)
  # set this figure path string as needed
  fig.path <- paste0("figure/", sub(".Rmd$", "", basename(input)), "/")
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")
  render_jekyll()
  knit(input, envir = parent.frame())
}
