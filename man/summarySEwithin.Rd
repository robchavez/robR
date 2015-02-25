\name{summarySEwithin}
\alias{summarySEwithin}
\title{Summarize data for within-subjects designs.}
\description{Summarizes data, handling within-subjects variables by removing inter-subject variability. It will still work if there are no within-S variables. Gives count, un-normed mean, normed mean (with same between-group mean), standard deviation, standard error of the mean, and confidence interval. If there are within-subject variables, calculate adjusted values using method from Morey (2008).
}
\usage{
summarySEwithin(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE)
}

\arguments{
  \item{data}{
A data frame.
}

  \item{measurevar}{
The name of a column that contains the variable to be summariezed
}

  \item{betweenvars}{
A vector containing names of columns that are between-subjects variables.
}

  \item{withinvars}{
A vector containing names of columns that are within-subjects variables
}

  \item{idvar}{
The name of a column that identifies each subject (or matched subjects).
}

  \item{na.rm}{
A boolean that indicates whether to ignore NA's
}

  \item{conf.interval}{
The percent range of the confidence interval (default is 95%)
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{}
\references{}
\author{Winston Chang}
\note{See : http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/}


\seealso{}
\examples{
x <- rnorm(100,100,15)
y <- sample(c("Group 1","Group 2"),100,TRUE)
z <- sample(c("Low","Med","High"),100,TRUE)
subject <- rep(1:5,20)
df <- data.frame(x,y,z,subject) 

summarySEwithin(data=df, measurevar="x", betweenvars="y", withinvars="z", idvar="subject")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
