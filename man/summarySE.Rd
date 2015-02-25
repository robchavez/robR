\name{summarySE}
\alias{summarySE}
\title{Summarize data for between-subjects designs.}
\description{Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
}
\usage{
summarySE(data=NULL, measurevar, groupvars=NULL,  na.rm=FALSE, conf.interval=.95, .drop=TRUE)
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
\details{}
\value{}
\references{}
\author{Winston Chang}
\note{See : http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/}


\seealso{}
\examples{
x <- rnorm(100,100,15)
y <- sample(c("Group 1","Group 2"),100,TRUE)
df <- data.frame(x,y) 

summarySE(data=df, measurevar="x", betweenvars="y")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summarySE}