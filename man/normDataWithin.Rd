\name{normDataWithin}
\alias{normDataWithin}
\title{Normalizing helper function for within-subjects data.}
\description{Norms the data within specified groups in a data frame; it normalizes each.}
\usage{
normDataWithi(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE)
}
\arguments{

  \item{data}{
A data frame.
}

  \item{idvar}{
The name of a column that identifies each subject (or matched subjects).
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
\keyword{ ~normDataWithin }

