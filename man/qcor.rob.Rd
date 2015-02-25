\name{qcor.rob}
\alias{qcor.rob}
\title{Quick Correlation}
\description{
Prints scatterplot with trend line, simple regression model summary, and Pearson R for correlations.
}
\usage{
qcor.rob(x, y)
}

\arguments{
  \item{x}{
X variable
}
  \item{y}{
Y variable
}
}
\details{
 
}
\value{ }
\references{n/a}
\author{
Robert S. Chavez
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ggplot2
}
\examples{
a <- rnorm(100,100,15)
b <- rnorm(100,90,15)
qcor.rob(a,b)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{qcor.rob}
