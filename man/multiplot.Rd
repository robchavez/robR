\name{multiplot}
\alias{multiplot}
\title{Multiple ggplots in single page.}
\description{Print multiple graphs from ggplot2 on one page.}
\usage{
multiplot(..., plotlist = NULL, file, cols = 1, layout = NULL)
}
\arguments{
  \item{\dots}{
 ggplot objects.
}
  \item{plotlist}{
Use a list of ggplots.
}
  \item{file}{
Use ggplots from file.
}
  \item{cols}{
Number of columns in multiplot.
}
  \item{layout}{
Order layout.
}
}
\details{ }
\value{ }
%% ...

\references{}
\author{Winston Chang}
\note{ See : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
}


\seealso{}
\examples{
x <- rnorm(100,100,15)
y <- sample(c("Group 1","Group 2"),100,TRUE)
p1 <- qplot(x)
p2 <- qplot(y)
multiplot(p1,p2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multiplot}
