\name{sobel.lovez}
\alias{sobel.lovez}
\title{Sobel Test Calculator}
\description{Performs mediation analysis, calculates Sobel test, and prints plots.}
\usage{
sobel.lovez(pred, med, out, plab = "", mlab = "", olab = "", sbeta = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pred}{
Predictor variable.
}
  \item{med}{
Mediator variable.
}
  \item{out}{
Outcome variable.
}
  \item{plab}{
Predictor label.
}
  \item{mlab}{
Mediator label.
}
  \item{olab}{
Outcome label.
}
  \item{sbeta}{
Use standardized betas.
}
}
\details{ }
\value{ }
\references{ }
\author{
Robert S. Chavez & Richard B. Lopez
}
\note{Requires QuantPsyc package}


\seealso{QuantPsyc, mediation}
\examples{
a <- rnorm(100,100,15)
b <- rnorm(100,90,15)
c <- rnorm(100,80,15)
sobel.lovez(a,b,c)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sobel.lovez}
\keyword{sobel}
