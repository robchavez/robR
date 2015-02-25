\name{ggbar.maker}
\alias{ggbar.maker}
\title{Easy bar graph maker for ggplot2}
\description{Makes quick and easy bar graphs for ggplot2.}
\usage{ggbar.maker(dv, groups, factor = NULL, id = NULL, groups_between=FALSE, palette="Set1")}
\arguments{
  \item{dv}{
Dependent variable vector.
}
  \item{groups}{
Grouping or first factor variable.
}
  \item{factor}{
Second factor variable.
}

  \item{id}{
The name of a column that identifies each subject for within-subject designs.
}

  \item{groups_between}{
The 'groups' variable is a between subjects factor.
}
  \item{palette}{
Select RColorBrewer palette.
}


}
\details{ }
\value{ }
\references{ }
\author{Robert S. Chavez}
\note{You must manually adjust y-axis range using the ggplot2 function coord_cartesian().}
\seealso{}
\examples{

# Between subject design.
x <- rnorm(100,100,15)
y <- sample(c("Group 1","Group 2"),100,TRUE)
z <- sample(c("Low","Med","High"),100,TRUE)

# Only one factor.
ggbar.maker(x,y)  

# Two factors.
ggbar.maker(x,y,z) 



# Within subject design 
x <- rnorm(100,100,15)
y <- sample(c("Group 1","Group 2"),100,TRUE)
z <- sample(c("Low","Med","High"),100,TRUE)
subject <- rep(1:5,20)

# No between subjects factors.
ggbar.maker(x,y,z, id =subject) 

# Grouping factor is between subjects.
ggbar.maker(x,y,z, id =subject, groups_between=TRUE)

}
\keyword{ggbar.maker}