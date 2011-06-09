\name{covariance}
\alias{covariance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Covariance Matrix}

\description{
The function allow to estimate de Covariance Matrix
}
\usage{
covariance(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ 

  \item{x}{
must be a matrix of the quality characteristics.
}
  \item{n}{
(subgroup size) an integer
}
}
\details{
The cases of individual observations the covariance matrix is estimated according to Holmes and Mergen(1993).

}

\references{
 Montgomery,Douglas C.(2005)
"Introduction to Statistical Quality Control",John Wiley & Sons,
5 edition.

}
\author{
Edgar Santos Fernandez
}

\examples{
# calculating the covariance matrix for 4 quality characteristics
x<-rbeta(400,1,2); x<-matrix(x,nrow=100)
b<-covariance(x,5);print(b)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
