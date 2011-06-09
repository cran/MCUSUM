\name{mcusum}
\alias{mcusum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Cumulative Sum (MCUSUM) Control Chart
}
\description{
mcusum function perform the MCUSUM Control Chart according to Crosier(1988).
Allow to monitor simultaneously multiple quality characteristics.
MCUSUM is an extension of the univariate CUSUM chart.
}
\usage{
mcusum(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x must be a matrix
}
  \item{n}{
n(subgroup size) an integer 
}
}
\details{
For more details see Crosier(1988).
}
\value{
\item{x}{Matrix of the quality characteristics}
\item{n}{Subgroup size}
}
\references{
Crosier, R.B.(1988)"Multivariate Generalizations of Cumulative Sum Quality Control Schemes", Technometrics 30.
}
\author{
Edgar Santos Fernandez
}
\note{
If x is not a matrix or n is not an integer an error message is displayed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For more details see Crosier(1988)
}
\examples{
# constructing a MCUSUM chart with subgroup size(n)=2
x<-rbeta(300,1,3); x<-matrix(x,nrow=60)
mcusum(x,2)

# constructing a MCUSUM chart for individual observations
x<-rgamma(500,1,1); x<-matrix(x,nrow=100)
mcusum(x,1)


# constructing a MCUSUM chart and saving the t2 statistics
x<-rbeta(400,1,2); x<-matrix(x,nrow=100)
a<-mcusum(x,5)$t2;print(a)

# constructing a MCUSUM chart and saving the covariance matrix
x<-rbeta(400,1,2); x<-matrix(x,nrow=100)
b<-mcusum(x,5)$covariance;print(b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
