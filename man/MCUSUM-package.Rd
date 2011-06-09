\name{MCUSUM-package}
\alias{MCUSUM-package}
\alias{MCUSUM}
\docType{package}
\title{
Multivariate Cumulative Sum (MCUSUM) Control Chart

}
\description{
MCUSUM package perform the MCUSUM Control Chart according to Crosier(1988).
Allow to monitor simultaneously multiple quality characteristics.
MCUSUM is an extension of the univariate CUSUM chart.

}
\details{
\tabular{ll}{
Package: \tab MCUSUM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-06-09\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}

The mcusum function have the following structure, mcusum(x,n); where x must be a matrix and n(subgroup size).   
The package use the UCL =5.5
}
\author{
Edgar Santos Fernandez

Maintainer: Edgar Santos Fernandez <edgar.santos@etecsa.cu>
}
\references{
Crosier, R.B.(1988)"Multivariate Generalizations of Cumulative Sum Quality Control Schemes", Technometrics 30.
 Montgomery,Douglas C.(2005) "Introduction to Statistical Quality Control",John Wiley & Sons, 5 edition.
}

\keyword{ package }
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
