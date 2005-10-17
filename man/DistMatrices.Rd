\name{DistMatrices}

\alias{DistMatrices}

\title{Compute Dissimilarity Matrices}

\description{
  'DistMatrices' uses 'dist' to compute dissimilarity matrices for 'data' and
   each cluster k from 'GetClusters'
}

\usage{
 DistMatrices(x.data, cluster.assignment)
}

\arguments{
  \item{      x.data}{ original 'data' matrix }
  \item{      cluster.assignment}{ cluster assignment vector, "clusters", returned by 'GetClusters' }
}

\value{
  returns a list with two components:

  \item{     Dsubsets }{dissimilarity matrices for each cluster k}
  \item{     Dfull }{dissimilarity matrix for the original 'data'}
}

\author{ Brian Steinmeyer }

\note{ 'GetClusters' should be executed prior to 'DistMatrices' }

\seealso{'GetClusters'}

\examples{
%\dontrun{
# simulate a p x n microarray expression dataset, where p = genes and n = samples
data.sep <- rbind(matrix(rnorm(1000), ncol=50), matrix(rnorm(1000, mean=5), ncol=50))
noise <- matrix(runif(40000), ncol=1000)
data <- t(cbind(data.sep, noise))
data <- data[1:200, ]
# data has p = 1,050 genes and n = 40 samples

clusters.result <- GetClusters(data, 100, 100)
dissimilarity.matrices <- DistMatrices(data, clusters.result$clusters)

}
%}

\keyword{ cluster }% at least one, from doc/KEYWORDS

