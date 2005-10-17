\name{GetClusters}

\alias{GetClusters}

\title{Over-Partition a (p x n) Data Matrix using 'kmeans'}

\description{
  'GetClusters' uses an overly large k with the 'kmeans'
   function to over-partition p variables (rows = genes) from n
   objects (cols = samples) from a given data matrix 'x.data'}

\usage{
GetClusters(x.data, num.k, num.iters)
}

\arguments{
  \item{     x.data}{p x n data matrix of numeric values}
  \item{     num.k}{number of k partitions desired}
  \item{     num.iters}{number of iterations - recommend >= 100}
}

\value{
  'GetClusters' returns a list with the following components:

  \item{     clusters}{cluster assignment from 'kmeans'}
  \item{     cluster.sizes}{size of each cluster k from 'kmeans'}
}

\author{ Brian Steinmeyer }

\note{The input data matrix, x.data, must be numeric (e.g., gene-expression values).
      We recommend using 'num.k' = one-half the number of genes and 'num.iters' greater than 50}


\seealso{'kmeans'}


\examples{
%\dontrun{
# simulate a p x n microarray expression dataset, where p = genes and n = samples
data.sep <- rbind(matrix(rnorm(1000), ncol=50), matrix(rnorm(1000, mean=5), ncol=50))
noise <- matrix(runif(40000), ncol=1000)
data <- t(cbind(data.sep, noise))
data <- data[1:200, ]
# data has p = 1,050 genes and n = 40 samples

clusters.result <- GetClusters(data, 100, 100)

}
%}

\keyword{ cluster } % at least one, from doc/KEYWORDS


