\name{MantelCorrs}

\alias{MantelCorrs}

\title{ Compute Mantel Correlation(s) }

\description{
   'MantelCorrs' computes the Mantel correlation between two
   dissimilarity matrices
}

\usage{
MantelCorrs(Dfull, Dsubsets)
}

\arguments{
  \item{Dfull}{ distance matrix returned by 'DistMatrices' using original 'data' }
  \item{   Dsubsets}{ list of distance matrices from each k cluster or partition returned
                   by 'DistMatrices' }
}

\value{
  A list with k components
  \item{     where component i }{Mantel correlation for cluster i, i = 1,...,k}
}

\references{ Mantel N:  The detection of disease clustering and a generalized regression
             approach.  Cancer Research. 27(2), 209-220 (1967). }


\author{ Brian Steinmeyer }

\note{ the value 'k' corresponds to the parameter 'num.k' in 'GetClusters' }

\seealso{ 'GetClusters' 'DistMatrices' 'kmeans' }

\section{Warning}{ The function is meant to be executed AFTER 'GetClustes' and 'DistMatrices' (see example) }

\examples{
%\dontrun{
# simulate a p x n microarray expression dataset, where p = genes and n = samples
data.sep <- rbind(matrix(rnorm(1000), ncol=50), matrix(rnorm(1000, mean=5), ncol=50))
noise <- matrix(runif(40000), ncol=1000)
data <- t(cbind(data.sep, noise))
data <- data[1:200, ]
# data has p = 1,050 genes and n = 40 samples

clusters.result <- GetClusters(data, 100, 100)
dist.matrices <- DistMatrices(data, clusters.result$clusters)
mantel.corrs <- MantelCorrs(dist.matrices$Dfull, dist.matrices$Dsubsets)

}
%}

\keyword{ cluster }% at least one, from doc/KEYWORDS

