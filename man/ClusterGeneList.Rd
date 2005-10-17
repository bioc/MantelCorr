\name{ClusterGeneList}

\alias{ClusterGeneList}

\title{ Generate Genes from a Cluster List }
\description{
  'ClusterGeneList' produces a list of both significant and
   nonsignificant genes from each respective cluster type
}

\usage{
ClusterGeneList(clus, clustlist.sig, x.data)
}

\arguments{
  \item{     clus}{ 'clusters' object returned by 'GetClusters' }
  \item{     clustlist.sig}{ 'SignificantClusters' object returned by 'ClusterList'}
  \item{     x.data}{ original (p x n) numeric data matrix (e.g., gene-expression data)}
}

\value{
  A list with components:
  \item{     SignificantClusterGenes }{significant cluster genes returned from 'ClusterList'}
  \item{     NonSignificantClusterGenes }{nonsignificant cluster genes returned from 'ClusterList'}
}

\author{ Brian Steinmeyer }

\note{ argument 'x.data' should have an ID gene variable, 'probes', attached as a 'dimnames'
attribute}

\seealso{ 'GetClusters' 'ClusterList' }

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
permutation.result <- PermutationTest(dist.matrices$Dfull, dist.matrices$Dsubsets, 100, 40, 0.05)

# generate both significant and non-significant gene clusters
cluster.list <- ClusterList(permutation.result, clusters.result$cluster.sizes, mantel.corrs)

# significant and non-significant cluster genes (expression values)
cluster.genes <- ClusterGeneList(clusters.result$clusters, cluster.list$SignificantClusters, data)

}
%}

\keyword{ cluster }% at least one, from doc/KEYWORDS

