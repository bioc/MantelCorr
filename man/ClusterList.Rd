\name{ClusterList}

\alias{ClusterList}

\title{ Generate a Cluster List }

\description{
  'ClusterList' generates a list of both significant and nonsignificant
   clusters, with cluster number, Mantel cluster correlation and size
}

\usage{
ClusterList(p.val, clus.size, mantel.cors)
}

\arguments{
  \item{     p.val}{ permutation p-value returned from 'PermutationTest' }
  \item{     clus.size}{ vector of k cluster sizes returned from 'GetCluster' }
  \item{     mantel.cors}{ orignal, unpermuted k Mantel correlations returned from 'MantelCorrs' }
}

\value{
  A list with components:
  \item{     SignificantClusters}{clusters with significant Mantel correlation, equal to or larger
        than the permutation p-value returned by 'PermutationTest'}
  \item{     NonSignificantClusters}{clusters with nonsignificant Mantel correlation, smaller
                than the permutation p-value returned by 'PermutationTest'}
}

\author{ Brian Steinmeyer }

\seealso{ 'PermutationTest' }

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

}
%}

\keyword{ cluster }% at least one, from doc/KEYWORDS

