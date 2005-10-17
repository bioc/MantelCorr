##########################################################################################
# Function ClusterGeneList() returns both significant and nonsignificant genes from each # 
# type of cluster                                                                        #
#                                                                                        #
# Input Parameters:                                                                      #
# 1) cluster assignment from the kmeans algorithm                                        #
# 2) significant cluster list returned from ClusterList                                  #
# 3) original data                                                                       #
#                                                                                        #
# Returns:                                                                               #
# 1) Two lists of both significant and nonsignificant cluster genes                      #
#                                                                                        #
# Date:  August 12, 2005                                                                 #
# written by:  Brian Steinmeyer                                                          #
##########################################################################################


## create and return genes from all significant and non-significant clusters

ClusterGeneList <- function(clus, clustlist.sig, x.data)  {
      
     store.clus <- rep(0, length(clus))
         for (i in clustlist.sig[,1]) store.clus[clus==i] <- 1
            cluster.genes.sig <- x.data[store.clus==1,]
            cluster.genes.nonsig <- x.data[store.clus==0,]

     SigGenes <- dimnames(cluster.genes.sig)[[1]]
     NonSigGenes <- dimnames(cluster.genes.nonsig)[[1]]

  return(list(SignificantClusterGenes=SigGenes, NonSignificantClusterGenes=NonSigGenes))
}





