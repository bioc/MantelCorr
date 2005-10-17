############################################################################
# Function ClusterList() returns two lists containing both significant and #
# nonsignificant clusters, based on the permuted p-value found previously  #
#                                                                          #
# Input Parameters:                                                        #
# 1) permuted p-value                                                      #
# 2) cluster sizes                                                         #
# 3) original k mantel cluster correlations                                #
#                                                                          #
# Returns:                                                                 #
# 1) list of significant clusters (with cluster number, Mantel cluster     #
#    correlation, cluster size)                                            #
# 2) list of nonsignificant clusters with identical information as in (1)  #
#                                                                          #
# Date:  August 12, 2005                                                   #
# written by:  Brian Steinmeyer                                            #
############################################################################


## Create and return information for both significant and non-significant 
## clusters (based on the critical value), including:  cluster number, mantel 
## cluster correlation and cluster size


ClusterList <- function(p.val, clus.size, mantel.cors)  {
   
  ## list the original k Mantel cluster correlations 
  clust.list <- cbind(as.numeric(names(mantel.cors)), as.numeric(mantel.cors), 
                as.numeric(clus.size))

  ## return cluster lists to the user as a data frame with the appropriate 
  ## column headings
  clustlist.sig <- as.data.frame(clust.list[abs(clust.list[,2]) >= p.val, , drop=FALSE])
  names(clustlist.sig) <- c("significant cluster", "Mantel correlation", "cluster size")

  clustlist.nonsig <- as.data.frame(clust.list[abs(clust.list[,2]) < p.val, , drop=FALSE])
  names(clustlist.nonsig) <- c("nonsignificant cluster", "Mantel correlation", "cluster size")
 return(list(SignificantClusters=clustlist.sig, NonSignificantClusters=clustlist.nonsig))
}





