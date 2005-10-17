############################################################################
# Function "GetClusters()" uses the kmeans() function in R; partitions the # 
# gene-space                                                               #
#                                                                          #
# Input parameters:                                                        #
# 1) Data - does not use the first two columns                             #
# 2) choice of k                                                           #
# 3) number of iterations                                                  #
#                                                                          #
# Returns to the end user:                                                 #
# 1) cluster assignment from the kmeans() function                         #
# 2) size of each cluster k                                                # 
#                                                                          #
# Date:  August 12, 2005                                                   #
# written by:  Brian Steinmeyer                                            #
############################################################################


## over-partition (one-half the number of variables, p) the gene-space using
## kmeans.  Return kmeans cluster assignment and size to the user

## declare original data
## use only gene-expression values
# data <- <your data>

GetClusters <- function(x.data, num.k, num.iters)  {
   p <- kmeans(x.data, num.k, num.iters)        
   clus <- p$cluster                      
   clus.size <- p$size
 return(list(clusters=clus, cluster.sizes=clus.size))
}



