##############################################################################
# Function "DistMatrices()" computes similarity matrices for the data matrix # 
# and each cluster                                                           #
#                                                                            #
# Input Parameters:                                                          #
# 1) Data                                                                    #
# 2) Cluster assignment vector from RunKmeans() function                     #
#                                                                            #
# Returns to the end user:                                                   #
# 1) D_Full                                                                  #
# 2) D_Subset{i}                                                             #
#                                                                            #
# Date:  August 12, 2005                                                     #
# written by:  Brian Steinmeyer                                              #
##############################################################################


## calculate Euclidean distance matrix for all samples
## calculate Euclidean distance matrix for each cluster
## return to end user


DistMatrices <- function(x.data, cluster.assignment)  {
       tdist <- function(p) dist(t(p))
       dist.full <- dist(t(x.data))                    
       Dsub.i <- by(x.data, cluster.assignment, tdist)                 
  return(list(Dsubsets=Dsub.i, Dfull=dist.full))
}







