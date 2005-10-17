######################################################################
# Function MantelCorrs() computes each cluster's mantel correlation, #
# correlating its distance matrix with Dfull                         #
#                                                                    #
# Input Parameters:                                                  # 
# 1) Distance Matrix using all p variables (genes).                  #
# 2) Distance Matrices for each kth cluster Dsubset{k}               #
#                                                                    #
# Returns to the end user:                                           #
# 1) a list of k mantel correlations from the RunKmeans() algorithm  #
#                                                                    #
# Date:  August 12, 2005                                             #
# written by:  Brian Steinmeyer                                      #
######################################################################


## correlate each cluster's distance matrix with
## "Dfull" using the Mantel correlation


MantelCorrs <- function(Dfull, Dsubsets)  {
       mantel <- function(a, b) cor(a,b)
       mantel.cors <- lapply(Dsubsets, mantel, Dfull)
  return(mantel.cors)
}




 
