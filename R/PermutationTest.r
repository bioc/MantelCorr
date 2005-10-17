###################################################################################
# Function PermutationTest() obtains a p-value of significance from a permutation # 
# test of Dfull.  Uses MantelCorr().                                              #
#                                                                                 #
# Input Parameters:                                                               #
# 1) Dfull (distance matrix using all p variables)                                #
# 2) Dsubsets (individual cluster distance matrices)                              #
# 3) number of permutations                                                       #
# 4) number of samples (chips)                                                    #
# 5) desired alpha-level of significance                                          #
#                                                                                 #
# Returns to the end user:                                                        #
# 1) permuted p-value for the alpha-chosen level of significance                  #
#                                                                                 #
# Date:  August 12, 2005                                                          #
# written by:  Brian Steinmeyer                                                   #
###################################################################################


## permute rows and columns "num.permute" times
## to destroy the dependent nature of Dfull

## for each permutation, recalculate the mantel correlation
## of "Dfull" with each cluster's distance matrix, and
## retain the maximum Mantel correlation


PermutationTest <- function(Dfull, Dsubsets, num.per, num.chips, alpha)
{
     mantel <- function(a, b) cor(a,b) 
     permute <- NULL                             
     for (i in 1:num.per)                        
     {                                           
       distmat <- as.matrix(Dfull)           
       x <- sample(1:num.chips)                  
       distmat <- as.dist(distmat[x,x])          
       permute <- cbind(permute, max(abs(as.numeric(lapply(Dsubsets, mantel, distmat)))))
     }
   cut.value1 <- quantile(as.vector(permute), probs = (1 - alpha))
 return(cut.value1)
}
      
## select the "alpha" percentile from the empirical distribution
## of the maximum Mantel correlations found in the permutation
## test as the critical value for determining cluster significance.
## return its value to the end user






