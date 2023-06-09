#' Block matrix.
#'
#' \code{blockmatrix} returns the block matrix of the stochastic block model for the resource transfers in the model.
#' 
#' This is a function that defines the probabilities that an individual transfers resource to other individual, depending on the life cycle stage that they both are. \code{block_offsets} is a matrix that defines the weights of sharing directionalyt. \code{block_probs} calculates the block matrix for the stochastic block model.

blockmatrix <- function(prob_ties){
  block_offsets<-matrix(c(1,8,8,8,
                          1,1,1,1,
                          1,2,2,3,
                          1,3,3,2),
                        nrow=num_blocks,ncol=num_blocks) #weights of sharing directionality
  block_probs<-prob_ties*block_offsets #block matrix
  return(block_probs)
}


