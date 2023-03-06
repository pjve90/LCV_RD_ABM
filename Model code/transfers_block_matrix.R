#' Block matrix.
#'
#' \code{block_probs} returns a block matrix with the stage-specific probabilities of resource transfers.
#' 
#' This is a function that defines the block matrix that contains the stage-specific probabilities of resource transfers. Each column and row represents a life cycle stage.

block_probs <- matrix(c(1e-10, 8e-10, 8e-10, 8e-10,
                        1e-10, 1e-10, 1e-10, 1e-10,
                        1e-10, 2e-10, 2e-10, 3e-10,
                        1e-10, 3e-10, 3e-10, 2e-10),
                      nrow = num_blocks, ncol = num_blocks)
