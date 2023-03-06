#' Parameters for stochastic block model.
#'
#' \code{N} returns the population size. \code{num_blocks} returns the number of life cycle stages. \code{block_sizes} returns the number of individuals in each life cycle stage (i.e. block).
#' 
#' Population size, number of life cycle stages, and the number of individuals per stage are parameters necessary to build a stochstic block model.

N <- nrow(it_indpop)
num_blocks <- length(levels(as.factor(it_indpop$stage)))
block_sizes <- as.vector(table(it_indpop$stage))
