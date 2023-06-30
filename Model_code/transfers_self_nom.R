#' Self sharing.
#'
#' \code{create_self_noms} returns the individual-specific probability of someone to keep the resources instead of transferring them.
#' 
#' This is a function that defines the individual probability that an individual transfers resources to herself instead of other members of the network. The parameters are the population size (\code{N}) and the probability of self nomination (\code{probs}).

create_self_noms <- function(N, probs) {
  vectors <- vector("list", N)
  for (i in 1:N) {
    vectors[[i]] <- rep(0, N)
    vectors[[i]][i] <- probs
  }
  return(vectors)
}
