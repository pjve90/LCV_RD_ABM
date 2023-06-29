#' Self nominations.
#'
#' \code{create_self_noms} still need to write this down.
#' 
#' Still need to write this down.

create_self_noms <- function(N, probs) {
  vectors <- vector("list", N)
  for (i in 1:N) {
    vectors[[i]] <- rep(0, N)
    vectors[[i]][i] <- probs
  }
  return(vectors)
}
