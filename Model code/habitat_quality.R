#' Habitat quality
#'
#' \code{habitat} returns the stage-specific amount of resources that an individual can produce from the environment.
#'
#' This is a vector containing the amount of resources that an individual can produces depending on her life cycle stage. The first value is for juvenile stage, the second one is for adult stage, the third one for reproductive career stage, and the fourth one for post-reproductive stage.
#' 
habitat <- c(1,2,2,1)
names(habitat) <- c("juvenile","adult","reproductive career", "post-reproductive")