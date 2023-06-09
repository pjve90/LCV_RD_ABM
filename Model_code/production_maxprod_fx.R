#' Stage-specific maximum amount of resource production .
#'
#' \code{max_production} returns the maximum amount of resources an individual can produce, based on her life cycle stage.
#' 
#' This is a function that defines the maximum amount of resource that an individual can produce, depending on the life cycle stage that she is. \code{habitat_quality} is the parameter that defines the amount of resources available in the environment. \code{stage_maxprod} is a vector with the stage-specific offset in the maximum resources an individual can produce. \code{maxprod} calculates the stage-specific maximum amount of resource production.

max_production <- function(habitat_quality){
  stage_maxprod<-c(0.25,1,1,0.33) #stage-specific offset in the maximum resources an individual can produce
  maxprod <- round(habitat_quality * stage_maxprod,0) #stage-specific maximum amount of resource production 
  names(maxprod) <- c("juvenile","adult","reproductive career", "post-reproductive")
  return(maxprod)
}
