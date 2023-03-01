set.seed(8765309)
A <- length(prob_survive)
N_dead <- 1200

AOD <- rep(NA,N_dead)

for (i in 1:N_dead) {
    for (age in 1:A) {
            if (runif(1) > prob_survive[age] ) {
            AOD[i] <- age
            break
        }
    }
}


D <- A
Survived <-matrix(NA,nrow=length(AOD), ncol=(D+1))
 for( i in 1:length(AOD)) {
 Z <- c(rep(1,AOD[i]),0)
 Survived[i,1:length(Z)] <- Z
 }

Survived[is.na(Survived)] <- -1
Survived <- Survived[,-1]


Outcome <- rbind(Survived[,1:A])
N_S <- nrow(Outcome)
D_S <- ncol(Outcome)
