set.seed(8675309)


N_B <- nrow(Outcome)
D_B <- ncol(Outcome)

Birth <- matrix(NA,nrow=N_B,ncol=D_B)

Age <- rep(NA,N_B)

for(i in 1:N_B){
Age[i] <- max(c(0,which(Outcome[i,]==1)))
}

Age <- ifelse(Age==0, 1, Age)

for(i in 1:N_B){
	for(d in 1:Age[i]){
		Birth[i,d] <- rbinom(1, size=1, prob=prob_birth[d])
	}
}

Birth[is.na(Birth)] <- -1

str(Age)
