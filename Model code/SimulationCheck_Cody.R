################################################## Population growth rate as a function of fertility and mortality
# Cody Ross
A = 89

chains <- 1
iter <- 2000
warmup <- 1000

################################################################################################### Survival
prob_survive <- c(0.89 , 0.92 , 0.94, 0.96, 0.97, seq(0.99, 0.90,length.out=70), seq(0.90,0, length.out=14))
source("Code/SimSurvival.R")      # Simulate data under the above survival vector

dat_deaths <- list(N=N_S, D=D_S, Outcome=Outcome )
m_deaths <- stan(file="Code/Survival.stan", data = dat_deaths, iter = iter, warmup=warmup, chains = chains)

###################### Check fit on fake data
plot(m_deaths, pars="prob_survive")
plot(prob_survive, pch=16, col=rangi2, ylim=c(0,1))                     # Generateive parameters
post <- extract.samples(m_deaths)
lines(1:length(prob_survive), colMeans(post$prob_survive))              # Posterior mean
shade(apply(post$prob_survive,2,PI), 1:length(prob_survive))            # Credible region
Outcome2<-Outcome; Outcome2[which(Outcome2== -1)]<-NA
lines(1:length(prob_survive), colMeans(Outcome2,na.rm=TRUE),col="red")  # Mean of data as seen by Stan

################################################################################################## Fertility
prob_birth <- c(rep(0.0,15), seq(0.01,0.3, length.out=15), seq(0.3,0.01, length.out=15), rep(0,44))
source("Code/SimFertility.R")      # Simulate data under the above fertility vector

dat_births <- list(N=N_B , D=D_B, Birth=Birth, Age=Age )
m_births <- stan(file="Code/Fertility.stan", data = dat_births, iter = iter, warmup=warmup, chains = chains)

###################### Check fit on fake data
plot(m_births,pars="prob_birth")
plot(prob_birth, pch=16, col=rangi2, ylim=c(0,1))                   # Generateive parameters
post <- extract.samples(m_births)
lines(1:length(prob_birth), colMeans(post$prob_birth))              # Posterior mean
shade(apply(post$prob_birth,2,PI), 1:length(prob_birth))            # Credible region
Birth2<-Birth; Birth2[which(Birth2== -1)]<-NA
lines(1:length(prob_birth), colMeans(Birth2,na.rm=TRUE),col="red")  # Mean of data as seen by Stan

