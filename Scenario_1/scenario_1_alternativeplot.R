# Plot results from scenario 1
# Aim is to focus on the standard deviation of the different life history traits, which is the main focus of the predictions
# Plot is set up to show i) which life history trait has the highest standard deviation, and ii) how the different parameters shape the standard deviation.


# Life cycle variation and resource dynamics ABM: Scenario 1 - results ----

#Here is the code to get the summary statistics and plots from the simulation of Scenario 1, which aims to understand the influence of resource production on the variability of life cycles.

#Production: parameter sweep between 0.1 and 0.9 (17 values)
#Transfers: null
#Habitat quality: baseline

#R settings ----

#set work directory
getwd()
setwd("./LCV_RD_ABM")

#install packages
#install.packages("scales")
library(scales)

#Data import ----

#import raw results from the simulation
results <- readRDS("./Scenario_1/lht_list.RData")

#Summary statistics ----

summary_stats <- lapply(1:length(results),function(x)list())
#match the original names with the sample
names(summary_stats) <- names(results)
names(summary_stats)

#gather summary statistics of each result
for (i in 1:length(results)){
  if(nrow(results[[i]]) == 0){
    summary_stats[[i]] <- summary_stats[[i]]
  } else{
    summary_stats[[i]]$summary <- summary(results[[i]][,c("lng","lro","asm","afr","alr","meno")])
    summary_stats[[i]]$sd <- as.data.frame(lapply(results[[i]][,c("lng","lro","asm","afr","alr","meno")],sd,na.rm=T))
  }
}

# We extract and summarize the results. For the standard deviation of each life history trait, we take the median value from across the 10 repetitions. 

lng<-NA
lro<-NA
asm<-NA
afr<-NA
alr<-NA
meno<-NA

counter<-1
for(variant in 1:17){
  currentlng<-NA
  currentlro<-NA
  currentasm<-NA
  currentafr<-NA
  currentalr<-NA
  currentmeno<-NA
  for(rep in 1:10){
    currentlng[rep]<-ifelse(is.null(summary_stats[[counter]]$sd$lng),NA,summary_stats[[counter]]$sd$lng)
    currentlro[rep]<-ifelse(is.null(summary_stats[[counter]]$sd$lro),NA,summary_stats[[counter]]$sd$lro)
    currentasm[rep]<-ifelse(is.null(summary_stats[[counter]]$sd$asm),NA,summary_stats[[counter]]$sd$asm)
    currentafr[rep]<-ifelse(is.null(summary_stats[[counter]]$sd$afr),NA,summary_stats[[counter]]$sd$afr)
    currentalr[rep]<-ifelse(is.null(summary_stats[[counter]]$sd$alr),NA,summary_stats[[counter]]$sd$alr)
    currentmeno[rep]<-ifelse(is.null(summary_stats[[counter]]$sd$meno),NA,summary_stats[[counter]]$sd$meno)
   counter<-counter+1
  }
  lng[variant]<-median(currentlng,na.rm=T)
  lro[variant]<-median(currentlro,na.rm=T)
  asm[variant]<-median(currentasm,na.rm=T)
  afr[variant]<-median(currentafr,na.rm=T)
  alr[variant]<-median(currentalr,na.rm=T)
  meno[variant]<-median(currentmeno,na.rm=T)
}


# We combine the median sd of each life history parameter in a table

combined<-as.data.frame(matrix(ncol=7,nrow=17))
colnames(combined)<-c("parameter","lng","lro","asm","afr","alr","meno")
combined$parameter<-c(seq(from=0.1,to=0.9,by=0.05))
combined$lng<-lng
combined$lro<-lro
combined$asm<-asm
combined$afr<-afr
combined$alr<-alr
combined$meno<-meno


# We plot the median standard deviations. Each life history trait has it's own plot, but the y-axis are standardized to quickly show which parameter has the highest standard deviation. In each plot, the values for the different parameters are sorted on the x-axis. Here, this shows that i) varying the parameters does not seem to change the standard deviation, and ii) that the highest standard deviation is in longevity, followed by age at last reproduction and menopause, whereas age at sexual maturity and age at first reproduction only show very little variation. The current version of the plots are rough, they could be made prettier. Instead of the median, we could also show all 10 values for each parameter in each life history trait (or the mean, or the range).
par(mfrow=c(1,6))
plot(data=combined,lng~parameter,ylim=c(1,15))
plot(data=combined,lro~parameter,ylim=c(1,15))
plot(data=combined,asm~parameter,ylim=c(1,15))
plot(data=combined,afr~parameter,ylim=c(1,15))
plot(data=combined,alr~parameter,ylim=c(1,15))
plot(data=combined,meno~parameter,ylim=c(1,15))
