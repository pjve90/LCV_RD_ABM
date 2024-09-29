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
results <- readRDS("./Scenario 1/lht_list.RData")

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

#Plot ----

#Longevity ----

par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,300),
       type = "n",
       main=paste("Parameter value",d),
       xlab="Longevity",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]) > 0)
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lng))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lng),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Lifetime reproductive output ----

par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,20),c(0,600),
       type = "n",
       main=paste("Parameter value",d),
       xlab="Lifetime reproductive output",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]) > 0)
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lro))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$lro),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at sexual maturity ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(10,25),c(0,600),
       type = "n",
       main=paste("Parameter value",d),
       xlab="Age sexual maturity",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]) > 0)
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$asm))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$asm),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at first reproduction ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",d),
       xlab="Age first reproduction",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]] > 0))
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$afr))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$afr),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at last reproduction ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",d),
       xlab="Age last reproduction",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]] > 0))
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$alr))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$alr),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}

#Age at menopause ----


par(mfrow=c(4,5))

for(d in 1:17){
  plot(c(0,100),c(0,600),
       type = "n",
       main=paste("Parameter value",d),
       xlab="Age menopause",
       ylab="Frequency")
  for(r in 1:10){
    if(nrow(results[grep(paste("d",d,"",sep="_"),names(results))][[r]] > 0))
      points(as.numeric(names(table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$meno))),
             table(results[grep(paste("d",d,"",sep="_"),names(results))][[r]]$meno),
             col=alpha(hcl.colors(10,palette = "Zissou 1")[r],0.5),
             pch=16)
  }
}
