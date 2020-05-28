
### needed:
#daily.beh == data set with daily shared observation time and interactions observed per dyad

comparison.frame=data.frame(k=1, cor.halves=1, days=NA, mean.obs.time=NA, interactions=NA)
for(k in seq(1,0.05, by=-0.05)){
  for(j in 1:100){
    days=as.Date(unique(daily.beh$date))
    start.day=sample(days[1:(length(days)-length(days)*k)],1)
    all.days=days[which(days==start.day):(which(days==start.day)+length(days)*k)]
    ### Full Data Resample
    ran.days=sample(all.days, size=round(length(all.days)*0.5,0), replace=F)
    other.days=all.days[!all.days%in%ran.days]
    ran.beh=subset(daily.beh, date%in%ran.days)
    ran.dai=subset(dai.data, date%in%ran.days)
    xx.set=aggregate(ran.beh[,c("behaviour.sent", "observation.time")], by=list(ran.beh$individual2, ran.beh$individual1), sum)
    colnames(xx.set)=c("individual2", "individual1", "behaviour.sent", "observation.time")
    xx.set$behaviour.ph=xx.set$behaviour.sent/xx.set$observation.time
    xx.set$behaviour.ph[is.na(xx.set$behaviour.ph)]=0
    xx.set1=xx.set
    full.data.behaviour=xx.set$behaviour.ph

    ###### Subset

    ran.beh=subset(daily.beh, date%in%other.days)
    ran.dai=subset(dai.data, date%in%other.days)
    xx.set=aggregate(ran.beh[,c("behaviour.sent", "observation.time")], by=list(ran.beh$individual2, ran.beh$individual1), sum)
    colnames(xx.set)=c("individual2", "individual1", "behaviour.sent", "observation.time")
    xx.set$behaviour.ph=xx.set$behaviour.sent/xx.set$observation.time
    xx.set$behaviour.ph[is.na(xx.set$behaviour.ph)]=0

    comparison.frame[nrow(comparison.frame)+1,]=NA
    comparison.frame$k[nrow(comparison.frame)]=k
    comparison.frame$cor.halves[nrow(comparison.frame)]=cor(xx.set$behaviour.ph, full.data.behaviour)
    comparison.frame$mean.obs.time[nrow(comparison.frame)]=mean(c(xx.set$observation.time,xx.set1$observation.time))
    comparison.frame$interactions[nrow(comparison.frame)]=sum(xx.set$behaviour.sent)+sum(xx.set1$behaviour.sent)
    comparison.frame$days[nrow(comparison.frame)]=length(unique(ran.days))*2

  }
}
comparison.frame=comparison.frame[-1,]
comp.mang=comparison.frame
