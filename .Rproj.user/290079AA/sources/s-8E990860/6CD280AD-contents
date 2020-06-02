library(ggplot2)
library(compiler)

comparison <- function(dyad, beh.h, date, k, behaviour, inds, interactions, undirectional, average.duration = 1){
  dates = sort(as.Date(unique(date)))
  start.day=sample(dates[1:(length(dates)-round(length(dates)*k))], 1, FALSE, NULL)
  all.days=as.character(dates[which(dates==start.day):(which(dates==start.day)+length(dates)*k)])
  #all.days=unique(sample(unique(date), size = round(length(unique(date)) * k, 0), replace = F)) # set all dates as date format
  
  ### Resample data within the time frame
  ran.days=sample(all.days, size=round(length(all.days)*0.5,0), replace=F, NULL) # randomly select half the days
  other.days=all.days[!all.days%in%ran.days] # select days for the other half
  
  # Aggregate data in both halves
  ran.beh.h = beh.h[date%in%ran.days]
  ran.dyad = dyad[date%in%ran.days]
  xx.set1=aggregate(ran.beh.h, by=list(ran.dyad), sum) # aggregate for both individuals
  colnames(xx.set1)=c("dyad", "behaviour.ph")
  
  ran.beh.h = beh.h[date%in%other.days]
  ran.dyad = dyad[date%in%other.days]
  xx.set2=aggregate(ran.beh.h, by=list(ran.dyad), sum) # aggregate for both individuals
  colnames(xx.set2)=c("dyad", "behaviour.ph")
  
  common = intersect(xx.set1$dyad, xx.set2$dyad)
  xx.set1 = xx.set1[xx.set1$dyad %in% common,]
  xx.set2 = xx.set2[xx.set2$dyad %in% common,]
  if(isTRUE(undirectional)){
    common = sort(unique(unlist(lapply(strsplit(common, split = '.', fixed = T), function(x) {paste(sort(x), collapse = '.')}))))
    xx.set1 = xx.set1[xx.set1$dyad %in% common,]
    xx.set2 = xx.set2[xx.set2$dyad %in% common,]
                    }
  
  
  comparison.frame = data.frame(subset.size = k,
                                individuals = inds,
                                cor.halves = cor(xx.set1$behaviour.ph, xx.set2$behaviour.ph, method = 'spearman'),
                                interactions = sum(interactions[date %in% all.days])/average.duration,
                                days=length(all.days),
                                interactions.per.individual = (sum(interactions[date %in% all.days])/average.duration) / inds,
                                behaviour = behaviour)
  comparison.frame$interactions.per.dyad = comparison.frame$interactions / ((comparison.frame$individuals^2 - comparison.frame$individuals)/2)
  return(comparison.frame)
}

standardisation <- function(consistency.frame){
  xx.frame = consistency.frame
  xx.frame$interactions.per.dyad = round((xx.frame$interactions * 2 / ((as.numeric(xx.frame$individuals)^2 - as.numeric(xx.frame$individuals))/2)), 0)
  xx.frame$count = 1
  ind.int = aggregate(xx.frame$cor.halves, by = list(xx.frame$interactions.per.dyad), median, na.rm = T)
  colnames(ind.int) = c('average.interactions.per.dyad', 'average.median')
  ind.int$average.interactions.per.dyad = ind.int$average.interactions.per.dyad/2
  ind.int$sd = aggregate(xx.frame$cor.halves, by = list(xx.frame$interactions.per.dyad), sd, na.rm = T)$x
  ind.int$count = aggregate(xx.frame$count, by = list(xx.frame$interactions.per.dyad), sum, na.rm = T)$x
  results = data.frame(interaction.per.dyad = NA, median.correlation = NA, sd = NA)
  if(max(ind.int$average.median)>0.5){
    nr = min(which(ind.int$average.median >= 0.5))
    results=data.frame(interaction.per.dyad = ind.int$average.interactions.per.dyad[nr],
                       median.correlation = ind.int$average.median[nr],
                       sd = ind.int$sd[nr])
  }
  
  return(list(ind.int = ind.int, results = results))
}


consistency <- function(individual1, individual2, date, interactions, undirectional = FALSE, observation.time, k.seq = 0.05, j = 20, plot.col = 'black', behaviour = '', average.duration = 1){
  date = as.character(date)
  zero.days = aggregate(observation.time, by = list(date), sum)
  zero.days = setdiff(date, zero.days$Group.1[zero.days$x==0])
  individual1 = individual1[date%in%zero.days]
  individual2 = individual2[date%in%zero.days]
  interactions = interactions[date%in%zero.days]
  observation.time = observation.time[date%in%zero.days]
  date = date[date%in%zero.days]
  dyad = interaction(individual1, individual2)
  beh.h = (interactions / observation.time)
  beh.h[is.na(beh.h)] = 0
  beh.h[is.infinite(beh.h)] = 0
  inds = length(unique(c(individual1, individual2)))
  comparison = cmpfun(comparison)
  
  consistency.frame = lapply(seq(1,0.02, by=-k.seq), function(k){ # select increasingly smaller subsets of the data to calculate consistency
    comparison.frame = do.call(rbind, lapply(1:j, function(x) comparison(dyad = dyad, beh.h = beh.h, date = date, k = k, behaviour = behaviour, inds = inds, interactions = interactions, undirectional = undirectional, average.duration = average.duration)))
    return(comparison.frame)
  })
  consistency.frame = do.call(rbind, consistency.frame)
  standard = standardisation(consistency.frame = consistency.frame)
  consistency.plot = ggplot(consistency.frame, aes(x = interactions.per.dyad, y = cor.halves)) +
    geom_point(alpha = 0.5, show.legend = F, color = plot.col, fill = plot.col) +
    ylim(min(consistency.frame$cor.halves, na.rm = T),1) +
    ylab('Correlation between Random Halves') +
    xlab('Average Interactions per Dyad') +
    geom_hline(yintercept = 0.5, linetype = 2) +
    ggtitle(behaviour) +
    theme_bw()
  consistency.plot = consistency.plot +
    geom_line(standard$ind.int, mapping = aes(x = average.interactions.per.dyad, y = average.median), color = plot.col, size = 1.5) +
    geom_errorbar(standard$ind.int, mapping = aes(x = average.interactions.per.dyad, y = average.median, ymin=average.median-sd, ymax=average.median+sd), color = plot.col, width=.5,
                  position=position_dodge(0.05))
  return(list(consistency = consistency.frame, plot = consistency.plot, standardised = standard$results))
}
