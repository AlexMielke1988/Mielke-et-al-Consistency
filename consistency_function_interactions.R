##### This function creates the consistency measure as described in 'Mielke et al 2020 Consistency of social interactions in sooty mangabeys and chimpanzees'
##### This is an adaptation of the script that works with data that are structured by interaction rather than day
##### Essentially, the script repeatedly splits the interaction distributions randomly in half and compares the halves; it does this for successively smaller subsets of the data as a standardised measure of consistency

##### The following parameters need to be defined
#### Input
## 'Individual1' is a vector with the sender of the interaction type
## 'Individual2' is a vector with the receiver of the interaction type
## 'date' is a vector with the date for which the dyadic data are included
## 'duration' is a vector with the number of minutes of interactions for the dyad on that day; for event data, set to 1 for all
## 'undirectional' is a boolean parameter; if 'undirectional == T', sender and receiver are considered the same for each day and rates are calculated only once for the dyad
## 'observation time' is a vector with the number of hours that the dyad could have been observed on that day; if not available, set to 1
## 'k.seq' is the size of the steps by which the dataset is reduced repeatedly. So, if k.seq is 0.05, the consistency is calculated for 100%, 95%, 90% etc of the data. The smaller k.seq, the longer things take, but the more accurate the result
## 'j' is the number of interations for each subset of data. So, if j = 20, then the full dataset will be split 20 times, then the next smallest subset of data will be split and compared 20 times, etc 
## 'plot.col' is the color of the plot
## 'behaviour' adds the title of the behaviour to the plot
## 'average.duration' is the average duration of an interaction of that type in case durations are used rather than occurrences, to make comparable plots for event and state interactions
## 'plotting' is the x-axis label for the plots; 'interactions' plots by interactions per dyad, 'time' plots by observation time in hours

#### Output
##'consistency' data frame with all iterations including the number of interactions per dyad, the size of the dataset, and the correlation between the halves
##'plot' ggplot of the consistency frame
##'standardised' data frame with every number of interactions per dyad that was observed and the median of the correlations for this number of interactions per dyad

library(ggplot2)
library(tidyverse)

consistency_interactions <- function(individual1, 
                        individual2, 
                        date, 
                        duration, 
                        undirectional = FALSE, 
                        observation.time, 
                        k.seq = 0.05, 
                        j = 20, 
                        plot.col = 'black', 
                        behaviour = '', 
                        average.duration = 1,
                        plotting = 'interactions'){
  #vectorise dates
  date = as.character(date)
  
  # create dyads
  dyad = interaction(individual1, individual2, sep = '_') %>% 
    as.character()
  
  # create interactions per hour if observation time is available
  beh.h = (duration / observation.time)
  beh.h[is.na(beh.h)] = 0
  beh.h[is.infinite(beh.h)] = 0
  
  #number of individuals in dataset
  inds = length(unique(c(individual1, individual2)))
  ids = unique(c(individual1, individual2))
  
  # make all dyads to add after
  dyads <- expand_grid(actor = ids, receiver = ids) %>% 
    data.frame() %>% 
    filter(actor!=receiver)
  
  # create directional and non-directional dyad labels
  dyads$dyad_directional <- sapply(1:nrow(dyads), 
                                   function(x){
                                     paste(c(dyads$actor[x], 
                                             dyads$receiver[x]), 
                                           collapse = '_')})
  dyads$dyad_undirectional <- sapply(1:nrow(dyads), 
                                     function(x){
                                       paste(sort(c(dyads$actor[x], 
                                                    dyads$receiver[x])), 
                                             collapse = '_')})
  
  # apply comparison function  for subsets of different sizes (between 100% and 2% of days, in steps determined by k.seq)
  consistency.frame = lapply(
    seq(1,0.02, by=-k.seq), 
    function(k){ # select increasingly smaller subsets of the data to calculate consistency
    
    # j is the number of trials that was set
    comparison.frame = lapply(1:j, function(x){
      ### sub-function that does the comparison between one random half and the other random half of the dataset
      # dates
      dates = sort(as.Date(unique(date)))
      # randomly select start day of time period
      start.day=sample(dates[1:(length(dates)-round(length(dates)*k))], 1, FALSE, NULL)
      
      # select all days of the specified length after the start date (100% of days, 95% of days etc)
      all.days=as.character(dates[which(dates==start.day):(which(dates==start.day)+length(dates)*k)])
      
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
      
      # add both to dyads
      xx.set <- dyads %>% 
        left_join(xx.set1, by = c('dyad_directional' = 'dyad')) %>% 
        rename('behaviour.ph1' = 'behaviour.ph') %>% 
        left_join(xx.set2, by = c('dyad_directional' = 'dyad')) %>% 
        rename('behaviour.ph2' = 'behaviour.ph') %>% 
        replace_na(list('behaviour.ph1' = 0,
                        'behaviour.ph2' = 0))
      
      if(undirectional){
        xx.set <- xx.set %>% 
          group_by(dyad_undirectional) %>% 
          summarise(behaviour.ph1 = sum(behaviour.ph1),
                    behaviour.ph2 = sum(behaviour.ph2)) %>% 
          ungroup()
      }
      
      
      # create results by correlating the distributions of both halves
      comparison.frame = data.frame(subset.size = k,
                                    individuals = inds,
                                    cor.halves = cor(xx.set$behaviour.ph1, xx.set$behaviour.ph2, method = 'spearman'),
                                    interactions = sum(duration[date %in% all.days])/average.duration,
                                    days=length(all.days),
                                    obs.time = sum(observation.time[date %in% all.days]),
                                    interactions.per.individual = (sum(duration[date %in% all.days])/average.duration) / inds,
                                    behaviour = behaviour)
      comparison.frame$interactions.per.dyad = comparison.frame$interactions / 
        ((comparison.frame$individuals^2 - comparison.frame$individuals)/2)
      
      return(comparison.frame)
    }) %>%
      bind_rows()
    return(comparison.frame)
  }) %>% bind_rows()
  
# plot results
  if(plotting == 'interactions'){
  consistency.plot = ggplot(consistency.frame, aes(x = interactions.per.dyad, y = cor.halves)) +
    geom_point(alpha = 0.5, show.legend = F, color = plot.col, fill = plot.col) +
    geom_smooth(color = plot.col, method = 'loess')+
    ylim(min(consistency.frame$cor.halves, na.rm = T),1) +
    ylab('Correlation between Random Halves') +
    xlab('Average Interactions per Dyad') +
    geom_hline(yintercept = 0.5, linetype = 2) +
    ggtitle(behaviour) +
    theme_bw()
  }
  if(plotting == 'time'){
    consistency.plot = ggplot(consistency.frame, aes(x = obs.time, y = cor.halves)) +
      geom_point(alpha = 0.5, show.legend = F, color = plot.col, fill = plot.col) +
      geom_smooth(color = plot.col, method = 'loess')+
      ylim(min(consistency.frame$cor.halves, na.rm = T),1) +
      ylab('Correlation between Random Halves') +
      xlab('Observation Time') +
      geom_hline(yintercept = 0.5, linetype = 2) +
      ggtitle(behaviour) +
      theme_bw()
  }
  
  # standardisation: for every number of interactions per dyad, detect median of correlations
    # take consistency frame from previous analysis
    xx.frame = consistency.frame
    
    # calculate interactions per dyad used in the frame
    xx.frame$interactions.per.dyad = 
      round((xx.frame$interactions * 2 / ((as.numeric(xx.frame$individuals)^2 - as.numeric(xx.frame$individuals))/2)), 0)
    xx.frame$count = 1
    
    # determine median value of all correlations for that number of interactions per dyad
    ind.int = aggregate(xx.frame$cor.halves, by = list(xx.frame$interactions.per.dyad), median, na.rm = T)
    colnames(ind.int) = c('average.interactions.per.dyad', 'average.median')
    ind.int$average.interactions.per.dyad = ind.int$average.interactions.per.dyad/2
    
    # create standard deviation for the same thing
    ind.int$sd = aggregate(xx.frame$cor.halves, by = list(xx.frame$interactions.per.dyad), sd, na.rm = T)$x
    
    
  
  
  return(list(consistency = consistency.frame, plot = consistency.plot, standardised = ind.int))
}
