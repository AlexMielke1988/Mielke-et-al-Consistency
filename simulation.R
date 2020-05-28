######### this code creates simulated data for n number of individuals, interacting 1-4 times a day, with three different certainties of choice (strong preference for some individuals, medium preference for some individuals, egalitarian distribution)
### at the end, for each individual, there should be interactions every day that they are the focal

nr.ids = c(10, 10, 10, 18, 18, 18, 26, 26, 26)

sim.data = lapply(nr.ids, function(k) {
  subj.to.keep = as.character(1:k)
  dates = seq(as.Date("2014-11-01"), as.Date("2015-10-31"), "days")
  date.set = data.frame(expand.grid(subj.to.keep, dates))
  colnames(date.set) = c('focal', 'date')

  data.set = lapply(1:nrow(date.set), function(x) {
    xx = date.set[x, ]
    xx = xx[rep(seq_len(nrow(xx)), each = sample(1:k, 1)), ]
    xx$gr.bout.index = x + (1:nrow(xx)) - 1
    xx = xx[rep(seq_len(nrow(xx)), each = length(subj.to.keep)), ]
    xx$pot.partner = as.character(subj.to.keep)
    xx$focal = as.character(xx$focal)
    xx = subset(xx, focal != pot.partner)
    return(xx)
  })

  data.set = do.call(rbind, data.set)

  #### set association likelihood
  foc = as.character(expand.grid(subj.to.keep, subj.to.keep)[, 1])
  dyads = data.frame(
    focal = foc,
    partner = sort(as.character(
      expand.grid(subj.to.keep, subj.to.keep)[, 1]
    ))
  )

  remove = unlist(lapply(1:nrow(data.set), function(i) {
    return(sample(c(0, 1), size = 1, prob = c(0.3, 0.7)))
  }))
  data.set = subset(data.set, remove == 1)
  data.set = data.set[data.set$focal!= data.set$pot.partner,]

  data.set$eff.gr.size = unlist(lapply(unique(data.set$gr.bout.index), function(i) {
    xx = nrow(subset(data.set, gr.bout.index == i))
    xx = rep(xx, xx)
    return(xx)
  }))

  data.set = subset(data.set, eff.gr.size > 1)
  data.set$dyad = apply(cbind(
    as.character(data.set$focal),
    as.character(data.set$pot.partner)
  ), 1, function(x) {
    paste(sort(x), collapse = "_")
  })

  ### set grooming likelihood
  dyads$gr.llh = 0
  dyads$dyad = apply(cbind(as.character(dyads$focal), as.character(dyads$partner)), 1, function(x) {
    paste(sort(x), collapse = "_")
  })
  dyads = dyads[!duplicated(dyads$dyad), ]
  dyads$gr.llh = abs(rnorm(
    n = nrow(dyads),
    mean = 0.5,
    sd = 0.2
  ))
  dyads$gr.llh[dyads$gr.llh > 1] = 1

  data.set$gr.llh = dyads$gr.llh[match(data.set$dyad, dyads$dyad)]
  data.set$llh.high = data.set$gr.llh ^ 4
  data.set$llh.medium = data.set$gr.llh ^ 2
  data.set$llh.low = data.set$gr.llh ^ 0.8
  data.set = data.set[complete.cases(data.set), ]
  data.set$llh.high[data.set$llh.high == 1] = 0.9999
  data.set$llh.medium[data.set$llh.medium == 1] = 0.9999
  data.set$llh.low[data.set$llh.low == 1] = 0.001
  data.set$llh.high[data.set$llh.high == 0] = 0.001
  data.set$llh.medium[data.set$llh.medium == 0] = 0.001
  data.set$llh.low[data.set$llh.low == 0] = 0.001

  data.set$gr.initiated.low = 0
  data.set$gr.initiated.medium = 0
  data.set$gr.initiated.high = 0
  data.set$gr.initiated.lowin = 0
  data.set$gr.initiated.mediumin = 0
  data.set$gr.initiated.highin = 0
  data.set$gr.initiated.random = 0
  data.set$gr.initiated.randomin = 0

  xx = names(table(data.set$gr.bout.index)[table(data.set$gr.bout.index)==1])
  data.set = data.set[!data.set$gr.bout.index%in%xx,]

  for (i in unique(data.set$gr.bout.index)) {
    xx = which(data.set$gr.bout.index == i)
    nrlow = sample(xx, size = 1, prob = (data.set$llh.low[xx]))
    nrmedium = sample(xx,
                      size = 1,
                      prob = (data.set$llh.medium[xx]))
    nrhigh = sample(xx, size = 1, prob = (data.set$llh.high[xx]))
    nrlowin = sample(xx, size = 1, prob = 1 - (data.set$llh.low[xx]))
    nrmediumin = sample(xx,
                        size = 1,
                        prob = 1 - (data.set$llh.medium[xx]))
    nrhighin = sample(xx,
                      size = 1,
                      prob = 1 - (data.set$llh.high[xx]))
    nrrandom = sample(xx, size = 1)
    nrrandomin = sample(xx, size = 1)
    data.set$gr.initiated.low[nrlow] = 1
    data.set$gr.initiated.medium[nrmedium] = 1
    data.set$gr.initiated.high[nrhigh] = 1
    data.set$gr.initiated.lowin[nrlowin] = 1
    data.set$gr.initiated.mediumin[nrmediumin] = 1
    data.set$gr.initiated.highin[nrhighin] = 1
    data.set$gr.initiated.random[nrrandom] = 1
    data.set$gr.initiated.randomin[nrrandomin] = 1
  }

  data.set = data.set[order(data.set$gr.bout.index),]
  row.nr = 1:nrow(data.set)
  subs = as.numeric(row.nr >= (nrow(data.set) / 2)) + 1
  data.list = list(
    low.certainty = data.set[data.set$gr.initiated.low == 1, ],
    med.certainty = data.set[data.set$gr.initiated.medium == 1, ],
    high.certainty = data.set[data.set$gr.initiated.high == 1, ],
    low.certainty.change = data.set[data.set$gr.initiated.low == 1 &
                                      subs == 1 | data.set$gr.initiated.lowin == 1 & subs == 2, ],
    med.certainty.change = data.set[data.set$gr.initiated.medium == 1 &
                                      subs == 1 | data.set$gr.initiated.mediumin == 1 & subs == 2, ],
    high.certainty.change = data.set[data.set$gr.initiated.high == 1 &
                                       subs == 1 | data.set$gr.initiated.highin == 1 & subs == 2, ],
    random.certainty = data.set[data.set$gr.initiated.random == 1, ]
  )

  return(data.list)
})

names(sim.data) = nr.ids

sim.results = lapply(sim.data, function(n){
  part.results = lapply(1:length(n), function(m){
    dsi.data = n[[m]]
    dsi.data$Duration = 1
    for (i in 1:nrow(dsi.data)) {
      xx = rnorm(1000, mean = 60, sd = 50)
      xx = xx[xx > 20]
      dsi.data$Duration[i] = sample(xx, 1)
    }
    dsi.data$Sender = dsi.data$focal
    dsi.data$Receiver = dsi.data$pot.partner

    rand.dsi.data=dsi.data
    rand.dsi.data$chosen = 1
    rand.dsi.data=rand.dsi.data[complete.cases(rand.dsi.data),]
    rand.dsi.data$day.focal = unlist(lapply(unique(rand.dsi.data$date), function(i){
      xx = sample(unique(rand.dsi.data$focal[rand.dsi.data$date == i]), 1)
      xx = rep(xx, nrow(rand.dsi.data[rand.dsi.data$date == i,]))
      return(xx)
    }))
    rand.dsi.data=subset(rand.dsi.data, Sender == day.focal | Receiver == day.focal)

    subj.to.keep = unique(c(rand.dsi.data$Receiver, rand.dsi.data$Sender))
    dates = unique(rand.dsi.data$date)
    foc=sort(rep(subj.to.keep, times=length(subj.to.keep)))
    data.set=data.frame(individual1=foc, individual2=rep(subj.to.keep, times=length(subj.to.keep)), grooming.sent=0, observation.time=0)
    foc.dates=sort(rep(dates, times=nrow(data.set)))
    data.set=data.frame(individual1=rep(foc, times=length(dates)), individual2=rep(subj.to.keep, times=length(subj.to.keep)), date=as.Date(foc.dates), grooming.sent=0, grooming.interactions.sent=0, observation.time=0)

    data.set=subset(data.set, individual1!=individual2)
    data.set$dyad=apply(cbind(as.character(data.set$individual1), as.character(data.set$individual2)), 1, function(x){paste(sort(x), collapse="_")})

    for(i in 1:nrow(rand.dsi.data)){
      nr=which(data.set$individual1==rand.dsi.data$Sender[i] & data.set$individual2==rand.dsi.data$Receiver[i] & data.set$date==rand.dsi.data$date[i])
      nr2=which(data.set$individual2==rand.dsi.data$Sender[i] & data.set$individual1==rand.dsi.data$Receiver[i] & data.set$date==rand.dsi.data$date[i])
      data.set$grooming.sent[nr]=data.set$grooming.sent[nr]+rand.dsi.data$Duration[i]
      data.set$grooming.interactions.sent[nr]=data.set$grooming.interactions.sent[nr]+1
    }

    data.set$individual1=as.character(data.set$individual1)
    data.set$individual2=as.character(data.set$individual2)

    for(i in 1:length(unique(data.set$individual1))){
      ind1=unique(data.set$individual1)[i]
      ind.dates=unique(rand.dsi.data$date[rand.dsi.data$foc==ind1])
      data.set$observation.time[(data.set$individual1==ind1|data.set$individual2==ind1) & data.set$date%in%ind.dates]=data.set$observation.time[(data.set$individual1==ind1|data.set$individual2==ind1) & data.set$date%in%ind.dates]+12
    }

    observation.effort = c(1, 0.66, 0.33)
    observation.data = lapply(observation.effort, function(y){
      yy=sample(unique(data.set$date), size = length(unique(data.set$date)) * y)
      rand.data.set=subset(data.set, date%in%yy)
      return(rand.data.set)
    })

    names(observation.data) = c(1, 0.66, 0.33)

    consistency.frame = lapply(1:length(observation.data), function(x){
      xx = consistency(individual1 = observation.data[[x]]$individual1, individual2 = observation.data[[x]]$individual1, date = observation.data[[x]]$date, interactions = observation.data[[x]]$grooming.sent, observation.time = observation.data[[x]]$observation.time, k.seq = 0.02, j = 20, plot.col = 'black', behaviour = paste(c(names(n)[[m]], names(observation.data)[x]), collapse = ' '))
      xx.cons = xx$consistency
      xx.cons$observation.time = as.numeric(names(observation.data)[x])
      return(list(consist = xx.cons, plot = xx$plot))
    })
    consistency.plot = lapply(consistency.frame, function(x) x$plot)
    consistency.frame = lapply(consistency.frame, function(x) x$consist)
    consistency.frame = do.call(rbind, consistency.frame)
    consistency.frame = consistency.frame[complete.cases(consistency.frame),]

    consistency.frame$inverted = as.numeric(grepl(names(n)[[m]], pattern = 'change', fixed = T))
    consistency.frame$certainty = unlist(strsplit(names(n)[[m]], split = '.', fixed = T))[1]
    return(list(consistency.frame = consistency.frame, plot = consistency.plot))
  })
  part.plot = lapply(part.results, function(x) x$plot)
  part.results = lapply(part.results, function(x) x$consistency.frame)
  part.results = do.call(rbind, part.results)
  return(list(part.results = part.results, plot = part.plot))
})

sim.plot = lapply(sim.results, function(x) x$plot)
sim.results = lapply(sim.results, function(x) x$part.results)
all.results = lapply(1:length(sim.results), function(x){
  xx = sim.results[[x]]
  xx$individuals = names(sim.results)[x]
  xx$dyads = (as.numeric(names(sim.results)[x])^2 - as.numeric(names(sim.results)[x]))/2
  return(xx)
})

all.results = do.call(rbind, all.results)

all.results$interactions.dyad = all.results$interactions/all.results$dyads


####### plots
library(ggplot2)
library(tidyverse)
#show that number of individuals is linear

#### impact of number of individuals
standardisation.individuals10 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1 & individuals == 10)))$ind.int
standardisation.individuals10$individuals = 10
standardisation.individuals15 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1 & individuals == 18)))$ind.int
standardisation.individuals15$individuals = 18
standardisation.individuals20 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1 & individuals == 26)))$ind.int
standardisation.individuals20$individuals = 26
standardisation.individuals = rbind(standardisation.individuals10, standardisation.individuals15, standardisation.individuals20)

individual.impact = ggplot(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1) %>%
                             sample_frac(0.3, replace = T),
                           aes(x = interactions.dyad, y = cor.halves, color = individuals)) +
  geom_point(alpha = 0.6, size = 2) + ylim(0,1) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(standardisation.individuals, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(individuals)), size = 1.5) +
  geom_errorbar(standardisation.individuals, mapping = aes(x = average.interactions.per.dyad, y = average.median, ymin=average.median-sd, ymax=average.median+sd, color = as.factor(individuals)), width=.5,
                position=position_dodge(0.05)) +
  facet_grid(cols = vars(individuals)) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad') +
  scale_color_discrete(name = 'Number of Individuals')


#### impact of collection density

standardisation.collection1 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 10 & observation.time == 1)))$ind.int
standardisation.collection1$observation.time = 1
standardisation.collection0.66 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 10 & observation.time == 0.66)))$ind.int
standardisation.collection0.66$observation.time = 0.66
standardisation.collection0.33 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 10 & observation.time == 0.33)))$ind.int
standardisation.collection0.33$observation.time = 0.33
standardisation.collection = rbind(standardisation.collection0.33, standardisation.collection0.66, standardisation.collection1)

collection.impact = ggplot(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 10) %>% sample_frac(0.2, replace = T), aes(x = interactions.dyad, y = cor.halves, color = as.factor(observation.time))) +
  geom_point(alpha = 0.6, size = 2) + ylim(0,1) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(standardisation.collection, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(observation.time)), size = 1.5) +
  geom_errorbar(standardisation.collection, mapping = aes(x = average.interactions.per.dyad, y = average.median, ymin=average.median-sd, ymax=average.median+sd, color = as.factor(observation.time)), width=.5,
                position=position_dodge(0.05)) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad') +
  scale_color_discrete(name = 'Observation Time')


#### impact of certainty

standardisation.certaintyhigh = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 10 & observation.time == 1)))$ind.int
standardisation.certaintyhigh$certainty = 'high'
standardisation.certaintymedium = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'med' & individuals == 10 & observation.time == 1)))$ind.int
standardisation.certaintymedium$certainty = 'med'
standardisation.certaintylow = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'low' & individuals == 10 & observation.time == 1)))$ind.int
standardisation.certaintylow$certainty = 'low'
standardisation.certainty = rbind(standardisation.certaintylow, standardisation.certaintymedium, standardisation.certaintyhigh)

certainty.impact = ggplot(filter(all.results, inverted == 0 & observation.time == 1 & individuals == 10 & certainty != 'random') %>% sample_frac(0.2, replace = T), aes(x = interactions.dyad, y = cor.halves, color = as.factor(certainty))) +
  geom_point(alpha = 0.6, size = 2) + ylim(-0.5,1) +
  geom_line(standardisation.certainty, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(certainty)), size = 1.5) +
  geom_errorbar(standardisation.certainty, mapping = aes(x = average.interactions.per.dyad, y = average.median, ymin=average.median-sd, ymax=average.median+sd, color = as.factor(certainty)), width=.5,
                position=position_dodge(0.05)) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 1) +
  facet_grid(cols = vars(certainty)) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad') +
  scale_color_discrete(name = 'Certainty')



#### impact of collection conditions

all.results$condition = 'None'
all.results$condition[all.results$inverted == 1] = 'inverted'
all.results$condition[all.results$inverted == 0] = 'consistent'
all.results$condition[all.results$certainty == 'random'] = 'random'


standardisation.conditionconsistent = standardisation(consistency.frame = data.frame(filter(all.results, certainty == 'high' & individuals == 10 & observation.time == 1 & condition == 'consistent')))$ind.int
standardisation.conditionconsistent$condition = 'consistent'
standardisation.conditioninverted = standardisation(consistency.frame = data.frame(filter(all.results, certainty == 'high' & individuals == 10 & observation.time == 1 & condition == 'inverted')))$ind.int
standardisation.conditioninverted$condition = 'inverted'
standardisation.conditionrandom = standardisation(consistency.frame = data.frame(filter(all.results, certainty == 'random' & individuals == 10 & observation.time == 1)))$ind.int
standardisation.conditionrandom$condition = 'random'
standardisation.condition = rbind(standardisation.conditionrandom, standardisation.conditioninverted, standardisation.conditionconsistent)


condition.impact = ggplot(filter(all.results, observation.time == 1 &
                                   individuals == 10 &
                                   certainty %in% c('high', 'random') & condition != 'None') %>% sample_frac(0.2, replace = T),
                          aes(x = interactions.dyad, y = cor.halves, color = as.factor(condition))) +
  geom_point(alpha = 0.4, size = 2) + ylim(-0.5,1) +
  geom_line(standardisation.condition, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(condition)), size = 1.5) +
  geom_errorbar(standardisation.condition, mapping = aes(x = average.interactions.per.dyad, y = average.median, ymin=average.median-sd, ymax=average.median+sd, color = as.factor(condition)), width=.5,
                position=position_dodge(0.05)) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 1) +
  theme_minimal()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad') +
  facet_grid(cols = vars(condition)) +
  scale_color_discrete(name = 'Condition')

#
# library(gridExtra)
#
# tiff("E:/Workspace Leipzig/Consistency Paper/Simulation_ID.tiff", width = 6, height = 8, units = 'in', res = 300)
# grid.arrange(individual.impact, collection.impact)
# dev.off()