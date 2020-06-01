load("Paper 3 Prepared Data.RData")
source('consistency_function.R')
library(compiler)
consistency = cmpfun(consistency)


east.consistency.grooming = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$grooming.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green', behaviour = 'grooming')
east.consistency.aggression.contact = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$aggression.nonphysical.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'contact aggression')
east.consistency.aggression.noncontact = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$aggression.physical.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'noncontact aggression')
east.consistency.aggression = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$aggression.physical.sent + east.data.set$aggression.nonphysical.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'aggression')
east.consistency.proximity = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, undirectional = TRUE, date = east.data.set$date, interactions = east.data.set$proximity3m, observation.time = east.data.set$obs.time.without.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'proximity')
east.consistency.bodycontact = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2,undirectional = TRUE,  date = east.data.set$date, interactions = east.data.set$proximity, observation.time = east.data.set$obs.time.without.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'body contact')
east.consistency.foodshare = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$food.share.sent, observation.time = east.data.set$obs.time.without.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'food sharing')
east.consistency.pant = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$pant.grunt.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'pant grunt')

all.east = do.call(rbind, list(east.consistency.grooming$consistency,
                               east.consistency.aggression.contact$consistency,
                               east.consistency.aggression.noncontact$consistency,
                               east.consistency.aggression$consistency,
                               east.consistency.proximity$consistency,
                               east.consistency.bodycontact$consistency,
                               east.consistency.foodshare$consistency,
                               east.consistency.pant$consistency))
all.east$group = 'East'

ggplot(all.east, aes(x = interactions.per.dyad, y = cor.halves, color = behaviour, fill = behaviour)) +
  geom_point(alpha = 0.5) +
  geom_smooth(show.legend = F, method = 'loess', formula = y~x, na.rm = T, n = 100, level = 0.95, se = T) +
  ylim(-0.1,1) +
  ylab('Correlation between Random Halves') +
  xlab('Average Interactions per Dyad') +
  geom_hline(yintercept = 0.5, linetype = 2) +
  facet_grid(~behaviour, scales = 'free') +
  theme_bw()

east.stand = rbind(east.consistency.grooming$standardised,
                   east.consistency.bodycontact$standardised,
                   east.consistency.proximity$standardised,
                   east.consistency.aggression.contact$standardised,
                   east.consistency.aggression.noncontact$standardised,
                   east.consistency.aggression$standardised,
                   east.consistency.foodshare$standardised,
                   east.consistency.pant$standardised)
east.stand$behaviour = c('grooming', 'body contact', 'proximity', 'noncontact aggression', 'contact aggression', 'aggression', 'food share', 'pant grunt')


dsi.east.all = dsi.consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = cbind(east.data.set$grooming.sent, east.data.set$proximity, east.data.set$proximity3m, east.data.set$food.share.sent), observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green')
dsi.east.consistent = dsi.consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = cbind(east.data.set$grooming.sent, east.data.set$proximity, east.data.set$proximity3m), observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green')
dsi.east.grbc = dsi.consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = cbind(east.data.set$grooming.sent, east.data.set$proximity), observation.time = east.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green')


mang.consistency.grooming = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$grooming.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 100, plot.col = 'yellow', behaviour = 'grooming')
mang.consistency.aggression.contact = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$aggression.nonphysical.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 100, plot.col = 'yellow',behaviour = 'contact aggression')
mang.consistency.aggression.noncontact = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$aggression.physical.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 100, plot.col = 'yellow',behaviour = 'noncontact aggression')
mang.consistency.aggression = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$aggression.physical.sent + mang.data.set$aggression.nonphysical.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 100, plot.col = 'yellow',behaviour = 'aggression')
mang.consistency.proximity = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$proximity3m, undirectional = TRUE, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 50, plot.col = 'yellow',behaviour = 'proximity')
mang.consistency.bodycontact = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$proximity, undirectional = TRUE, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 100, plot.col = 'yellow',behaviour = 'body contact')
mang.consistency.supplant = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$supplant.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.02, j = 100, plot.col = 'yellow',behaviour = 'supplant')


all.mang = do.call(rbind, list(mang.consistency.grooming$consistency,
                               mang.consistency.aggression.contact$consistency,
                               mang.consistency.aggression.noncontact$consistency,
                               mang.consistency.aggression$consistency,
                               mang.consistency.proximity$consistency,
                               mang.consistency.bodycontact$consistency,
                               mang.consistency.supplant$consistency))
all.mang$group = 'Mangabey'

ggplot(all.mang, aes(x = interactions.per.individual, y = cor.halves, color = behaviour, fill = behaviour)) +
  geom_point(alpha = 0.5) +
  geom_smooth(show.legend = F, method = 'loess', formula = y~x, na.rm = T, n = 100, level = 0.95, se = T) +
  ylim(-0.1,1) +
  ylab('Correlation between Random Halves') +
  xlab('Average Interactions per Dyad') +
  geom_hline(yintercept = 0.5, linetype = 2) +
  facet_grid(~behaviour, scales = 'free') +
  theme_bw()


mang.stand = rbind(mang.consistency.grooming$standardised,
                   mang.consistency.bodycontact$standardised,
                   mang.consistency.proximity$standardised,
                   mang.consistency.aggression.contact$standardised,
                   mang.consistency.aggression.noncontact$standardised,
                   mang.consistency.aggression$standardised,
                   mang.consistency.supplant$standardised)
mang.stand$behaviour = c('grooming', 'body contact', 'proximity', 'noncontact aggression', 'contact aggression', 'aggression', 'supplant')

south.consistency.grooming = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$grooming.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'blue', behaviour = 'grooming')
south.consistency.aggression.contact = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$aggression.nonphysical.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'blue',behaviour = 'contact aggression')
south.consistency.aggression.noncontact = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$aggression.physical.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'blue',behaviour = 'noncontact aggression')
south.consistency.aggression = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$aggression.physical.sent + south.data.set$aggression.nonphysical.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'blue',behaviour = 'aggression')
south.consistency.proximity = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, undirectional = TRUE,  interactions = south.data.set$proximity3m, observation.time = south.data.set$obs.time.without.ass, k.seq = 0.02, j = 100, plot.col = 'blue',behaviour = 'proximity')
south.consistency.bodycontact = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, undirectional = TRUE, interactions = south.data.set$proximity, observation.time = south.data.set$obs.time.without.ass, k.seq = 0.02, j = 100, plot.col = 'blue',behaviour = 'body contact')
south.consistency.foodshare = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$food.share.sent, observation.time = south.data.set$obs.time.without.ass, k.seq = 0.02, j = 100, plot.col = 'blue',behaviour = 'food sharing')
south.consistency.pant = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$pant.grunt.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.02, j = 100, plot.col = 'green',behaviour = 'pant grunt')

all.south = do.call(rbind, list(south.consistency.grooming$consistency,
                               south.consistency.aggression.contact$consistency,
                               south.consistency.aggression.noncontact$consistency,
                               south.consistency.aggression$consistency,
                               south.consistency.proximity$consistency,
                               south.consistency.bodycontact$consistency,
                               south.consistency.foodshare$consistency,
                               south.consistency.pant$consistency))
all.south$group = 'South'

ggplot(all.south, aes(x = interactions.per.individual, y = cor.halves, color = behaviour, fill = behaviour)) +
  geom_point(alpha = 0.5) +
  geom_smooth(show.legend = F, method = 'loess', formula = y~x, na.rm = T, n = 100, level = 0.95, se = T) +
  ylim(-0.1,1) +
  ylab('Correlation between Random Halves') +
  xlab('Average Interactions per Individual') +
  geom_hline(yintercept = 0.5, linetype = 2) +
  facet_grid(~behaviour, scales = 'free') +
  theme_bw()


south.stand = rbind(south.consistency.grooming$standardised,
                   south.consistency.bodycontact$standardised,
                   south.consistency.proximity$standardised,
                   south.consistency.aggression.contact$standardised,
                   south.consistency.aggression.noncontact$standardised,
                   south.consistency.aggression$standardised,
                   south.consistency.foodshare$standardised,
                   south.consistency.pant$standardised)
south.stand$behaviour = c('grooming', 'body contact', 'proximity', 'noncontact aggression', 'contact aggression', 'aggression', 'food share', 'pant grunt')


south.means=aggregate(comp.south[,c("cor.groom","cor.prox","cor.dai","cor.prox3m","cor.aggtot", "cor.aggp", "cor.agg", "cor.food", "cor.pant", "groom.per.dyad", "prox.per.dyad", "prox3m.per.dyad", "aggtot.per.dyad","agg.per.dyad", "aggp.per.dyad", "food.per.dyad", "pant.per.dyad", "mean.obs.time")], by=list(comp.south$k), median, na.rm=T)
east.means=aggregate(comp.east[,c("cor.groom","cor.prox","cor.dai","cor.prox3m","cor.aggtot", "cor.aggp", "cor.agg", "cor.food", "cor.pant", "groom.per.dyad", "prox.per.dyad", "prox3m.per.dyad", "aggtot.per.dyad","agg.per.dyad", "aggp.per.dyad", "food.per.dyad", "pant.per.dyad",  "mean.obs.time")], by=list(comp.east$k), median, na.rm=T)
mang.means=aggregate(comp.mang[,c("cor.groom","cor.prox","cor.dai","cor.prox3m","cor.aggtot", "cor.aggp", "cor.agg", "cor.supplant", "groom.per.dyad", "prox.per.dyad", "prox3m.per.dyad", "aggtot.per.dyad","agg.per.dyad", "aggp.per.dyad", "supplant.per.dyad", "mean.obs.time")], by=list(comp.mang$k), median, na.rm=T)

south.ci=aggregate(comp.south[,c("cor.groom","cor.prox","cor.dai","cor.prox3m","cor.aggtot", "cor.aggp", "cor.agg", "cor.food", "cor.pant", "groom.per.dyad", "prox.per.dyad", "prox3m.per.dyad", "aggtot.per.dyad","agg.per.dyad", "aggp.per.dyad", "food.per.dyad", "pant.per.dyad", "mean.obs.time")], by=list(comp.south$k), quantile, probs = c(0.025, 0.975), na.rm=T)
east.ci=aggregate(comp.east[,c("cor.groom","cor.prox","cor.dai","cor.prox3m","cor.aggtot", "cor.aggp", "cor.agg", "cor.food", "cor.pant", "groom.per.dyad", "prox.per.dyad", "prox3m.per.dyad", "aggtot.per.dyad","agg.per.dyad", "aggp.per.dyad", "food.per.dyad", "pant.per.dyad", "mean.obs.time")], by=list(comp.east$k), quantile, probs = c(0.025, 0.975), na.rm=T)
mang.ci=aggregate(comp.mang[,c("cor.groom","cor.prox","cor.dai","cor.prox3m","cor.aggtot", "cor.aggp", "cor.agg", "cor.supplant", "groom.per.dyad", "prox.per.dyad", "prox3m.per.dyad", "aggtot.per.dyad","agg.per.dyad", "aggp.per.dyad", "supplant.per.dyad", "mean.obs.time")], by=list(comp.mang$k), quantile, probs = c(0.025, 0.975), na.rm=T)

test.frame.east$dyad.dir=apply(cbind(as.character(test.frame.east$id1), as.character(test.frame.east$id2)), 1, function(x){paste(x, collapse="_")})
test.frame.south$dyad.dir=apply(cbind(as.character(test.frame.south$id1), as.character(test.frame.south$id2)), 1, function(x){paste(x, collapse="_")})
test.frame.mang$dyad.dir=apply(cbind(as.character(test.frame.mang$id1), as.character(test.frame.mang$id2)), 1, function(x){paste(x, collapse="_")})

tiff("Y:/primint/Alex/Paper 3/Plots/Halves South.tiff", width = 8, height = 6, units = 'in', res = 300)
library(scales)
par(mfrow=c(2, 3))
par(mar=c(2,2,2,2))
plot(x=comp.south$groom.per.dyad, y=comp.south$cor.groom, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Grooming")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$prox.per.dyad, y=comp.south$cor.prox, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Body Contact")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$prox3m.per.dyad, y=comp.south$cor.prox3m, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Proximity")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$aggtot.per.dyad, y=comp.south$cor.aggtot, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Aggression")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$pant.per.dyad, y=comp.south$cor.pant, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Pant Grunts/Supplants")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$food.per.dyad, y=comp.south$cor.food, ylim=c(-0.1,1), xlim=c(0,2),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Food Share")
abline(h=0.5, lty=3, lwd=1)
dev.off()

library(scales)
tiff("Y:/primint/Alex/Paper 3/Plots/Halves All.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(2, 3))
par(mar=c(2,2,2,2))
plot(x=comp.east$groom.per.dyad, y=comp.east$cor.groom, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Grooming")
points(x=comp.south$groom.per.dyad, y=comp.south$cor.groom, pch=19, cex=1, col=alpha(col="blue", alpha=0.2))
points(x=comp.mang$groom.per.dyad, y=comp.mang$cor.groom, pch=19, cex=1, col=alpha(col="green", alpha=0.2))
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$prox.per.dyad, y=comp.east$cor.prox, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Body Contact")
points(x=comp.south$prox.per.dyad, y=comp.south$cor.prox, pch=19, cex=1, col=alpha(col="blue", alpha=0.2))
points(x=comp.mang$prox.per.dyad, y=comp.mang$cor.prox, pch=19, cex=1, col=alpha(col="green", alpha=0.2))
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$prox3m.per.dyad, y=comp.east$cor.prox3m, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Proximity")
points(x=comp.south$prox3m.per.dyad, y=comp.south$cor.prox3m, pch=19, cex=1, col=alpha(col="blue", alpha=0.2))
points(x=comp.mang$prox3m.per.dyad, y=comp.mang$cor.prox3m, pch=19, cex=1, col=alpha(col="green", alpha=0.2))
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$aggtot.per.dyad, y=comp.east$cor.aggtot, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Aggression")
points(x=comp.south$aggtot.per.dyad, y=comp.south$cor.aggtot, pch=19, cex=1, col=alpha(col="blue", alpha=0.2))
points(x=comp.mang$aggtot.per.dyad, y=comp.mang$cor.aggtot, pch=19, cex=1, col=alpha(col="green", alpha=0.2))
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$pant.per.dyad, y=comp.east$cor.pant, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Pant Grunts/Supplants")
points(x=comp.south$pant.per.dyad, y=comp.south$cor.pant, pch=19, cex=1, col=alpha(col="blue", alpha=0.2))
points(x=comp.mang$supplant.per.dyad, y=comp.mang$cor.supplant, pch=19, cex=1, col=alpha(col="green", alpha=0.2))
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$food.per.dyad, y=comp.east$cor.food, ylim=c(-0.1,1), xlim=c(0,2),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Food Share")
points(x=comp.south$food.per.dyad, y=comp.south$cor.food, pch=19, cex=1, col=alpha(col="blue", alpha=0.2))
legend("topright", col = c("green", "darkgoldenrod3", "blue"), legend = c("Mangabey", "East", "South"), pch=19)
abline(h=0.5, lty=3, lwd=1)
dev.off()
library(scales)
tiff("Y:/primint/Alex/Paper 3/Plots/Labels.tiff", width = 8, height = 6, units = 'in', res = 300)
plot(x=comp.east$food.per.dyad, y=comp.east$cor.food, ylim=c(-0.1,1), xlim=c(0,2),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Food Share")
dev.off()


tiff("Y:/primint/Alex/Paper 3/Plots/Halves Mangabey.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(2, 2), mar=c(2,2,2,2))
plot(x=comp.mang$groom.per.dyad, y=comp.mang$cor.groom, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1, col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Grooming")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.mang$prox.per.dyad, y=comp.mang$cor.prox, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1, col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Body Contact")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.mang$aggtot.per.dyad, y=comp.mang$cor.aggtot, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Noncontact Aggression")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.mang$aggp.per.dyad, y=comp.mang$cor.aggp, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1,  col=alpha(col="darkgoldenrod3", alpha=0.2),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Contact Aggression")
abline(h=0.5, lty=3, lwd=1)
dev.off()



library("scales", lib.loc="~/R/win-library/3.5")
tiff("Y:/primint/Alex/Paper 3/Plots/Comparison Grooming.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(1,3),mar=c(4,4,2,2))
plot(x=comp.mang$groom.per.dyad, y=comp.mang$cor.groom, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1, col=scales::alpha("blue", alpha=0.4),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Mangabey")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$groom.per.dyad, y=comp.east$cor.groom, ylim=c(-0.1,1), xlim=c(0,10), pch=19, cex=1, col=scales::alpha("red", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "East")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$groom.per.dyad, y=comp.south$cor.groom, ylim=c(-0.1,1), xlim=c(0,10), pch=19, cex=1, col=scales::alpha("black", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "South")
abline(h=0.5, lty=3, lwd=1)
dev.off()

tiff("Y:/primint/Alex/Paper 3/Plots/Comparison Aggression.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(1,3),mar=c(4,4,2,2))
plot(x=comp.mang$aggtot.per.dyad, y=comp.mang$cor.aggtot, ylim=c(-0.1,1), xlim=c(0,10),pch=19, cex=1, col=scales::alpha("blue", alpha=0.4),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Mangabey")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$aggtot.per.dyad, y=comp.east$cor.aggtot, ylim=c(-0.1,1), xlim=c(0,10), pch=19, cex=1, col=scales::alpha("red", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "East")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$aggtot.per.dyad, y=comp.south$cor.aggtot, ylim=c(-0.1,1), xlim=c(0,10), pch=19, cex=1, col=scales::alpha("black", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "South")
abline(h=0.5, lty=3, lwd=1)
dev.off()

tiff("Y:/primint/Alex/Paper 3/Plots/Comparison Contact.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(1,3),mar=c(4,4,2,2))
plot(x=comp.mang$prox.per.dyad, y=comp.mang$cor.prox, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=scales::alpha("blue", alpha=0.4),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Mangabey")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$prox.per.dyad, y=comp.east$cor.prox, ylim=c(-0.1,1), xlim=c(0,30), pch=19, cex=1, col=scales::alpha("red", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "East")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$prox.per.dyad, y=comp.south$cor.prox, ylim=c(-0.1,1), xlim=c(0,30), pch=19, cex=1, col=scales::alpha("black", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "South")
abline(h=0.5, lty=3, lwd=1)
dev.off()

tiff("Y:/primint/Alex/Paper 3/Plots/Comparison Proximity.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(1,3),mar=c(4,4,2,2))
plot(x=comp.mang$prox3m.per.dyad, y=comp.mang$cor.prox3m, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=scales::alpha("blue", alpha=0.4),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Mangabey")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$prox3m.per.dyad, y=comp.east$cor.prox3m, ylim=c(-0.1,1), xlim=c(0,30), pch=19, cex=1, col=scales::alpha("red", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "East")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$prox3m.per.dyad, y=comp.south$cor.prox3m, ylim=c(-0.1,1), xlim=c(0,30), pch=19, cex=1, col=scales::alpha("black", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "South")
abline(h=0.5, lty=3, lwd=1)
dev.off()

tiff("Y:/primint/Alex/Paper 3/Plots/Comparison Pant.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mfrow=c(1,3),mar=c(4,4,2,2))
plot(x=comp.mang$supplant.per.dyad, y=comp.mang$cor.supplant, ylim=c(-0.1,1), xlim=c(0,30),pch=19, cex=1, col=scales::alpha("blue", alpha=0.4),  ylab="Spearman rank correlation coefficient", xlab="Interactions per Dyad", main = "Mangabey")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.east$pant.per.dyad, y=comp.east$cor.pant, ylim=c(-0.1,1), xlim=c(0,30), pch=19, cex=1, col=scales::alpha("red", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "East")
abline(h=0.5, lty=3, lwd=1)
plot(x=comp.south$pant.per.dyad, y=comp.south$cor.pant, ylim=c(-0.1,1), xlim=c(0,30), pch=19, cex=1, col=scales::alpha("black", alpha=0.4),  ylab="", xlab="Interactions per Dyad", main = "South")
abline(h=0.5, lty=3, lwd=1)
dev.off()

#
# tiff("Y:/primint/Alex/Paper 3/Plots/Consistency all.tiff", width = 14, height = 10, units = 'in', res = 300)
# par(mfrow=c(1, 3), mar=c(4,5,2,4))
#####Mangabey Summary
tiff("Y:/primint/Alex/Paper 3/Plots/Consistency Mangabey.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mar=c(4,3,2,6), xpd=F)
plot(x=0, y=0, ylim=c(-0.1,1), xlim=c(0,1), pch=19, col="white", ylab="Consistency", xlab="Subset Size", main = "Mangabey", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
abline(h=0.5, lty=3, lwd=1)
lines(x=(mang.means$groom.per.dyad/max(mang.means$groom.per.dyad)), y=mang.means$cor.groom, lwd=2, lty=1, col="blue")
polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.groom[, 1], rev(mang.ci$cor.groom[, 2])),
        border=NA, col=adjustcolor(col=c("blue"), alpha.f=0.3))
# lines(x=(mang.means$food.per.dyad/max(mang.means$food.per.dyad)), y=mang.means$cor.food, lwd=2, lty=1, col="red")
# polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.food[, 1], rev(mang.ci$cor.food[, 2])),
#         border=NA, col=adjustcolor(col=c("red"), alpha.f=0.3))
lines(x=(mang.means$aggtot.per.dyad/max(mang.means$aggtot.per.dyad)), y=mang.means$cor.aggtot, lwd=2, lty=1, col="darkgreen")
polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.aggtot[, 1], rev(mang.ci$cor.aggtot[, 2])),
        border=NA, col=adjustcolor(col=c("darkgreen"), alpha.f=0.3))
# lines(x=(mang.means$aggp.per.dyad/max(mang.means$aggp.per.dyad)), y=mang.means$cor.aggp, lwd=2, lty=1, col="green")
# polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.aggp[, 1], rev(mang.ci$cor.aggp[, 2])),
#         border=NA, col=adjustcolor(col=c("green"), alpha.f=0.3))
# lines(x=(mang.means$agg.per.dyad/max(mang.means$agg.per.dyad)), y=mang.means$cor.agg, lwd=2, lty=1, col="darkgreen")
# polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.agg[, 1], rev(mang.ci$cor.agg[, 2])),
#         border=NA, col=adjustcolor(col=c("darkgreen"), alpha.f=0.3))
lines(x=(mang.means$prox.per.dyad/max(mang.means$prox.per.dyad)), y=mang.means$cor.prox, lwd=2, lty=1, col="sandybrown")
polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.prox[, 1], rev(mang.ci$cor.prox[, 2])),
        border=NA, col=adjustcolor(col=c("sandybrown"), alpha.f=0.3))
lines(x=(mang.means$prox3m.per.dyad/max(mang.means$prox3m.per.dyad)), y=mang.means$cor.prox3m, lwd=2, lty=1, col="brown")
polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.prox3m[, 1], rev(mang.ci$cor.prox3m[, 2])),
        border=NA, col=adjustcolor(col=c("brown"), alpha.f=0.3))
lines(x=(mang.means$supplant.per.dyad/max(mang.means$supplant.per.dyad)), y=mang.means$cor.supplant, lwd=2, lty=1, col="purple")
polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.supplant[, 1], rev(mang.ci$cor.supplant[, 2])),
        border=NA, col=adjustcolor(col=c("purple"), alpha.f=0.3))
lines(x=(mang.means$mean.obs.time/max(mang.means$mean.obs.time)), y=mang.means$cor.dai, lwd=2, lty=1, col="grey")
polygon(x=c(mang.ci$Group.1, rev(mang.ci$Group.1)), y=c(mang.ci$cor.dai[, 1], rev(mang.ci$cor.dai[, 2])),
        border=NA, col=grey(level=0.6, alpha=0.2))
# legend("topright", inset=c(-0.2,0), legend=c("Grooming","Agg Contact", "Agg Noncontact", "Close Contact", "Proximity", "DAI"), lwd=2,cex = 0.65, col = c("blue","green", "darkgreen", "sandybrown", "brown", "grey))
# dev.off()


#####East Summary
# tiff("Y:/primint/Alex/Paper 3/Plots/Consistency East.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mar=c(4,5,2,4), xpd=F)
plot(x=0, y=0, ylim=c(-0.1,1), xlim=c(0,1), pch=19, col="white", ylab="Consistency", main = "East", xlab="Subset Size", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
abline(h=0.5, lty=3, lwd=1)
lines(x=(east.means$groom.per.dyad/max(east.means$groom.per.dyad)), y=east.means$cor.groom, lwd=2, lty=1, col="blue")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.groom[, 1], rev(east.ci$cor.groom[, 2])),
        border=NA, col=adjustcolor(col=c("blue"), alpha.f=0.3))
lines(x=(east.means$food.per.dyad/max(east.means$food.per.dyad)), y=east.means$cor.food, lwd=2, lty=1, col="red")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.food[, 1], rev(east.ci$cor.food[, 2])),
        border=NA, col=adjustcolor(col=c("red"), alpha.f=0.3))
# lines(x=(east.means$aggtot.per.dyad/max(east.means$aggtot.per.dyad)), y=east.means$cor.aggtot, lwd=2, lty=1, col="lawngreen")
# polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.aggtot[, 1], rev(east.ci$cor.aggtot[, 2])),
#         border=NA, col=adjustcolor(col=c("lawngreen"), alpha.f=0.3))
lines(x=(east.means$aggp.per.dyad/max(east.means$aggp.per.dyad)), y=east.means$cor.aggp, lwd=2, lty=1, col="green")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.aggp[, 1], rev(east.ci$cor.aggp[, 2])),
        border=NA, col=adjustcolor(col=c("green"), alpha.f=0.3))
lines(x=(east.means$agg.per.dyad/max(east.means$agg.per.dyad)), y=east.means$cor.agg, lwd=2, lty=1, col="darkgreen")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.agg[, 1], rev(east.ci$cor.agg[, 2])),
        border=NA, col=adjustcolor(col=c("darkgreen"), alpha.f=0.3))
lines(x=(east.means$prox.per.dyad/max(east.means$prox.per.dyad)), y=east.means$cor.prox, lwd=2, lty=1, col="sandybrown")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.prox[, 1], rev(east.ci$cor.prox[, 2])),
        border=NA, col=adjustcolor(col=c("sandybrown"), alpha.f=0.3))
lines(x=(east.means$prox3m.per.dyad/max(east.means$prox3m.per.dyad)), y=east.means$cor.prox3m, lwd=2, lty=1, col="brown")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.prox3m[, 1], rev(east.ci$cor.prox3m[, 2])),
        border=NA, col=adjustcolor(col=c("brown"), alpha.f=0.3))
lines(x=(east.means$pant.per.dyad/max(east.means$pant.per.dyad)), y=east.means$cor.pant, lwd=2, lty=1, col="purple")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.pant[, 1], rev(east.ci$cor.pant[, 2])),
        border=NA, col=adjustcolor(col=c("purple"), alpha.f=0.3))
lines(x=(east.means$mean.obs.time/max(east.means$mean.obs.time)), y=east.means$cor.dai, lwd=2, lty=1, col="grey")
polygon(x=c(east.ci$Group.1, rev(east.ci$Group.1)), y=c(east.ci$cor.dai[, 1], rev(east.ci$cor.dai[, 2])),
        border=NA, col=grey(level=0.6, alpha=0.2))
# legend("topright", inset=c(-0.2,0), legend=c("Grooming","Food Share", "Agg Contact", "Agg Noncontact", "Close Contact", "Proximity"), lwd=2,cex = 0.65, col = c("blue","red","green", "darkgreen", "sandybrown", "brown"))
# dev.off()



#####South Summary
# tiff("Y:/primint/Alex/Paper 3/Plots/Consistency South.tiff", width = 8, height = 6, units = 'in', res = 300)
par(mar=c(4,5,2,4), xpd=F)
plot(x=0, y=0, ylim=c(-0.1,1), xlim=c(0,1), pch=19, col="white",  ylab="Consistency", xlab="Subset Size", main = "South", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
abline(h=0.5, lty=3, lwd=1)
lines(x=(south.means$groom.per.dyad/max(south.means$groom.per.dyad)), y=south.means$cor.groom, lwd=2, lty=1, col="blue")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.groom[, 1], rev(south.ci$cor.groom[, 2])),
        border=NA, col=adjustcolor(col=c("blue"), alpha.f=0.3))
lines(x=(south.means$food.per.dyad/max(south.means$food.per.dyad)), y=south.means$cor.food, lwd=2, lty=1, col="red")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.food[, 1], rev(south.ci$cor.food[, 2])),
        border=NA, col=adjustcolor(col=c("red"), alpha.f=0.3))
# lines(x=(south.means$aggtot.per.dyad/max(south.means$aggtot.per.dyad)), y=south.means$cor.aggtot, lwd=2, lty=1, col="lawngreen")
# polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.aggtot[, 1], rev(south.ci$cor.aggtot[, 2])),
#         border=NA, col=adjustcolor(col=c("lawngreen"), alpha.f=0.3))
lines(x=(south.means$aggp.per.dyad/max(south.means$aggp.per.dyad)), y=south.means$cor.aggp, lwd=2, lty=1, col="green")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.aggp[, 1], rev(south.ci$cor.aggp[, 2])),
        border=NA, col=adjustcolor(col=c("green"), alpha.f=0.3))
lines(x=(south.means$agg.per.dyad/max(south.means$agg.per.dyad)), y=south.means$cor.agg, lwd=2, lty=1, col="darkgreen")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.agg[, 1], rev(south.ci$cor.agg[, 2])),
        border=NA, col=adjustcolor(col=c("darkgreen"), alpha.f=0.3))
lines(x=(south.means$prox.per.dyad/max(south.means$prox.per.dyad)), y=south.means$cor.prox, lwd=2, lty=1, col="sandybrown")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.prox[, 1], rev(south.ci$cor.prox[, 2])),
        border=NA, col=adjustcolor(col=c("sandybrown"), alpha.f=0.3))
lines(x=(south.means$prox3m.per.dyad/max(south.means$prox3m.per.dyad)), y=south.means$cor.prox3m, lwd=2, lty=1, col="brown")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.prox3m[, 1], rev(south.ci$cor.prox3m[, 2])),
        border=NA, col=adjustcolor(col=c("brown"), alpha.f=0.3))
lines(x=(south.means$pant.per.dyad/max(south.means$pant.per.dyad)), y=south.means$cor.pant, lwd=2, lty=1, col="purple")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.pant[, 1], rev(south.ci$cor.pant[, 2])),
        border=NA, col=adjustcolor(col=c("purple"), alpha.f=0.3))
lines(x=(south.means$mean.obs.time/max(south.means$mean.obs.time)), y=south.means$cor.dai, lwd=2, lty=1, col="grey")
polygon(x=c(south.ci$Group.1, rev(south.ci$Group.1)), y=c(south.ci$cor.dai[, 1], rev(south.ci$cor.dai[, 2])),
        border=NA, col=grey(level=0.6, alpha=0.2))
# legend("topright", inset=c(-0.2,0), legend=c("Grooming","Food Share", "Agg Contact", "Agg Noncontact", "Close Contact", "Proximity"), lwd=2,cex = 0.65, col = c("blue","red","green", "darkgreen", "sandybrown", "brown"))
dev.off()

tiff("Y:/primint/Alex/Paper 3/Plots/Consistency legend.tiff", width = 10, height = 10, units = 'in', res = 300)
par(mar=c(4,4,4,4))
plot(x=0, y=0, ylim=c(0,1), xlim=c(0,1), pch=19, col="white",  ylab="Consistency", xlab="", main = "Consistencies Mangabey")
legend("topleft", legend=c("Grooming","Food Share", "Agg Contact", "Agg Noncontact", "Close Contact", "Proximity", "Pant Grunt", "DAI"), lwd=2,cex = 1, col = c("blue","red","green", "darkgreen", "sandybrown", "brown", "purple", "grey"))
dev.off()

#
#
#


#### Make result table

comp.east.s=comp.east
comp.east.s[, 2:8]=round(comp.east[, 2:8], 1)
comp.east.s$cor.aggtot=round(comp.east$cor.aggtot, 1)
comp.east.s$cor.food=round(comp.east$cor.food, 1)
comp.east.s$cor.pant=round(comp.east$cor.pant, 1)
xx.east.groom = aggregate(comp.east.s$groom.per.dyad, by = list(comp.east.s$cor.groom), median, na.rm = T)
xx.east.aggtot = aggregate(comp.east.s$aggtot.per.dyad, by = list(comp.east.s$cor.aggtot), median, na.rm = T)
xx.east.aggp = aggregate(comp.east.s$aggp.per.dyad, by = list(comp.east.s$cor.aggp), median, na.rm = T)
xx.east.agg = aggregate(comp.east.s$agg.per.dyad, by = list(comp.east.s$cor.agg), median, na.rm = T)
xx.east.prox = aggregate(comp.east.s$prox.per.dyad, by = list(comp.east.s$cor.prox), median, na.rm = T)
xx.east.prox3m = aggregate(comp.east.s$prox3m.per.dyad, by = list(comp.east.s$cor.prox3m), median, na.rm = T)
xx.east.food = aggregate(comp.east.s$food.per.dyad, by = list(comp.east.s$cor.food), median, na.rm = T)
xx.east.pant = aggregate(comp.east.s$pant.per.dyad, by = list(comp.east.s$cor.pant), median, na.rm = T)
sd.east.groom = aggregate(comp.east.s$groom.per.dyad, by = list(comp.east.s$cor.groom), sd, na.rm = T)
sd.east.aggtot = aggregate(comp.east.s$aggtot.per.dyad, by = list(comp.east.s$cor.aggtot), sd, na.rm = T)
sd.east.aggp = aggregate(comp.east.s$aggp.per.dyad, by = list(comp.east.s$cor.aggp), sd, na.rm = T)
sd.east.agg = aggregate(comp.east.s$agg.per.dyad, by = list(comp.east.s$cor.agg), sd, na.rm = T)
sd.east.prox = aggregate(comp.east.s$prox.per.dyad, by = list(comp.east.s$cor.prox), sd, na.rm = T)
sd.east.prox3m = aggregate(comp.east.s$prox3m.per.dyad, by = list(comp.east.s$cor.prox3m), sd, na.rm = T)
sd.east.food = aggregate(comp.east.s$food.per.dyad, by = list(comp.east.s$cor.food), sd, na.rm = T)
sd.east.pant = aggregate(comp.east.s$pant.per.dyad, by = list(comp.east.s$cor.pant), sd, na.rm = T)

results=data.frame(group=sort(rep(c("East","South","Mangabey"), times=8)), behav=rep(sort(c(behaviours,"Aggression.total")),3), median.interactions=NA, sd.interactions=NA, cor.full=NA, interactions=NA)
results$behav=as.character(results$behav)
results$behav[results$group=="East" & results$behav=="Supplant"]="Pant Grunt"
results$median.interactions[results$group=="East" & results$behav=="Grooming"]=xx.east.groom$x[which(xx.east.groom$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Proximity"]=xx.east.prox$x[which(xx.east.prox$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Proximity3m"]=xx.east.prox3m$x[which(xx.east.prox3m$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Aggression.nonphysical"]=xx.east.agg$x[which(xx.east.agg$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Aggression.total"]=xx.east.aggtot$x[which(xx.east.aggtot$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Aggression.physical"]=xx.east.aggp$x[which(xx.east.aggp$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Food Share"]=xx.east.food$x[which(xx.east.food$Group.1 == 0.5)]
results$median.interactions[results$group=="East" & results$behav=="Pant Grunt"]=xx.east.pant$x[which(xx.east.pant$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Grooming"]=sd.east.groom$x[which(sd.east.groom$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Proximity"]=sd.east.prox$x[which(sd.east.prox$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Proximity3m"]=sd.east.prox3m$x[which(sd.east.prox3m$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Aggression.nonphysical"]=sd.east.agg$x[which(sd.east.agg$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Aggression.total"]=sd.east.aggtot$x[which(sd.east.aggtot$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Aggression.physical"]=sd.east.aggp$x[which(sd.east.aggp$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Food Share"]=sd.east.food$x[which(sd.east.food$Group.1 == 0.5)]
results$sd.interactions[results$group=="East" & results$behav=="Pant Grunt"]=sd.east.pant$x[which(sd.east.pant$Group.1 == 0.5)]
results$cor.full[results$group=="East" & results$behav=="Grooming"]=mean(comp.east$cor.groom[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Proximity"]=mean(comp.east$cor.prox[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Proximity3m"]=mean(comp.east$cor.prox3m[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Aggression.nonphysical"]=mean(comp.east$cor.agg[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Aggression.total"]=mean(comp.east$cor.aggtot[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Aggression.physical"]=mean(comp.east$cor.aggp[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Food Share"]=mean(comp.east$cor.food[comp.east$k==1])
results$cor.full[results$group=="East" & results$behav=="Pant Grunt"]=mean(comp.east$cor.pant[comp.east$k==1])
results$interactions[results$group=="East" & results$behav=="Grooming"]=sum(east.data.set$grooming.interactions.sent, na.rm = T)
results$interactions[results$group=="East" & results$behav=="Proximity"]=sum(east.data.set$proximity.interactions, na.rm = T)/2
results$interactions[results$group=="East" & results$behav=="Proximity3m"]=sum(east.data.set$proximity3m.interactions, na.rm = T)/2
results$interactions[results$group=="East" & results$behav=="Aggression.nonphysical"]=sum(east.data.set$aggression.nonphysical.sent, na.rm = T)
results$interactions[results$group=="East" & results$behav=="Aggression.total"]=sum(east.data.set$aggression.physical.sent+east.data.set$aggression.nonphysical.sent, na.rm = T)
results$interactions[results$group=="East" & results$behav=="Aggression.physical"]=sum(east.data.set$aggression.physical.sent, na.rm = T)
results$interactions[results$group=="East" & results$behav=="Food Share"]=sum(east.data.set$food.share.sent, na.rm = T)
results$interactions[results$group=="East" & results$behav=="Pant Grunt"]=sum(east.data.set$pant.grunt.sent, na.rm = T)





comp.south.s=comp.south
comp.south.s[, 2:8]=round(comp.south[, 2:8], 1)
comp.south.s$cor.aggtot=round(comp.south$cor.aggtot, 1)
comp.south.s$cor.food=round(comp.south$cor.food, 1)
comp.south.s$cor.pant=round(comp.south$cor.pant, 1)
xx.south.groom = aggregate(comp.south.s$groom.per.dyad, by = list(comp.south.s$cor.groom), median, na.rm = T)
xx.south.aggtot = aggregate(comp.south.s$aggtot.per.dyad, by = list(comp.south.s$cor.aggtot), median, na.rm = T)
xx.south.aggp = aggregate(comp.south.s$aggp.per.dyad, by = list(comp.south.s$cor.aggp), median, na.rm = T)
xx.south.agg = aggregate(comp.south.s$agg.per.dyad, by = list(comp.south.s$cor.agg), median, na.rm = T)
xx.south.prox = aggregate(comp.south.s$prox.per.dyad, by = list(comp.south.s$cor.prox), median, na.rm = T)
xx.south.prox3m = aggregate(comp.south.s$prox3m.per.dyad, by = list(comp.south.s$cor.prox3m), median, na.rm = T)
xx.south.food = aggregate(comp.south.s$food.per.dyad, by = list(comp.south.s$cor.food), median, na.rm = T)
xx.south.pant = aggregate(comp.south.s$pant.per.dyad, by = list(comp.south.s$cor.pant), median, na.rm = T)
sd.south.groom = aggregate(comp.south.s$groom.per.dyad, by = list(comp.south.s$cor.groom), sd, na.rm = T)
sd.south.aggtot = aggregate(comp.south.s$aggtot.per.dyad, by = list(comp.south.s$cor.aggtot), sd, na.rm = T)
sd.south.aggp = aggregate(comp.south.s$aggp.per.dyad, by = list(comp.south.s$cor.aggp), sd, na.rm = T)
sd.south.agg = aggregate(comp.south.s$agg.per.dyad, by = list(comp.south.s$cor.agg), sd, na.rm = T)
sd.south.prox = aggregate(comp.south.s$prox.per.dyad, by = list(comp.south.s$cor.prox), sd, na.rm = T)
sd.south.prox3m = aggregate(comp.south.s$prox3m.per.dyad, by = list(comp.south.s$cor.prox3m), sd, na.rm = T)
sd.south.food = aggregate(comp.south.s$food.per.dyad, by = list(comp.south.s$cor.food), sd, na.rm = T)
sd.south.pant = aggregate(comp.south.s$pant.per.dyad, by = list(comp.south.s$cor.pant), sd, na.rm = T)

results$behav[results$group=="South" & results$behav=="Supplant"]="Pant Grunt"
results$median.interactions[results$group=="South" & results$behav=="Grooming"]=xx.south.groom$x[which(xx.south.groom$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Proximity"]=xx.south.prox$x[which(xx.south.prox$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Proximity3m"]=xx.south.prox3m$x[which(xx.south.prox3m$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Aggression.nonphysical"]=xx.south.agg$x[which(xx.south.agg$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Aggression.total"]=xx.south.aggtot$x[which(xx.south.aggtot$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Aggression.physical"]=xx.south.aggp$x[which(xx.south.aggp$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Food Share"]=xx.south.food$x[which(xx.south.food$Group.1 == 0.5)]
results$median.interactions[results$group=="South" & results$behav=="Pant Grunt"]=xx.south.pant$x[which(xx.south.pant$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Grooming"]=sd.south.groom$x[which(sd.south.groom$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Proximity"]=sd.south.prox$x[which(sd.south.prox$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Proximity3m"]=sd.south.prox3m$x[which(sd.south.prox3m$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Aggression.nonphysical"]=sd.south.agg$x[which(sd.south.agg$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Aggression.total"]=sd.south.aggtot$x[which(sd.south.aggtot$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Aggression.physical"]=sd.south.aggp$x[which(sd.south.aggp$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Food Share"]=sd.south.food$x[which(sd.south.food$Group.1 == 0.5)]
results$sd.interactions[results$group=="South" & results$behav=="Pant Grunt"]=sd.south.pant$x[which(sd.south.pant$Group.1 == 0.5)]
results$cor.full[results$group=="South" & results$behav=="Grooming"]=mean(comp.south$cor.groom[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Proximity"]=mean(comp.south$cor.prox[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Proximity3m"]=mean(comp.south$cor.prox3m[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Aggression.nonphysical"]=mean(comp.south$cor.agg[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Aggression.total"]=mean(comp.south$cor.aggtot[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Aggression.physical"]=mean(comp.south$cor.aggp[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Food Share"]=mean(comp.south$cor.food[comp.south$k==1])
results$cor.full[results$group=="South" & results$behav=="Pant Grunt"]=mean(comp.south$cor.pant[comp.south$k==1])
results$interactions[results$group=="South" & results$behav=="Grooming"]=sum(south.data.set$grooming.interactions.sent, na.rm = T)
results$interactions[results$group=="South" & results$behav=="Proximity"]=sum(south.data.set$proximity.interactions, na.rm = T)/2
results$interactions[results$group=="South" & results$behav=="Proximity3m"]=sum(south.data.set$proximity3m.interactions, na.rm = T)/2
results$interactions[results$group=="South" & results$behav=="Aggression.nonphysical"]=sum(south.data.set$aggression.nonphysical.sent, na.rm = T)
results$interactions[results$group=="South" & results$behav=="Aggression.total"]=sum(south.data.set$aggression.physical.sent+south.data.set$aggression.nonphysical.sent, na.rm = T)
results$interactions[results$group=="South" & results$behav=="Aggression.physical"]=sum(south.data.set$aggression.physical.sent, na.rm = T)
results$interactions[results$group=="South" & results$behav=="Food Share"]=sum(south.data.set$food.share.sent, na.rm = T)
results$interactions[results$group=="South" & results$behav=="Pant Grunt"]=sum(south.data.set$pant.grunt.sent, na.rm = T)




comp.mang.s=comp.mang
comp.mang.s[, 2:8]=round(comp.mang[, 2:8], 1)
comp.mang.s$cor.aggtot=round(comp.mang$cor.aggtot, 1)
comp.mang.s$cor.supplant=round(comp.mang$cor.supplant, 1)
xx.mang.groom = aggregate(comp.mang.s$groom.per.dyad, by = list(comp.mang.s$cor.groom), median, na.rm = T)
xx.mang.aggtot = aggregate(comp.mang.s$aggtot.per.dyad, by = list(comp.mang.s$cor.aggtot), median, na.rm = T)
xx.mang.aggp = aggregate(comp.mang.s$aggp.per.dyad, by = list(comp.mang.s$cor.aggp), median, na.rm = T)
xx.mang.agg = aggregate(comp.mang.s$agg.per.dyad, by = list(comp.mang.s$cor.agg), median, na.rm = T)
xx.mang.prox = aggregate(comp.mang.s$prox.per.dyad, by = list(comp.mang.s$cor.prox), median, na.rm = T)
xx.mang.prox3m = aggregate(comp.mang.s$prox3m.per.dyad, by = list(comp.mang.s$cor.prox3m), median, na.rm = T)
xx.mang.pant = aggregate(comp.mang.s$supplant.per.dyad, by = list(comp.mang.s$cor.supplant), median, na.rm = T)
sd.mang.groom = aggregate(comp.mang.s$groom.per.dyad, by = list(comp.mang.s$cor.groom), sd, na.rm = T)
sd.mang.aggtot = aggregate(comp.mang.s$aggtot.per.dyad, by = list(comp.mang.s$cor.aggtot), sd, na.rm = T)
sd.mang.aggp = aggregate(comp.mang.s$aggp.per.dyad, by = list(comp.mang.s$cor.aggp), sd, na.rm = T)
sd.mang.agg = aggregate(comp.mang.s$agg.per.dyad, by = list(comp.mang.s$cor.agg), sd, na.rm = T)
sd.mang.prox = aggregate(comp.mang.s$prox.per.dyad, by = list(comp.mang.s$cor.prox), sd, na.rm = T)
sd.mang.prox3m = aggregate(comp.mang.s$prox3m.per.dyad, by = list(comp.mang.s$cor.prox3m), sd, na.rm = T)
sd.mang.pant = aggregate(comp.mang.s$supplant.per.dyad, by = list(comp.mang.s$cor.supplant), sd, na.rm = T)

results$median.interactions[results$group=="Mangabey" & results$behav=="Grooming"]=xx.mang.groom$x[which(xx.mang.groom$Group.1 == 0.5)]
results$median.interactions[results$group=="Mangabey" & results$behav=="Proximity"]=xx.mang.prox$x[which(xx.mang.prox$Group.1 == 0.5)]
results$median.interactions[results$group=="Mangabey" & results$behav=="Proximity3m"]=xx.mang.prox3m$x[which(xx.mang.prox3m$Group.1 == 0.5)]
results$median.interactions[results$group=="Mangabey" & results$behav=="Aggression.nonphysical"]=xx.mang.agg$x[which(xx.mang.agg$Group.1 == 0.5)]
results$median.interactions[results$group=="Mangabey" & results$behav=="Aggression.total"]=xx.mang.aggtot$x[which(xx.mang.aggtot$Group.1 == 0.5)]
results$median.interactions[results$group=="Mangabey" & results$behav=="Aggression.physical"]=xx.mang.aggp$x[which(xx.mang.aggp$Group.1 == 0.5)]
results$median.interactions[results$group=="Mangabey" & results$behav=="Supplant"]=xx.mang.pant$x[which(xx.mang.pant$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Grooming"]=sd.mang.groom$x[which(sd.mang.groom$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Proximity"]=sd.mang.prox$x[which(sd.mang.prox$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Proximity3m"]=sd.mang.prox3m$x[which(sd.mang.prox3m$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Aggression.nonphysical"]=sd.mang.agg$x[which(sd.mang.agg$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Aggression.total"]=sd.mang.aggtot$x[which(sd.mang.aggtot$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Aggression.physical"]=sd.mang.aggp$x[which(sd.mang.aggp$Group.1 == 0.5)]
results$sd.interactions[results$group=="Mangabey" & results$behav=="Supplant"]=sd.mang.pant$x[which(sd.mang.pant$Group.1 == 0.5)]
results$cor.full[results$group=="Mangabey" & results$behav=="Grooming"]=mean(comp.mang$cor.groom[comp.mang$k==1])
results$cor.full[results$group=="Mangabey" & results$behav=="Proximity"]=mean(comp.mang$cor.prox[comp.mang$k==1])
results$cor.full[results$group=="Mangabey" & results$behav=="Proximity3m"]=mean(comp.mang$cor.prox3m[comp.mang$k==1])
results$cor.full[results$group=="Mangabey" & results$behav=="Aggression.nonphysical"]=mean(comp.mang$cor.agg[comp.mang$k==1])
results$cor.full[results$group=="Mangabey" & results$behav=="Aggression.total"]=mean(comp.mang$cor.aggtot[comp.mang$k==1])
results$cor.full[results$group=="Mangabey" & results$behav=="Aggression.physical"]=mean(comp.mang$cor.aggp[comp.mang$k==1])
results$cor.full[results$group=="Mangabey" & results$behav=="Supplant"]=mean(comp.mang$cor.supplant[comp.mang$k==1])
results$interactions[results$group=="Mangabey" & results$behav=="Grooming"]=sum(mang.data.set$grooming.interactions.sent, na.rm = T)
results$interactions[results$group=="Mangabey" & results$behav=="Proximity"]=sum(mang.data.set$proximity.interactions, na.rm = T)/2
results$interactions[results$group=="Mangabey" & results$behav=="Proximity3m"]=sum(mang.data.set$proximity3m.interactions, na.rm = T)/2
results$interactions[results$group=="Mangabey" & results$behav=="Aggression.nonphysical"]=sum(mang.data.set$aggression.nonphysical.sent, na.rm = T)
results$interactions[results$group=="Mangabey" & results$behav=="Aggression.total"]=sum(mang.data.set$aggression.physical.sent+mang.data.set$aggression.nonphysical.sent, na.rm = T)
results$interactions[results$group=="Mangabey" & results$behav=="Aggression.physical"]=sum(mang.data.set$aggression.physical.sent, na.rm = T)
results$interactions[results$group=="Mangabey" & results$behav=="Supplant"]=sum(mang.data.set$supplant.sent, na.rm = T)

results$median.interactions=round(results$median.interactions,1)
results$sd.interactions=round(results$sd.interactions,1)
results$cor.full=round(results$cor.full,2)
results$median.interactions[results$cor.full<0.5]=NA
results$sd.interactions[results$cor.full<0.5]=NA




bb = droplevels(subset(results,!behav %in% c("Aggression.nonphysical", "Aggression.physical", "Food Share")))
bb$behav[bb$behav=="Supplant"]="Pant Grunt"
tiff("Y:/primint/Alex/Paper 3/Plots/Consistency.tiff",width = 12, height = 6, units = 'in', res = 300)
plot.new()
par(mfrow = c(1, 1), mar = c(4, 4, 4, 4))
plot(
  x = as.numeric(as.factor(bb$behav)),
  y = as.numeric(bb$median.interactions),
  type = "n",
  ylab = "Median Interactions per Dyad",
  xlab = "Interaction Type",
  xaxt = "n",
  main = "Consistency",
  ylim = c(0,20),
  cex.lab = 1.2,
  cex.main = 1.5
)
points(
  as.numeric(as.factor(bb$behav[bb$group == "East"])),
  y = bb$median.interactions[bb$group == "East"],
  pch = 4,
  cex = 2,
  col = "red"
)
points(
  as.numeric(as.factor(bb$behav[bb$group == "South"])),
  y = bb$median.interactions[bb$group == "South"],
  pch = 17,
  cex = 2,
  col = "blue"
)
points(
  as.numeric(as.factor(bb$behav[bb$group == "Mangabey"])),
  y = bb$median.interactions[bb$group == "Mangabey"],
  pch = 10,
  cex = 2,
  col = "green"
)
# abline(h = 0.5, lty = 2)
legend(
  "topleft",
  legend = c("Mangabey", "East", "South"),
  cex = 1.5,
  col = c("green", "red", "blue"),
  pch = c(10, 4, 17)
)
axis(side=1, at=1:5, labels=c("Aggression", "Grooming", "Pant Grunt/Supplant", "Body Contact", "Proximity"), las=1)
dev.off()

