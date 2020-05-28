results = results[results$behav!='Aggression.total',]
results$behav[results$behav%in%c('Supplant', 'Pant Grunt')] = 'Pant Grunt/Supplant'
results$behav[results$behav%in%c('Aggression.nonphysical')] = 'Aggression nonphysical'
results.short = results[complete.cases(results), ]

results.gg = ggplot(results.short, aes(x = behav, y = median.interactions, color = group, shape = group)) +
  geom_point(size = 3) + ylim(0,20) + theme_minimal()  + labs(y = "Mean interactions per dyad to reach 0.5", x = 'Interaction Type')


results.groom = ggplot(comp.east, aes(x = groom.per.dyad, y = cor.groom)) +
  geom_point(color = 'yellow', alpha = 0.5, size = 2) +
  geom_point(comp.south, mapping = aes(x = groom.per.dyad, y = cor.groom), size = 2, color = 'blue', alpha = 0.5) +
  geom_point(comp.mang, mapping = aes(x = groom.per.dyad, y = cor.groom), size = 2, color = 'green', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,30) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Grooming')

results.prox = ggplot(comp.east, aes(x = prox.per.dyad, y = cor.prox)) +
  geom_point(color = 'yellow', alpha = 0.5, size = 2) +
  geom_point(comp.south, mapping = aes(x = prox.per.dyad, y = cor.prox), size = 2, color = 'blue', alpha = 0.5) +
  geom_point(comp.mang, mapping = aes(x = prox.per.dyad, y = cor.prox), size = 2, color = 'green', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,30) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Body Contact')

results.prox3m = ggplot(comp.east, aes(x = prox3m.per.dyad, y = cor.prox3m)) +
  geom_point(color = 'yellow', size = 2, alpha = 0.5) +
  geom_point(comp.south, mapping = aes(x = prox3m.per.dyad, y = cor.prox3m), size = 2, color = 'blue', alpha = 0.5) +
  geom_point(comp.mang, mapping = aes(x = prox3m.per.dyad, y = cor.prox3m), size = 2, color = 'green', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,30) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Proximity')

results.pant = ggplot(comp.east, aes(x = pant.per.dyad, y = cor.pant)) +
  geom_point(color = 'yellow', alpha = 0.5, size = 2) +
  geom_point(comp.south, mapping = aes(x = pant.per.dyad, y = cor.pant), size = 2, color = 'blue', alpha = 0.5) +
  geom_point(comp.mang, mapping = aes(x = supplant.per.dyad, y = cor.supplant), size = 2, color = 'green', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,30) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Pant Grunt/Supplant')

results.agg = ggplot(comp.east, aes(x = agg.per.dyad, y = cor.agg)) +
  geom_point(color = 'yellow', alpha = 0.5, size = 2) +
  geom_point(comp.south, mapping = aes(x = agg.per.dyad, y = cor.agg), size = 2, color = 'blue', alpha = 0.5) +
  geom_point(comp.mang, mapping = aes(x = agg.per.dyad, y = cor.agg), size = 2, color = 'green', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,5) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Aggression Non-physical')

results.aggp = ggplot(comp.east, aes(x = aggp.per.dyad, y = cor.aggp)) +
  geom_point(color = 'yellow', alpha = 0.5, size = 2) +
  geom_point(comp.south, mapping = aes(x = aggp.per.dyad, y = cor.aggp), size = 2, color = 'blue', alpha = 0.5) +
  geom_point(comp.mang, mapping = aes(x = aggp.per.dyad, y = cor.aggp), size = 2, color = 'green', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,5) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Aggression Physical')

results.food = ggplot(comp.east, aes(x = food.per.dyad, y = cor.food)) +
  geom_point(color = 'yellow', alpha = 0.5, size = 2) +
  geom_point(comp.south, mapping = aes(x = food.per.dyad, y = cor.food), size = 2, color = 'blue', alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5)) +
  xlim(0,5) +
  theme_classic()  + labs(y = "Correlation Halves", x = 'Interactions per Dyad', title = 'Food Share')

white = ggplot() + theme(panel.border = element_rect(colour = "white", fill='white', size=5))

common = grid.arrange(results.groom, results.prox, results.prox3m, results.pant, nrow = 2)
rare = arrangeGrob(results.agg, results.aggp, results.food, white, nrow = 2)

tiff("F:/Workspace Leipzig/Consistency Paper/Plot Common.tiff", width = 6, height = 4, units = 'in', res = 300)
plot(common)
dev.off()
tiff("F:/Workspace Leipzig/Consistency Paper/Plot Rare.tiff", width = 6, height = 4, units = 'in', res = 300)
plot(rare)
dev.off()
tiff("F:/Workspace Leipzig/Consistency Paper/Plot Results.tiff", width = 8, height = 5, units = 'in', res = 300)
plot(results.gg)
dev.off()