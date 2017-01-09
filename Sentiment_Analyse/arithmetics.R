quantile(data.delta$delta_neutral, probs=c(0, 0.25, 0.5, 0.75, 1))

boxplot(data.delta$delta_neutral, las=2, ylim=c(0, 0.05))
# Arithmetisches Mittel
mean(data.delta$delta_positiv)
# Median
median(data.delta$delta_positiv)
# Modus
names(sort(-table(data.delta$delta_positiv)))[1]

# mean = median = modus -> Symmetrische Verteilungen
# mean > median > modus -> Linkssteile / Rechtsschiefe Verteilungen
# mean < median < modus -> Rechtssteile / Linksschiefe Verteilungen

# Varianz
var(data.delta$delta_positiv)
# Standart Abweichung
sqrt(var(data.delta$delta_positiv))
