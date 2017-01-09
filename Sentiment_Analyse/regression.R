data.delta$delta_positiv <- round(data.delta$delta_positiv,digits=4)

# Chi-Quadrat-Test
chisq.test(data.delta$delta_positiv, data.delta$date)

# Korrelation
cor.p     = cor.test(data.delta$delta_positiv, data.delta$year)
cor.s     = cor.test(data.delta$delta_positiv, data.delta$year, method = "spearman")

# Regression
data.delta.mod <- data.delta
data.delta.mod$year <- laply(data.delta.mod$year, function(year){ return (year - 2001)})
lm(data.delta.mod$delta_positiv~data.delta.mod$year)
