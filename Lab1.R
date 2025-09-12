## Choose variables "population" and "gdp"
pop <- epi.data$population
gdp <- epi.data$gdp

# Variable summaries
summary(pop)
summary(gdp)

# Variable boxplots
boxplot(pop, name = "Population")
boxplot(gdp, name = "GDP")

# Histograms over theoretical probability distributions
hist(pop, prob=TRUE)
lines(density(pop,na.rm=TRUE))

hist(gdp, prob=TRUE)
lines(density(gdp,na.rm=TRUE))

# QQ plots of each variable against normal distribution
plot(ecdf(pop), do.points=FALSE, verticals=TRUE) 
plot(ecdf(gdp), do.points=FALSE, verticals=TRUE) 

# QQ plot of the 2 variables against each other
qqplot(pop, gdp, xlab = "Q-Q plot for Population vs GDP")

## Normality statsitical test for each variable 
shapiro.test(pop)
shapiro.test(gdp)

## Install package for ad test
install.packages("nortest")
library(nortest)

ad.test(pop)
ad.test(gdp)

# Statistical test for variables having identical distributions
ks.test(pop,gdp)

wilcox.test(pop,gdp)

var.test(pop,gdp)
t.test(pop,gdp)
