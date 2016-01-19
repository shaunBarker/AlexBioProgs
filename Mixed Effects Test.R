# From Zuur 2009
setwd("/home/alex/Desktop/Data/ZuurDataMixedModelling/")
RIKZ = read.table("RIKZ.txt",header=TRUE)
library(nlme)


################ 2-step analysis (not so good)

Beta <- vector(length = 9) # there are 9 beaches. records estimate of intercept and effect of NAP
for (i in 1:9) {
  Mi <- summary(lm(Richness ~ NAP,
                   subset = (Beach==i), data=RIKZ))
  Beta[i] <- Mi$coefficients[2, 1] }

fExposure9 <- factor(c(0, 0, 1, 1, 0, 1, 1, 0, 0))
tmp2 <- lm(Beta ~ fExposure9) # this is a one-way ANOVA apparently

# lmList version
lmList(Richness ~ NAP | Beach, data=RIKZ)

################## Mixed effects modelling

### Mixed effects model with random intercept

RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random = ~1| fBeach, # the random = ~1| fBeach signifies a random intercept model
               data = RIKZ)
summary(Mlme1)
#  AIC & BIC help with model selection

# generate plot of fitted values
F0 <- fitted(Mlme1, level = 0)
F1 <- fitted(Mlme1, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l", 
     ylim = c(0, 22), ylab = "Richness", xlab = "NAP") # this one is the "population model" fit
for (i in 1:9) {
  x1 <- RIKZ$NAP[RIKZ$Beach == i]
  y1 <- F1[RIKZ$Beach == i]
  K <- order(x1)
  lines(sort(x1), y1[K]) # fit lines for each beach
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9) 

# empty plot with just numbers and main pop line
#plot(NAPs, F0[I], lwd = 4, type = "l", 
#     ylim = c(0, 22), ylab = "Richness", xlab = "NAP")
#text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9) 

# as seen by the plot, the random intercept model allows one line (the thick "population" one) to be shifted up or down for each beach, following a normal distribution with variance d^2 (estimate of this is in the "Random effects" part of the lme command)

# Mixed effects model with random intercept and slope
Mlme2 <- lme(Richness ~ NAP,
             random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)

F0 <- fitted(Mlme2, level = 0)
F1 <- fitted(Mlme2, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l", 
     ylim = c(0, 22), ylab = "Richness", xlab = "NAP") # this one is the "population model" fit. from the "fixed" component only
for (i in 1:9) {
  x1 <- RIKZ$NAP[RIKZ$Beach == i]
  y1 <- F1[RIKZ$Beach == i]
  K <- order(x1)
  lines(sort(x1), y1[K]) # fit lines for each beach
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9) 

# Random effects model

Mlme3 <- lme(Richness ~1, random = ~1 | fBeach,
             data = RIKZ)
summary(Mlme3)

F0 <- fitted(Mlme3, level = 0)
F1 <- fitted(Mlme3, level = 1)
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l", 
     ylim = c(0, 22), ylab = "Richness", xlab = "NAP") # this one is the "population model" fit. from the "fixed" component only
for (i in 1:9) {
  x1 <- RIKZ$NAP[RIKZ$Beach == i]
  y1 <- F1[RIKZ$Beach == i]
  K <- order(x1)
  lines(sort(x1), y1[K]) # fit lines for each beach
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9) 
