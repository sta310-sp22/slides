library(tidyverse)
library(GLMsData)
data(nminer)

# Response - Minerab
# Predictor = Eucs

mu.hat <- nminer$Minerab + 0.1
z <- log(mu.hat) + (nminer$Minerab - mu.hat)/mu.hat
w <- mu.hat

bind_cols(nminer$Minerab, mu.hat, z, w)

x <- tibble(x0 = 1, Eucs = nminer$Eucs)

test <- lm(z ~x$Eucs, weight = w)
eta <- test$fitted # predicted values 


# next round

mu.hat <- exp(test$fitted.values)

z1 <- eta + exp(-eta) * (nminer$Minerab - exp(eta))
w1 <- exp(eta)
test1 <- lm(z1 ~ x$Eucs, weight = w1)

#calculate deviance

dev <- function(x, mu.hat){
  if(x == 0){
    -1 * sqrt(2 * (0 - (x - mu.hat)))
  }else{
    sign(x - mu.hat)*sqrt(2 * (x * log(x/ mu.hat) - (x - mu.hat)))
  }
}

n <- rep(-1, length(mu.hat))
for(i in 1:length(mu.hat)){
  n[i] <- dev(nminer$Minerab[i], mu.hat[i])
}

sum(n^2)

m <- glm(Minerab ~ Eucs, data = nminer, family = poisson,
         control=list(trace=TRUE))



