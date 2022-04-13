bootstrapAnova <- function(mA, m0, B=1000){
  oneBootstrap <- function(m0, mA){
    d <- drop(simulate(m0))
    m2 <-refit(mA, newresp=d)
    m1 <-refit(m0, newresp=d)
    return(anova(m2,m1)$Chisq[2])
  }  
  nulldist <- replicate(B, oneBootstrap(m0, mA))
  ret <- anova(mA, m0)
  ret$"Pr(>Chisq)"[2] <- mean(ret$Chisq[2] < nulldist)
  names(ret)[8] <- "Pr_boot(>Chisq)"
  attr(ret, "heading") <- c(attr(ret, "heading")[1], 
                            paste("Parametric bootstrap with", B,"samples."),
                            attr(ret, "heading")[-1])
  attr(ret, "nulldist") <- nulldist
  return(ret)
}