rm(list=ls())
require(lmtest)
require(SiZer)

do.single.sim <- function(){
  nn <- 150+100
  slope <- 2.5/30
  ages <- sample(c(rep(0,150),rep(1,100)),nn,replace=T)
  tau <- rgamma(nn,2,3)+1
  tau.dec <- slope*(tau-2+0.7*ages)
  tau.dec[tau.dec<0] <- 0
  cog.dec <- -tau.dec+rnorm(nn,-2/30,2/30)
  mod1 <- lm(cog.dec~poly(tau,3))
  mod2 <- lm(cog.dec~poly(tau,3)+ages)
  lik1 <- logLik(mod1)
  lik2 <- logLik(mod2)
  pp <- 1-pchisq(2*(lik2-lik1),df=1)
}

do.many.sims <- function(num.sims=1000){
  pps <- c()
  for(ii in 1:num.sims){
    pps <- c(pps,do.single.sim())
  }
  sum(pps<0.05)/num.sims
}

do.many.sims()

