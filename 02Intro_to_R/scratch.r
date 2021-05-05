crab <- read.csv("crabData.csv")

ggplot(as.data.frame(parent_pos), aes(parent_x, parent_y, color=1))+geom_point()

ggplot()+geom_point(as.data.frame(pos), aes(offspr_x, offspr_y))

Horseshoe crab data from [H. Jane Brockmann, Ethology 1996](https://people.clas.ufl.edu/hjb/research/)

library(paramtest)
library(dplyr)
ptm <- proc.time()
d <- 1 #effect size
num_sim <- 5000 #number of t tests to simulate for each N
pow <- 0 #initialize power
N <- 1 #initialize sample size
while(pow<0.8){
  N <- N+1 #increment sample size
  sig <- logical(num_sim) #initialize
  for(i in 1:num_sim){
    sim <- data.frame(group = c(rep("a",N), rep("b",N)), #simulate data
                      value = c(rnorm(N,0,1), rnorm(N,d,1)))
    ttest <- t.test(value~group, data=sim) #test for difference
    p <- ttest$p.value #extract p value
    sig[i] <- (p < 0.05) #record significance
  }
  pow <- mean(sig) #compute power
}
cat("sample size =",N)
proc.time() - ptm

N <- 14
b0 <- 0
b1 <- 0.15
b2 <- -0.15
x1 <- rnorm(N,0,1)
x2 <- rnorm(N,0,1)
y1 <- rnorm(N, b0+b1*x1, sqrt(1-b1^2))
summary(lm(y1~x1))
y <- rnorm(N, b0+b1*x1+b2*x2, sqrt(1-b1^2-b2^2))
summary(lm(y~x1+x2))

lm2<-lm(y~x1+x2)
summary(lm2)$fstatistic[1]

var(y1)
var(resid(lm(y1~x1)))

pwr.f2.test(u=1, f2=(0.15^2)/(1-0.15^2), power=0.8)
pwr.f2.test(u=2, f2=(0.5)/(1-0.5), power=0.8)

lm_test <- function(simNum, N, b1, b0=0, xm=0, xsd=1) {
  x <- rnorm(N, xm, xsd)
  y <- rnorm(N, b0 + b1*x, sqrt(1 - b1^2))  # var. approx. 1 after accounting
  # for explained variance by x
  model <- lm(y ~ x)
  
  # pull output from model
  est <- coef(summary(model))['x', 'Estimate']
  se <- coef(summary(model))['x', 'Std. Error']
  p <- coef(summary(model))['x', 'Pr(>|t|)']
  f <- summary(model)$fstatistic[1]
  r2 <- summary(model)$r.squared
  f2 <- r2/(1-r2)
  
  return(c(xm=mean(x), xsd=sd(x), ym=mean(y), ysd=sd(y), 
           est=est, se=se, p=p, 
           f=f, r2=r2, f2=f2,
           sig=(p < .05)))
}

# we vary N at 200 and 300; we are also setting coefficient of x predicting
# y to be approx. .15 across all simulations
power_lm <- grid_search(lm_test, params=list(N=14), n.iter=5000, output='data.frame', b1=.7)
results(power_lm) %>%
  group_by(N.test) %>%
  summarise(power=mean(sig))
summary(results(power_lm)$r2)
hist(results(power_lm)$r2)

summary(results(power_lm)$f.value)
summary(results(power_lm)$f2)
unique(results(power_lm)$f.value/12/results(power_lm)$f2)

hist(results(power_lm)$f2)

binom_glm_test <- function(simNum, N, b1, b0=1, xm=0, xsd=1) {
  x <- rnorm(N, xm, xsd)
  y <- rbinom(N, size = 1, prob = plogis(b0 + b1*x))
  model <- glm(y ~ x, family = binomial)
  
  # pull output from model
  est <- coef(summary(model))['x', 'Estimate']
  se <- coef(summary(model))['x', 'Std. Error']
  p <- coef(summary(model))['x', 'Pr(>|z|)']
  
  return(c(xm=mean(x), xsd=sd(x), 
           est=est, se=se, p=p, 
           sig=(p < .05)))
}



power_binom_glm <- grid_search(binom_glm_test, params=list(N=c(40,50,100)), n.iter=500, output='data.frame', b1=1)
results(power_binom_glm) %>%
  group_by(N.test) %>%
  summarise(power=mean(sig))

binom_glm_test <- function(simNum, N=30, b1=1, b0=1, xm=0, xsd=1) {
  x <- rnorm(N, xm, xsd)
  y <- rbinom(N, size=1, plogis(b0 + b1*x))
  model <- glm(y ~ x, family = binomial)
  
  # pull output from model
  est <- coef(summary(model))['x', 'Estimate']
  se <- coef(summary(model))['x', 'Std. Error']
  p <- coef(summary(model))['x', 'Pr(>|z|)']
  
  return(c(xm=mean(x), xsd=sd(x),
           est=est, se=se, p=p,
           sig=(p < .05)))
}

question_5 <- grid_search(binom_glm_test,
                          params = list(N=c(30,60,90)),
                          n.iter=500, output = 'data.frame')
results(question_5) %>%
  group_by(N.test) %>%
  summarise(power=mean(sig))
