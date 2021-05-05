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

pigweed_seeds <- function(L=30, 
                          nparents=50, offspr_per_parent = 10, dispdist=2,
                          M=2.3, alpha=0.49,
                          b = 271.6, k = 0.569){
  
  parent_x = runif(nparents,min=0,max=L) 
  parent_y = runif(nparents,min=0,max=L)
  noffspr = nparents*offspr_per_parent
  angle = runif(noffspr,min=0,max=2*pi) 
  dist = rexp(noffspr,1/dispdist)
  
  offspr_x = rep(parent_x,each=offspr_per_parent)+cos(angle)*dist
  offspr_y = rep(parent_y,each=offspr_per_parent)+sin(angle)*dist
  pos <- cbind(offspr_x,offspr_y); ndist <- as.matrix(dist(pos))
  
  nbrcrowd = rowSums(ndist<2)-1 
  ci = nbrcrowd*3 
  
  mass_det=M/(1+ci)
  mass = rgamma(length(mass_det),scale=mass_det,shape=alpha)
  
  seed_det <- b*mass
  seed <- rnbinom(length(seed_det),mu=seed_det,size=k)
  
  total_seeds <- sum(seed)
  return(total_seeds)
}

num_sim <- 10000
expected_seeds <- numeric(num_sim)
for(i in 1:num_sim){
  expected_seeds[i] <- pigweed_seeds()
}

pigweed_test <- function(simNum, M=2.3, b=271.6){
  test_seeds <- pigweed_seeds(M=M, b=b)
  p_greater <- mean(test_seeds < expected_seeds)
  p_less <- mean(test_seeds > expected_seeds)
  sig <- (2*p_greater<0.05)|(2*p_less<0.05)
  
  return(c(sig=sig, p_greater=p_greater, p_less=p_less))
}

power_pigweed_test <- grid_search(pigweed_test, 
                                  params = list(M=c(2,3,4,5), 
                                                b=c(100,200,300,400)), 
                                  n.iter=500, output = 'data.frame')
results(power_pigweed_test) %>%
  group_by(M.test, b.test) %>%
  summarise(power=mean(sig))

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
