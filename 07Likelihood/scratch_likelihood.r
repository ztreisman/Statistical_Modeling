library(bbmle)
library(ggplot2)

# Slides code

x<-rnorm(50,5,1)
y<-rnorm(50, 3+2*x,1.5)
lm(y~x)
nll_fn <- function(a,b,s) -sum(dnorm(y, a+b*x, s, log = TRUE))
mle2(nll_fn, start = list(a=2,b=3,s=1))
plot(y~x)
abline(3,2)
abline(lm(y~x), col="blue")

y2<-rbinom(50, size = 1, plogis(10-2*x))
y2
coef(glm(y2~x, family = binomial))
nll_fn <- function(a,b) -sum(dbinom(y2, size=1, plogis(a+b*x), log = TRUE))
mle2(nll_fn, start = list(a=10,b=-2))
plot(y2~x)

x <- runif(50)
y <- rpois(50, 2+x)

#https://stackoverflow.com/questions/45300955/using-bbmlemle2-with-vector-parameters-already-works-using-optim
mle2(y~dpois(exp(loglambda)),     ## use log link/exp inverse-link
     data=data.frame(y,x),      ## need to specify as data frame
     parameters=list(loglambda~x),  ## linear model for loglambda
     start=list(loglambda=0))         ## start values for *intercept*

glm(y~x, family = poisson)

n <- 50
x1 <- runif(n, min=0, max=5); x2 <- runif(n, min=0, max=10)
y <- rpois(n, 2+2*x1+x2)
glm2interact <- glm(y~x1*x2, family = poisson)
glm2both <- glm(y~x1+x2, family = poisson)
glm2x1 <- glm(y~x1, family = poisson)
glm2x2 <- glm(y~x2, family=poisson)
glm2null <- glm(y~1, family = poisson)
anova(glm2null,glm2x1,glm2both, glm2interact, test="LRT")
anova(glm2interact, test="LRT")

# Lab code

moose <- data.frame(dev.intensity = c(2.9,8.5,7.0,1.3,9.7,7.5,0.4,6.2,0.9,3.8),
                    dist.moved = c(456,141,47,1362,128,21,2123,189,899,38))

# 1-3
nll0 <- function(a) -sum(log(dexp(moose$dist.moved,rate = (1/a))))
nll0(1000)
nll0(500)
nll0(250)
nll0(750)
as <- 100*(10^0.1)^(0:10)
as
nll0s <- sapply(as, nll0)
plot(nll0s~as, type="o")
mod0 <- mle2(nll0, start = list(a=1000), data=moose)
mod0

#4-6
nll1 <- function(a=1000,b) -sum(log(dexp(moose$dist.moved,rate = 1/(a*moose$dev.intensity^b) )))
nll1(b=-0.8)
exp(-nll1(b=-0.8))
bs <- -0.1*(5:15)
# bs <- sort(runif(10, 5, 15)) # why doesn't this work?
bs
nll1s <- mapply(nll1, a=500, b=bs) 
nll1s
plot(nll1s~bs, type="o")
mod1 <- mle2(nll1, start = list(a=1000,b=-1), data=moose)
mod1
summary(mod1)

nll1 <- function(a=500,b) -sum(log(dexp(moose$dist.moved,rate = 1/(a*moose$dev.intensity^b) )))
exp(-nll1(b=-0.8))
nll1(a=500, b=-0.8)

plotnll <- function(nllfunc=nll1, amin=725, bmin= -1.7, astep=125, bstep=0.1, gridsize=20 ){
N <- gridsize
params <- expand.grid(a = amin+astep*(0:N), b = bmin+bstep*(0:N))
as<-params$a
bs<-params$b
params$lik <- mapply(nllfunc,a = as, b = bs )
ggplot(params, aes(a,b,z=lik))+geom_contour_filled()+theme_bw()
}
plotnll()
plotnll(bmin=-2, amin=500)
plotnll(bmin=-2, amin=750, astep = 250)
plotnll(nllfunc = function(a,b){exp(-nll1(a,b))}, bmin=-1.5, bstep=0.05, amin=500, astep = 50)


#7-8
anova(mod0, mod1)
AICtab(mod0, mod1)

#plot the model

mod1func <- function(x) coef(mod1)["a"]*x^coef(mod1)["b"]

ggplot(moose, aes(dev.intensity, dist.moved))+
  geom_point()+ 
  geom_rug()+
  geom_function(fun=mod1func)+
  geom_function(fun=function(x) qexp(0.025, rate = 1/mod1_func(x)), linetype=2)+
  geom_function(fun=function(x) qexp(0.975, rate = 1/mod1_func(x)), linetype=2)+
  lims(y=c(0,2500))


ggplot(moose, aes(dev.intensity,dist.moved))+
  geom_point(color="dodgerblue")+
  geom_line(aes(y=exp(predict(glmRidersNB))))+
  geom_line(aes(y=qnbinom(0.025,
                          mu=exp(predict(glmRidersNB)),
                          size=glmRidersNB$theta)),
            linetype=2)+
  geom_line(aes(y=qnbinom(0.975,
                          mu=exp(predict(glmRidersNB)),
                          size=glmRidersNB$theta)),
            linetype=2)+
  labs(title="Negative Binomial")