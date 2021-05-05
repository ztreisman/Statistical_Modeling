##
# Intro Bayesian notes
#

# Stuff from Bolker's Morelia workshop
## modeling packages
mod_pkgs <- c("bbmle", "blme", "brms", "gamm4", "glmmLasso", "glmmML",
              "glmmTMB", "lme4", "MCMCglmm", "robustlmm", "rstanarm", "spaMM")
## miscellaneous/data manipulation
data_pkgs <- c("benchmark", "brglm", "devtools", "emdbook", "MEMSS",
               "plyr", "reshape2", "SASmixed", "tidyverse")
## model processing/diagnostics/reporting
diag_pkgs <- c("afex", "agridat", "AICcmodavg", "aods3", "arm",
               "broom", "cAIC4", "car", "coda", "DHARMa", "effects",
               "emmeans", "HLMdiag", "Hmisc", "lmerTest", "multcomp",
               "MuMIn", "pbkrtest", "RLRsim", "rockchalk", "sjPlot",
               "sjstats", "stargazer", "texreg")
## graphics
graph_pkgs <- c("cowplot", "directlabels",
                "dotwhisker", "GGally", "ggalt", "ggplot2",
                "ggpubr", "ggstance", "gridExtra", "plotMCMC",
                "plotrix", "viridis")

all_pkgs <- c(mod_pkgs,data_pkgs,diag_pkgs,graph_pkgs)
avail_pkgs <- rownames(available.packages())
already_installed <- rownames(installed.packages())
to_install <- setdiff(all_pkgs,already_installed)
if (length(to_install)>0) {
  install.packages(to_install,dependencies=TRUE)
}
devtools::install_github("bbolker/broom.mixed")
devtools::install_github("mjskay/tidybayes")
## get INLA (optional!)
source("http://www.math.ntnu.no/inla/givemeINLA.R")

#Stuff for the notes

library(brms)
library(emdbook)
head(MyxoTiter_sum)
library(ggplot2)
ggplot(MyxoTiter_sum, aes(day, titer, color=factor(grade)))+
  geom_point()+geom_smooth()
unique(MyxoTiter_sum$grade)
ggplot(MyxoTiter_sum, aes(day, titer, color=factor(grade)))+
  geom_point()+scale_x_log10()+scale_y_log10()
myx_glm <- glm(titer~poly(day,2)*grade, 
               family = Gamma(link = "identity"), data=MyxoTiter_sum)
summary(myx_glm)

fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease + (1 + age|patient),
             data = kidney, family = lognormal(),
            prior = c(set_prior("normal(0,5)", class = "b"),
            set_prior("cauchy(0,2)", class = "sd"),
           set_prior("lkj(2)", class = "cor")), warmup = 1000,
          iter = 2000, chains = 4, control = list(adapt_delta = 0.95))

pairs(fit1)
summary(fit1, waic = TRUE)

x <- subset(ReedfrogPred,pred=="pred" & density==10 & size=="small")
fit_tad_small <- brm(surv~1, family=binomial(), data=x)
summary(fit_tad_small)


fit_tad0 <- brm(surv | trials(density) ~ 1, family=binomial(), data=ReedfrogPred)
summary(fit_tad0)
summary(fit_tad0)$fixed
plogis(summary(fit_tad0)$fixed[1])
sum(ReedfrogPred$surv)/sum(ReedfrogPred$density)
plot(fit_tad0)

qlogis(0.75)
fit_tad0_prior <- brm(surv | trials(density) ~ 1, 
                      family=binomial(), data=ReedfrogPred, 
                      prior = set_prior("normal(1.098612,0.1)", class = "Intercept"))
plot(fit_tad0_prior)

plogis(log(3))
qlogis(0.75)
log(3)

fit_tad <- brm(surv | trials(density) ~ pred + size, family=binomial(), data=ReedfrogPred)
summary(fit_tad)
summary(fit_tad)$fixed
plogis(summary(fit_tad0)$fixed["Intercept", "Estimate"])
exp(summary(fit_tad)$fixed[,"Estimate"])
sum(ReedfrogPred$surv)/sum(ReedfrogPred$density)
waic(fit_tad)
loo(fit_tad)

glm_fit_tad <- glm(cbind(surv, density-surv) ~ pred + size, family = binomial(), data = ReedfrogPred)
summary(glm_fit_tad)
MuMIn::loo(glm_fit_tad)

fit_tad_prior <- brm(surv | trials(density) ~ pred + size, 
                     family=binomial(), data=ReedfrogPred, 
                     prior = set_prior("normal(-1,0.1)", class = "b", coef = "sizebig") )
summary(fit_tad_prior)
plot(fit_tad_prior)

MyxoTiter_sum$fgrade <- factor(MyxoTiter_sum$grade)
MyxoTiter_sum$grade5 <- MyxoTiter_sum$grade==5

prior1 <- prior(normal(0.5, 1), nlpar = "a") +
  prior(normal(0.2, 0.4), nlpar = "b") + 
  prior(gamma(50, 0.1), class="shape")
fit_myx <- brm(bf(titer ~ a*day*exp(-b*day), a~fgrade, b~fgrade, nl=TRUE), 
               data = MyxoTiter_sum, 
               prior = prior1, 
               family=Gamma(link = "identity"))
              
plot(fit_myx)

summary(fit_myx)

WAIC(fit_myx)
brms::loo(fit_myx)

myx_det <- function(day, grade){
  a <- 3.06 - 0.56*(grade==3) - 1.06*(grade==4) - 0.65*(grade==5)
  b <- 0.14 - 0.05*(grade==3) - 0.06*(grade==4)
  a*day*exp(-b*day)
}

# This doesn't work. 
geom_myxfit <- function(day, grade){
  list(geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==grade,], fun=myx_det, args = list(grade=grade)),
    geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==grade,], fun=function(day){qgamma(0.025, shape=32.34, scale=myx_det(day,grade)/32.34)}, lty=2),
    geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==grade,], fun=function(day){qgamma(0.975, shape=32.34, scale=myx_det(day,grade)/32.34)}, lty=2))
  }


#There needs to be a geom that does this. ggproto?
ggplot(MyxoTiter_sum, aes(day, titer))+
  geom_point()+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==1,], 
                fun=myx_det, args = list(grade=1))+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==1,], 
                fun=function(day){
                  qgamma(0.025, shape=32.34, scale=myx_det(day,1)/32.34)}, 
                lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==1,], 
                fun=function(day){
                  qgamma(0.975, shape=32.34, scale=myx_det(day,1)/32.34)}, 
                lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==3,], fun=myx_det, args = list(grade=3))+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==3,], fun=function(day){qgamma(0.025, shape=32.34, scale=myx_det(day,3)/32.34)}, lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==3,], fun=function(day){qgamma(0.975, shape=32.34, scale=myx_det(day,3)/32.34)}, lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==4,], fun=myx_det, args = list(grade=4))+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==4,], fun=function(day){qgamma(0.025, shape=32.34, scale=myx_det(day,3)/32.34)}, lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==4,], fun=function(day){qgamma(0.975, shape=32.34, scale=myx_det(day,3)/32.34)}, lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==5,], fun=myx_det, args = list(grade=5))+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==5,], fun=function(day){qgamma(0.025, shape=32.34, scale=myx_det(day,5)/32.34)}, lty=2)+
  geom_function(data=MyxoTiter_sum[MyxoTiter_sum$grade==5,], fun=function(day){qgamma(0.975, shape=32.34, scale=myx_det(day,5)/32.34)}, lty=2)+
  facet_wrap(~fgrade)


AIC(myx_glm)
llMyx <- log_lik(fit_myx)
?brms::logLik.brmsfit
colSums(llMyx)
rowSums(llMyx)

nllMyx <- -sum(colMeans(llMyx))

(AIC_maybe <- 2*nllMyx-2*dim(fit_myx$data)[2])

BIC(fit_myx)

MuMIn::loo(myx_glm, type="loglik")
brms::loo(fit_myx)
brms::loo(fit_myx, moment_match=T)

MyxoTiter_sum$glm_preds <- predict(myx_glm, se.fit=T)$fit
MyxoTiter_sum$glm_se <- predict(myx_glm, se.fit=T)$se.fit
MyxoTiter_sum$glm_lower <- MyxoTiter_sum$glm_preds-2*MyxoTiter_sum$glm_se
MyxoTiter_sum$glm_upper <- MyxoTiter_sum$glm_preds+2*MyxoTiter_sum$glm_se


ggplot(MyxoTiter_sum, aes(day, titer))+
  geom_point()+
  facet_wrap(MyxoTiter_sum$fgrade)+
  geom_line(aes(y=glm_preds))+
  geom_line(aes(y=glm_lower), lty=2)+
  geom_line(aes(y=glm_upper), lty=2)
  
## lab code

moose <- data.frame(dev.intensity = c(2.9,8.5,7.0,1.3,9.7,7.5,0.4,6.2,0.9,3.8),
dist.moved = c(456,141,47,1362,128,21,2123,189,899,38))

N <- 20

lik_fun <- function(a,b) exp(sum(log(dexp(moose$dist.moved,rate = 1/(a*moose$dev.intensity^b) ))))
nll1 <- function(a=500,b) -sum(log(dexp(moose$dist.moved,rate = 1/(a*moose$dev.intensity^b) )))
exp(-nll1(b=-0.8))
lik_fun(a=500, b=-0.8)

params <- expand.grid(a = 250+125*(0:N), b = -(0.7+0.05*(0:N)))
as<-params$a
bs<-params$b
params$lik <- mapply(lik_fun,a = as, b = bs )
ggplot(params, aes(a,b,z=lik))+geom_contour_filled()+theme_bw()


strong_prior <- function(a,b){dnorm(a,511, 125)*dnorm(b,-0.9,0.05)} 
strong_prior(511,-0.9)
strong_prior(511,-0.8)
params$strong <- mapply(strong_prior,a = as, b = bs )
ggplot(params, aes(a,b,z=strong))+geom_contour_filled()+theme_bw()


weak_prior <- function(a,b){dnorm(a,511, 250)*dnorm(b,-0.9,0.1)} 
weak_prior(511, -0.9)
weak_prior(511, -0.8)
params$weak <- mapply(weak_prior,a = as, b = bs )
ggplot(params, aes(a,b,z=weak))+geom_contour_filled()+theme_bw()

flat_prior  <- function(a,b){dnorm(a,511, 10000)*dnorm(b,-0.9,20)} 
flat_prior(511, -0.9)
flat_prior(511, -0.8)
params$flat <- mapply(flat_prior,a = as, b = bs )
ggplot(params, aes(a,b,z=flat))+geom_contour_filled()+theme_bw()

params$strong_posterior <- params$lik*params$strong
ggplot(params, aes(a,b,z=strong_posterior))+geom_contour_filled()+theme_bw()

params$weak_posterior <- params$lik*params$weak
ggplot(params, aes(a,b,z=weak_posterior))+geom_contour_filled()+theme_bw()

params$flat_posterior <- params$lik*params$flat
ggplot(params, aes(a,b,z=flat_posterior))+geom_contour_filled()+theme_bw()


strong_model <- brm(bf(dist.moved~a*dev.intensity^b, a~1,b~1, nl=T),
    data = moose,
    prior=set_prior("normal(511,125)", nlpar ="a")+
      set_prior("normal(-0.9,0.05)", nlpar="b"),
    family = exponential(link="identity"))
    
weak_model <- brm(bf(dist.moved~a*dev.intensity^b, a~1,b~1, nl=T),
                    data = moose,
                    prior=set_prior("normal(511,250)", nlpar ="a")+
                      set_prior("normal(-0.9,0.1)", nlpar="b"),
                    family = exponential(link="identity"))


flat_model <- brm(bf(dist.moved~a*dev.intensity^b, a~1,b~1, nl=T),
                  data = moose,
                  prior=set_prior("normal(511,10000)", nlpar ="a")+
                    set_prior("normal(-0.9,20)", nlpar="b"),
                  family = exponential(link="identity"))

plot(strong_model)
plot(weak_model)
plot(flat_model)
pairs(flat_model)
