library(lme4)
library(lubridate)
library(ggplot2)
library(emdbook)

seedlings<-read.csv("./data/Siberia_seedlings_data.csv")
ADRseedlings<-read.csv("./data/Larch_seedlings.csv")

str(seedlings)

summary(seedlings)
levels(seedlings$site)
levels(seedlings$site) <- c("Alnus", "ANS", "BP", "CN", "FOC", "FU", "Gonzo", "HR", "Shark")
table(seedlings$treatment)
table(seedlings$site,seedlings$treatment)
table(seedlings$age, seedlings$burn_year)

seedlings <- droplevels(seedlings[-which(seedlings$treatment=="unburned"),])

write.csv(seedlings, file = "data/seedlings.csv")

seedlings <- seedlings[-which(seedlings$ht_cm>100),]
seedlings <- seedlings[-which(seedlings$bd_cm>1),]

seedlings <- na.omit(seedlings)

ggplot(seedlings, aes(age, ht_cm, size=bd_cm, color=treatment))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(seedlings, aes(age, lfg_cm, size=bd_cm, color=treatment))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(seedlings, aes(bd_cm, ht_cm, size=lfg_cm, color=treatment))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(seedlings, aes(log(lfg_cm), log(ht_cm*bd_cm), size=age, color=treatment))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(seedlings, aes(lfg_cm, ht_cm*bd_cm, size=age, color=treatment))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()

ADRseedlings$treatment <- as.factor(ifelse(ADRseedlings$radius_m <= 1, "high", "moderate"))

ggplot(ADRseedlings, aes(basal_diameter_cm, height_cm, size=age, color=treatment))+
  geom_point(alpha=0.5)+
  geom_point(data=seedlings, aes(bd_cm, ht_cm, size=age-10))+
  scale_x_log10()+
  scale_y_log10()

longdat<-rbind(data.frame(age=seedlings$age, ht=seedlings$ht_cm, bd=seedlings$bd_cm), 
               data.frame(age=ADRseedlings$age, ht=ADRseedlings$height_cm, bd=ADRseedlings$basal_diameter_cm))

dim(unique(longdat))
dim(unique(data.frame(age=seedlings$age, ht=seedlings$ht_cm, bd=seedlings$bd_cm)))
dim(unique(data.frame(age=ADRseedlings$age, ht=ADRseedlings$height_cm, bd=ADRseedlings$basal_diameter_cm)))

plants <- within(plants, date <- paste(c(month, day, year), sep=""))
plants$date <- NULL

# Fit randomized block design. 
# The syntax (1|site) means allow the levels of site to vary the intercept.
mod.lmer <- lmer(ht_cm~age*treatment+(1|site), data=seedlings)

# lmer does not return p-values
anova(mod.lmer)
summary(mod.lmer)

# The F-statistic is in the ANOVA table
anova(mod.lmer)[3,4]

# use a parametric bootstrap to obtain a p-value
# fit a model to the data without treatment as a predictor
mod1.lmer <- lmer(ht_cm~age+(1|site), data=seedlings)

# start Monte Carlo simulation--1000 simulations
nrep <- 1000
#initialize storage vectors
Fstat1 <- numeric(nrep)
FstatS <- numeric(nrep)

# loop to carry out simulations
for(i in 1:nrep) {
  # simulate data from model in which type has no effect
  rmath <- unlist(simulate(mod1.lmer))
  # estimate type model to these data
  rmod <- lmer(rmath~age*treatment+(1|site), data=seedlings)
  # extract statistic
  Fstat1[i] <- anova(rmod)[2,4]
  FstatS[i] <- anova(rmod)[3,4]
}

max(Fstat1)
max(FstatS)

# null distribution of F-statistic
hist(Fstat1)
hist(FstatS)

# P values
mean(anova(mod.lmer)[2,4]<Fstat1)
mean(anova(mod.lmer)[3,4]<FstatS)



# treat blocks as random: using nlme package
library(nlme)
mod2.lme <- lme(lw.rat~type, random=~1|pot, data=plants)

# compare random blocks with fixed blocks model
anova(mod1)
anova(mod2.lme)
summary(mod2.lme)
coef(mod1)

# The regression coefficient estimates are β0 = 1.5192 and β1 = 0.55229. The estimate of β1 is 
# identical to what we obtained using lm. The estimate of β0 is different from the fixed effects
# model but that's because β0 now represents something quite different. In the mixed effects model
# β0 is the average length-width ratio for all globe plants (not just the average length-width 
# ratio for globe plants in pot #16533).

# The section labeled "Random effects" displays estimates of the parameters of the random effects 
# distribution and the error distribution. The entry labeled "(Intercept)" is τ, the standard deviation
# of the random effects distribution, and the entry labeled "Residual" is σ, the standard deviation 
# of the error distribution.

# extract fixed effects estimates with fixef
fixef(mod2.lme)
# extract random effects predictions with ranef
ranef(mod2.lme)
# coef combines random effects and fixed effects
coef(mod2.lme)

# the predict function uses both fixed effects and random effects by default
predict(mod2.lme)[1:10]

# specifying level=0 gets predictions using just the fixed effects
predict(mod2.lme, level=0)[1:10]

# add points for each pot based on the predictions of the least squares and mixed effects models

ggplot()+geom_point(data = plants, aes(x=lw.rat,y=pot, color=type))+
  geom_point(aes(x=predict(mod2.lme), y=plants$pot), shape=3)+
  geom_point(aes(x=predict(mod1), y=plants$pot), shape=5)

# If we want a legend

me.predictions <- unique(data.frame("mean"=predict(mod2.lme),
                                    "model"="mixed effects", 
                                    "pot"=plants$pot, 
                                    "type"=plants$type))
lm.predictions <- unique(data.frame("mean"=predict(mod1),
                                    "model"="least squares", 
                                    "pot"=plants$pot, 
                                    "type"=plants$type))
predictions <- merge(lm.predictions, me.predictions, all = T)

ggplot()+geom_point(data = plants, aes(x=lw.rat,y=pot, color=type))+
  geom_point(data=predictions, aes(x=mean, y=pot, shape=model))

# Use intervals() to get confidence intervals for the parameters.
intervals(mod2.lme)

###########################################

plant_plot_pa_matrix <- plants %>% 
  filter(Func_group != "NONE") %>%
  group_by(plot, Func_group) %>%
  summarise(hits = 1*as.logical(sum(hits_count))) %>% 
  pivot_wider(names_from = Func_group, 
              values_from = hits,
              values_fill = list(hits = 0))

plant_sites <- plants %>%  
  group_by(site, Func_group, treatment) %>%
  summarize(hits = sum(hits_count),
            pins = n() 
  )


data(ReedfrogPred)

View(ReedfrogPred)

boxplot(propsurv~size*density*pred, data = ReedfrogPred)

ggplot(ReedfrogPred, aes(pred, propsurv, fill=factor(density), color = size))+
  geom_boxplot()+
  labs(x="", fill = "Density", y="Proportion Surviving", color="Size")+
  scale_color_brewer(palette="Dark2")

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
