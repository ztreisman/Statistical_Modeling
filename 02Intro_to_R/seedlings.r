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
