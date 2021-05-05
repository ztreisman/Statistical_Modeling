Owls <- read.table(file = "data/Owls.txt", header = TRUE, dec = ".")   

names(Owls)

ftable(Owls[,1:3])

library(MuMIn)
library(ggplot2)
library(glmmTMB)
library(lattice)

ggplot(Owls, aes(SexParent, NegPerChick, fill=FoodTreatment))+
  geom_boxplot(position = position_dodge(preserve = "single"))+
  facet_wrap(~Nest)+
  scale_y_log10()


ggplot(Owls, aes(ArrivalTime,NegPerChick))+
  geom_point()+
  geom_smooth()



M.lm=lm(NegPerChick~SexParent*FoodTreatment+SexParent*ArrivalTime,data=Owls)
plot(M.lm,select=c(1))

Owls$LogNeg<-log(Owls$NegPerChick+1)

M2.lm=lm(LogNeg~SexParent*FoodTreatment+SexParent*ArrivalTime,data=Owls)
E=rstandard(M2.lm)

op<-par(mar=c(3,3,2,2))
boxplot(E~Nest,data=Owls,axes=FALSE,ylim=c(-3,3))
abline(0,0)
axis(2)
text(1:27,-2.5, levels(Owls$Nest),cex=0.75,srt=65)
par(op)



#Step 2 of protocol
library(nlme)
Form<-formula(LogNeg~SexParent*FoodTreatment+SexParent*ArrivalTime)
M.gls=gls(Form,data=Owls)

M1.lme=lme(Form,random=~1|Nest,method="REML",data=Owls)

anova(M.gls,M1.lme)

E2<-resid(M1.lme,type="normalized")
F2<-fitted(M1.lme)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~SexParent,data=Owls,main="Sex of parent",ylab=MyYlab)
boxplot(E2~FoodTreatment,data=Owls,main="Food treatment",ylab=MyYlab)
plot(x=Owls$ArrivalTime,y=E,main="Arrival time",ylab=MyYlab,xlab="Time (hours)")
par(op)


#step 7 of the protocol
M1.lme=lme(Form,random=~1|Nest,method="REML",data=Owls)
summary(M1.lme)
anova(M1.lme)

M1.opt=lme(LogNeg~FoodTreatment+ArrivalTime,random=~1|Nest,data=Owls)


M1.Full=lme(Form,random=~1|Nest,method="ML",data=Owls)
M1.A=update(M1.Full,.~.-SexParent:FoodTreatment)
M1.B=update(M1.Full,.~.-SexParent:ArrivalTime)
anova(M1.Full,M1.A)
anova(M1.Full,M1.B)


Form2<-formula(LogNeg~SexParent+FoodTreatment+SexParent*ArrivalTime)
M2.Full=lme(Form2, random= ~1| Nest, method = "ML", data = Owls)
M2.A=update(M2.Full, .~. -FoodTreatment)
M2.B=update(M2.Full, .~. -SexParent:ArrivalTime)
anova(M2.Full,M2.A)
anova(M2.Full,M2.B)



Form3 <- formula(LogNeg~SexParent+FoodTreatment+ArrivalTime)
M3.Full <- lme(Form3, random= ~1| Nest, method = "ML", data = Owls)
M3.A <- update(M3.Full, .~. -FoodTreatment)
M3.B <- update(M3.Full, .~. -SexParent)
M3.C <- update(M3.Full, .~. -ArrivalTime)
anova(M3.Full,M3.A)
anova(M3.Full,M3.B)
anova(M3.Full,M3.C)




Form4 <- formula(LogNeg ~ FoodTreatment + ArrivalTime)
M4.Full <- lme(Form4, random= ~1| Nest, method = "ML", data = Owls)
M4.A <- update(M4.Full, .~. -FoodTreatment)
M4.B <- update(M4.Full, .~. -ArrivalTime)
anova(M4.Full,M4.A)
anova(M4.Full,M4.B)


M5 <- lme(LogNeg ~ FoodTreatment + ArrivalTime, random= ~1| Nest, method = "REML", data = Owls)
summary(M5)


E2<-resid(M5,type="normalized")
F2<-fitted(M5)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"

plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
boxplot(E2~SexParent,data=Owls,main="Sex of parent",ylab=MyYlab)
boxplot(E2~FoodTreatment,data=Owls,main="Food treatment",ylab=MyYlab)
plot(x=Owls$ArrivalTime,y=E,main="Arrival time",ylab=MyYlab,xlab="Time (hours)")
par(op)


intervals(M1.lme)$fixed[3:4, c(1,3)]
confint(M1.wrong)[3:4,]


(intervals(M1.lme)$fixed[2:4, c(1,3)]-confint(M1.wrong)[2:4,])/confint(M1.wrong)[2:4,]

Owls$predM1lme <- predict(M1.lme)
Owls$predM1wrong <- predict(M1.wrong)

ggplot(Owls, aes(y=Nest))+
  stat_summary(aes(x=predM1lme, color="Nest random"), fun.data = "mean_cl_boot")+
  stat_summary(aes(x=predM1wrong, color="Nest fixed"), fun.data = "mean_cl_boot")+
  xlab("Predicted Log Negotiations Per Chick")+
  geom_vline(xintercept = mean(Owls$LogNeg))
  

library(lattice)

xyplot(E2~ArrivalTime|SexParent*FoodTreatment,data=Owls,
       ylab="Residuals",xlab="Arrival time (hours)",
       panel=function(x,y){
         panel.grid(h=-1, v= 2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5,col=1,lwd=2)})




library(mgcv)
M6 <- gamm(LogNeg ~ FoodTreatment + +s(ArrivalTime),
           random=list(Nest=~1),data=Owls)

summary(M6)
plot(M6$gam)
anova(M6$gam)
summary(M6$gam)


M7=gamm(NegPerChick~SexParent*FoodTreatment+
          s(ArrivalTime),
        random=list(Nest=~1),data=Owls)

summary(M7$lme)

M7l=gamm(LogNeg~SexParent*FoodTreatment+
          s(ArrivalTime),
        random=list(Nest=~1),data=Owls)

M8=gamm(NegPerChick~FoodTreatment+
          s(ArrivalTime,by=as.numeric(SexParent=="Female"))+
          s(ArrivalTime,by=as.numeric(SexParent=="Male")),
        random=list(Nest=~1),data=Owls)

AIC(M1.opt,M7l$lme)

############################################
Owls$LogNeg<-log10(Owls$NegPerChick+1)
Form<-formula(LogNeg~SexParent*(FoodTreatment+ArrivalTime))
M2.gls<-gls(Form,correlation=corCompSymm(form=~1|Nest),
            method="REML",
            data=Owls)


library(lattice)
xyplot(LogNeg~ArrivalTime|Nest,data=Owls,type="h",col=1,
       subset=(FoodTreatment=="Deprived"),main="Deprived")




M1.lme=lme(Form,random=~1|Nest,method="REML",data=Owls)
M2.gls<-gls(Form,correlation=corCompSymm(form=~1|Nest),
            method="REML",
            data=Owls)

M3.gls<-gls(Form,correlation=corAR1(form=~1|Nest/FoodTreatment),
            method="REML",
            data=Owls)


M4 <- lme(Form,random=~1|Nest,method="REML",data=Owls,
          correlation=corAR1(form=~1|Nest/FoodTreatment),)

summary(M2.gls)
summary(M3.gls)



xyplot(LogNeg~ArrivalTime|Nest,data=Owls,type="h",col=1,
       subset=(FoodTreatment=="Satiated"))

################################################

Owls$NCalls<-Owls$SiblingNegotiation
Owls$LBroodSize<-log(Owls$BroodSize)

Form<-formula(NCalls~offset(LBroodSize)+
                SexParent*FoodTreatment+
                SexParent*ArrivalTime)
O1<-glm(Form,family=quasipoisson,data=Owls)
drop1(O1,test="F")
O2<-update(O1,.~. -SexParent*ArrivalTime)
drop1(O2,test="F")

Form<-formula(NCalls~offset(LBroodSize)+
                FoodTreatment+
                ArrivalTime)
O3<-glm(Form,family=quasipoisson,data=Owls)
drop1(O3,test="F")

#GEE:
library(geepack)
Form<-formula(NCalls~offset(LBroodSize)+
                SexParent*FoodTreatment+
                SexParent*ArrivalTime)
O2<-geeglm(Form,data=Owls,
           family=poisson,id=Nest,corstr = "exchangeable")


N<-length(Owls$Nest)
NewLevels<-c(paste(unique(Owls$Nest),".Dep",sep=""),
             paste(unique(Owls$Nest),".Sat",sep=""))
Owls$NestNight<-factor(levels=NewLevels)

for (i in 1:N){
  if (Owls$FoodTreatment[i]=="Deprived"){Owls$NestNight[i] <- paste(Owls$Nest[i],".Dep",sep="")}
  if (Owls$FoodTreatment[i]=="Satiated"){Owls$NestNight[i] <- paste(Owls$Nest[i],".Sat",sep="")}
}
cbind(Owls$Nest,Owls$FoodTreatment,Owls$NestNight)[1:5,]

cbind(Owls$Nest,Owls$FoodTreatment,Owls$NestNight)[1:5,]

#Alternative to the for loop with two if-statements:
Owls$NestNight<-factor(ifelse(Owls$FoodTreatment == "Deprived",
                              paste(Owls$Nest, ".Dep",sep=""),
                              paste(Owls$Nest, ".Sat",sep="")))



O3<-geeglm(Form,data=Owls,
           family=poisson,id=NestNight,corstr = "ar1")
summary(O3)



O3.A<-geeglm(NCalls~offset(LBroodSize)+
               SexParent+FoodTreatment+
               SexParent*ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")


O3.B<-geeglm(NCalls~offset(LBroodSize)+
               SexParent*FoodTreatment+
               SexParent+ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")

anova(O3,O3.A)
anova(O3,O3.B)


O4<-geeglm(NCalls~offset(LBroodSize)+
             SexParent+FoodTreatment+
             SexParent*ArrivalTime,data=Owls,
           family=poisson,id=NestNight,corstr = "ar1")

O4.A<-geeglm(NCalls~offset(LBroodSize)+
               SexParent+FoodTreatment+
               SexParent+ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")


O4.B<-geeglm(NCalls~offset(LBroodSize)+
               SexParent+
               SexParent*ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")


anova(O4,O4.A)
anova(O4,O4.B)



O5<-geeglm(NCalls~offset(LBroodSize)+
             SexParent+FoodTreatment+ArrivalTime,data=Owls,
           family=poisson,id=NestNight,corstr = "ar1")

O5.A<-geeglm(NCalls~offset(LBroodSize)+
               FoodTreatment+ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")

O5.B<-geeglm(NCalls~offset(LBroodSize)+
               SexParent+ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")

O5.C<-geeglm(NCalls~offset(LBroodSize)+
               SexParent+ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")

anova(O5,O5.A)
anova(O5,O5.B)
anova(O5,O5.C)

O6<-geeglm(NCalls~offset(LBroodSize)+
             FoodTreatment+ArrivalTime,data=Owls,
           family=poisson,id=NestNight,corstr = "ar1")


O6.A<-geeglm(NCalls~offset(LBroodSize)+
               ArrivalTime,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")


O6.B<-geeglm(NCalls~offset(LBroodSize)+
               FoodTreatment,data=Owls,
             family=poisson,id=NestNight,corstr = "ar1")


anova(O6,O6.A)
anova(O6,O6.B)
summary(O6)

###################################

#Owls

library(AED) ; data(Owls)
library(nlme)
Owls$NCalls<-Owls$SiblingNegotiation
Owls$LBroodSize<-log(Owls$BroodSize)
Owls$fNest<-factor(Owls$Nest)


#GLMM
library(lme4)

O1.lmer<-glmer(NCalls~offset(LBroodSize)+
                SexParent*FoodTreatment+
                SexParent*ArrivalTime+(1|fNest),data=Owls,
              family=poisson)
summary(O1.lmer)

O2.lmer<-glmer(NCalls~offset(LBroodSize)+
                SexParent*FoodTreatment+
                SexParent+ArrivalTime+(1|fNest),data=Owls,
              family=poisson)
anova(O1.lmer,O2.lmer,test="F")



O3.lmer<-glmer(NCalls~offset(LBroodSize)+
                FoodTreatment+
                ArrivalTime+(1|fNest),data=Owls,
              family=poisson)
summary(O3.lmer)





library(mgcv)
M.gamm<-gamm(SiblingNegotiation~offset(BroodSize)+
                SexParent*FoodTreatment+s(ArrivalTime, by=SexParent),
              random=list(Nest=~1),data=Owls,
              family=poisson)

summary(M.gamm$gam)
anova(M.gamm$gam)
plot(M.gamm$gam)


summary(M.gamm$lme)

E4<-resid(O4.gamm$lme,type="normalized")

M.gamm2<-uGamm(SiblingNegotiation~offset(BroodSize)+
               SexParent*FoodTreatment+s(ArrivalTime, by=SexParent),
             random=list(Nest=~1),data=Owls,
             family=poisson)
M.gamm3<-uGamm(SiblingNegotiation~offset(BroodSize)+
                 SexParent*FoodTreatment+s(ArrivalTime),
               random=list(Nest=~1),data=Owls,
               family=poisson)


dredge(M.gamm2)

AIC(M.gamm2, M.gamm3)

intervals(O4.gamm$lme,which="var-cov")



eta <- fitted(O4.gamm$lme)+Owls$LBroodSize
fv  <- exp(eta)
res.raw <- Owls$NCalls - fv
res.P <- (Owls$NCalls - fv) / sqrt(fv )
sd(res.P)

data("Owls")
bwplot(reorder(Nest,NegPerChick) ~ NegPerChick | FoodTreatment:SexParent,
       data=Owls)
dotplot(reorder(Nest,NegPerChick) ~ NegPerChick| FoodTreatment:SexParent,
        data=Owls)
## Not run: 
## Fit negative binomial model with "constant" Zero Inflation :
owls_nb1 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent +
                      (1|Nest)+offset(log(BroodSize)),
                    family = nbinom1(), zi = ~1, data=Owls)
owls_nb1_bs <- update(owls_nb1,
                      . ~ . - offset(log(BroodSize)) + log(BroodSize))
fixef(owls_nb1_bs)


`(0+x|group)` = `(-1+x|group)`  random slope of x within group: no variation in intercept
`(1|group) + (0+x|group)`   uncorrelated random intercept and random slope within group

## Specifying effects

(Modified from Robin Jeffries, UCLA via Ben Bolker:)

R formula      meaning
`(1|group)`   random group intercept
`(x|group)` = `(1+x|group)`  random slope of x within group with correlated intercept
`(0+x|group)` = `(-1+x|group)`  random slope of x within group: no variation in intercept
`(1|group) + (0+x|group)`   uncorrelated random intercept and random slope within group
`(1|site/block)` = `(1|site)+(1|site:block)`  intercept varying among sites and among blocks within sites (nested random effects)
`site+(1|site:block)`  *fixed* effect of sites plus random variation in intercept among blocks within sites
`(x|site/block)` = `(x|site)+(x|site:block)` = `(1 + x|site)+(1+x|site:block)`  slope and intercept varying among sites and among blocks within sites
`(x1|site)+(x2|block)`  two different effects, varying at different levels
`x*site+(x|site:block)`  fixed effect variation of slope and intercept varying among sites and random variation of slope and intercept among blocks within sites
`(1|group1)+(1|group2)`  intercept varying among crossed random effects (e.g. site, year)
## Mixed model cheat sheet

\begin{align*}
\beta_0 + \beta_1X_{i} + \epsilon_{si} & \text{n/a (Not a mixed-effects model) & \\
  (\beta_0 + b_{S,0s}) + \beta_1X_i + \epsilon_{si} & `(1|group)` &  random group intercept \\
  (\beta_0 + b_{S,0s}) +  (\beta_1 + b_{S,1s}) X_i + e_{si} & `(x|group)` = `(1+x|group)` & random slope of x within group with correlated intercept \\
  (\beta_0 + b_{S,0s} + b_{I,0i}) + (\beta_1 + b_{S,1s}) X_i + e_{si} & `∼ X + (1 + X∣Subject) + (1∣Item)` \\
  \text{As above, but $S_{0s}$, $S_{1s}$ independent} & `∼ X + (1∣Subject) + (0 + X∣ Subject) + (1∣Item)` \\
  (\beta_0 + b_{S,0s} + b_{I,0i}) + \beta_1X_i + e_{si} & `∼ X + (1∣Subject) + (1∣Item)` \\ 
  (\beta_0 + b_{I,0i}) +  (\beta_1 + b_{S,1s})X_i + e_{si} & `∼ X + (0 + X∣Subject) + (1∣Item)` \\
  & `(1|site/block)` = `(1|site)+(1|site:block)` & intercept varying among sites and among blocks within sites (nested random effects)
\end{align*}
  
  
Modified from: [https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet?lq=1] (Livius)
  
## Lab
  
zipm3 = glmmTMB(count~spp * mined + (1|site), zi=~spp * mined, Salamanders, family="poisson")
summary(zipm3)

Msal1 <- glmmTMB(count~mined, data=Salamanders)
summary(Msal1)  

Msal2 <- glmmTMB(count~mined+cover+DOP+Wtemp+DOY+spp, data = Salamanders)
Anova(Msal2)
summary(Msal2)

Msal3 <- glmmTMB(count~mined+cover+DOP+Wtemp+DOY+spp, data = Salamanders, 
                 family = "poisson")
Msal4 <- glmmTMB(count~mined+cover+DOP+Wtemp+DOY+spp, data = Salamanders, 
                 family = "nbinom1")
Msal5 <- glmmTMB(count~mined+cover+DOP+Wtemp+DOY+spp, data = Salamanders, 
                 family = "nbinom2")
summary(Msal3)
summary(Msal4)
summary(Msal5)

Msal6 <- glmmTMB(count~mined+cover+DOP+Wtemp+DOY+spp + (1|site), 
                 data = Salamanders, 
                 family = "nbinom1") # chosen based on AIC
summary(Msal6)

Anova(Msal4)
Anova(Msal6)

Msal7 <- glmmTMB(count~mined+cover+DOP+Wtemp+DOY+spp + (1|site), 
                 data = Salamanders, 
                 zi = ~cover+DOP+Wtemp+DOY,
                 family = "nbinom1") # Again, match this to your choice above.
summary(Msal7)
