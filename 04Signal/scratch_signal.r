library(ggplot2)
library(emdbook)
library(lubridate)
library(splines)
library(gam)

curve(2 * exp(-x/2), from = 0, to = 7, ylim = c(0, 2), ylab = "")
curve(2 * exp(-x), add = TRUE, lty = 4)
curve(x * exp(-x/2), add = TRUE, lty = 2)
curve(2 * x * exp(-x/2), add = TRUE, lty = 3)
text(0.4, 1.9, expression(paste("exponential: ", 2 * e^(-x/2))), adj = 0)
text(4, 0.7, expression(paste("Ricker: ", x * e^(-x/2))))
text(4, 0.25, expression(paste("Ricker: ", 2 * x * e^(-x/2))), adj = 0)
text(2.8, 0, expression(paste("exponential: ", 2 * e^(-x))))

xvec = seq(0, 7, length = 100)
exp1_vec = 2 * exp(-xvec/2)
exp2_vec = 2 * exp(-xvec)
plot(xvec, exp1_vec, type = "l", ylim = c(0, 2), ylab = "")
lines(xvec, exp2_vec, lty = 4)

matplot(xvec, cbind(exp1_vec, exp2_vec), type = "l", ylab = "")

expfun = function(x, a = 1, b = 1) {a * exp(-b * x)}
exp1_vec = sapply(xvec, expfun, a = 2, b = 1/2)
exp2_vec = sapply(xvec, expfun, a = 2, b = 1)

ggplot(SeedPred, aes(date, seeds))+
  geom_jitter(alpha= 0.3)

str(mpg)
ggplot(mtcars, aes(mpg, hp)) + 
  geom_point(alpha=0.5) +
  geom_smooth(method = lm, formula = y~x, se=F) +
  geom_smooth(method = lm, formula = y~poly(x,2), se=F, color="red")

ggplot(DamselRecruitment, aes(init, surv))+geom_jitter()+scale_x_log10()

# exp decay

decay <- data.frame(age = sample(1:120, 100, replace = TRUE), species = sample(c("a","b","c"), 100, replace = TRUE))
decay$mass <- expfun(decay$age, a=10, b=0.01 + 0.002*(decay$species=="b")-0.005*(decay$species=="c"))*rnorm(100, 1, 0.1)

plot(mass~age, data=decay, col=species)

summary(lm(log(mass)~age*species, data=decay))
summary(lm(mass~age*species, data=decay))


library(plotrix)
par(mfrow=c(2,2),mar=c(0,0,0,0),lwd=2,cex=1.5)
eqcex=0.9
curve(ifelse(x<3,0,3),from=1,to=9,ylim=c(-1,5),
      xlim=c(0,11),axes=FALSE,ann=FALSE,type="s")
text(0,0,expression(a[1]))
text(10,3,expression(a[2]))
segments(3,-0.5,3,3.5,col="gray",lty=2)
text(3,-1,expression(s[1]))
corner.label("threshold:",adj=0)
corner.label(expression(paste(f(x)==a[1]," if ",x<s[1])),adj=0,yoff=0.13,cex=eqcex)
corner.label(expression(paste(phantom(f(x))==a[2]," if ",x>s[1])),adj=0,yoff=0.21,cex=eqcex)
box(col="gray")
##
##curve(ifelse(x<3,1,1+0.75*(x-3)),from=0,to=10,ylim=c(0,8),axes=FALSE,ann=FALSE)
curve(ifelse(x<5,x,5),from=0,to=10,xlim=c(-1,12),ylim=c(0,8),axes=FALSE,ann=FALSE)
corner.label("hockey stick:",adj=0)
corner.label(expression(paste(f(x)==a*x," if ",x<s[1])),adj=0,yoff=0.13,cex=eqcex)
corner.label(expression(paste(phantom(f(x))==a*s[1]," if ",x>s[1])),adj=0,yoff=0.21,cex=eqcex)
segments(5,0.5,5,5.5,col="gray",lty=2)
segments(c(1,2),c(1,1),c(2,2),c(1,2),col="gray")
text(2.5,1.5,"a")
text(5,0,expression(s[1]))
text(11,5,expression(a*s[1]))
box(col="gray")
##
a=2
s1=4
b=0.5
curve(ifelse(x<s1,a*x,(a*s1)-b*(x-s1)),from=0,to=20,
      ylim=c(0,12),axes=FALSE,ann=FALSE)
#curve(ifelse(x<6,0.5*x,3+1.5*(x-6)),add=TRUE,lty=2)
corner.label("general piecewise linear:",adj=0)
corner.label(expression(paste(f(x)==a*x," if ",x<s[1])),adj=0,yoff=0.13,cex=eqcex)
corner.label(expression(paste(phantom(f(x))==a*s[1]-b*(x-s[1])," if ",x>s[1])),
             adj=0,yoff=0.21,cex=eqcex)
segments(s1,0.5,s1,9,col="gray",lty=2)
segments(c(1,2),c(a,a),c(2,2),c(a,2*a),col="gray")
x1=10
x2=12
y1 = a*s1-b*(x1-s1)
y2 = a*s1-b*(x2-s1)
segments(c(x1,x1),c(y1,y2),
         c(x1,x2),c(y2,y2),col="gray")
text(x1-0.2,(y1+y2)/2,"-b",adj=1)
text(2.5,3,"a")
text(4,0,expression(s[1]))
box(col="gray")
##
## splines?
x1 <- 1:6
y1 <- c(0,2,4,1,2,3)
s1 <- splinefun(x1,y1)
curve(s1,axes=FALSE,ann=FALSE,from=0.5,to=6.5,ylim=c(0,5))
points(x1,y1,pch=16)
#y2 <- c(1,1.5,2,2.1,2.2,2.3)
#points(x1,y2,pch=1)
#s2 <- splinefun(x1,y2)
#curve(s2,add=TRUE,lty=2)
corner.label("splines:",adj=0)
corner.label("f(x) is complicated",adj=0,yoff=0.13,cex=eqcex)
box(col="gray")

SoilMoist <- read.csv("data/SoilMoisture_ALL.csv") # Alexia Cooper's data
SoilMoist$TimeStamp <- mdy(SoilMoist$TimeStamp)
SoilMoist$pred <- predict(lm(Moisture~ns(TimeStamp,4)+Treat, data = SoilMoist))
ggplot(SoilMoist, aes(TimeStamp,Moisture, color = Treat))+
  facet_wrap(~Site)+geom_point()+
  geom_line(aes(y=pred))

lilymod1 <- lm(flowers~vegetative+gopher, data=Lily_sum)

Lily_sum$predictions <- fitted(lilymod1)
ggplot(Lily_sum, aes(vegetative))+
  geom_point(aes(y=flowers))+
  geom_point(aes(y=predictions, color="red"), show.legend = FALSE)

lilymod2 <- lm(flowers~vegetative+gopher+moisture*rockiness, data=Lily_sum)
summary(lilymod2)
AIC(lilymod2)
Lily_sum$predictions2 <- fitted(lilymod2)
ggplot(Lily_sum, aes(x,y))+
  geom_point(aes(size=flowers), alpha = 0.7, color = "green")+
  geom_point(aes(size=predictions2, color="red"), alpha=0.7,show.legend = FALSE)

pairs(Lily_sum[,c("flowers", "vegetative", "gopher", "moisture", "rockiness")])
library(GGally)
ggpairs(Lily_sum, columns = c("flowers", "vegetative", "gopher", "moisture", "rockiness"), upper = list(continuous = wrap("cor", family="sans")))

lilymod3 <- glm(flowers~vegetative+gopher+moisture*rockiness, family=poisson, data=Lily_sum)
summary(lilymod3)
Lily_sum$predictions3 <- fitted(lilymod3)
ggplot(Lily_sum, aes(x,y))+
  geom_point(aes(size=flowers), alpha = 0.7, color = "green")+
  geom_point(aes(size=predictions3, color="red"), alpha=0.7,show.legend = FALSE)

lilies$pred1 <- fitted(lilymod1)
lilies$pred2 <- exp(fitted(lilymod2)+vr/2)-1
lilies$pred3 <- fitted(lilymod3)
lilies$pred4 <- fitted(lilymod4)
lilies$pred5 <- fitted(lilymod5)


p <- ggplot(lilies, aes(flowers))+
  geom_point(aes(y=pred1, color="lilymod1"))+
  geom_point(aes(y=pred2, color="lilymod2"))+
  geom_point(aes(y=pred3, color="lilymod3"))+
  geom_point(aes(y=pred4, color="lilymod4"))+
  geom_point(aes(y=pred5, color="lilymod5"))+
  geom_abline(slope = 1, intercept = 0)+
  scale_color_manual("model", values = c("darkolivegreen", "darkorange", "firebrick", "deepskyblue", "darkseagreen"))+
  ylab("predicted flowers")
p
p+scale_x_log10()+scale_y_log10()

mae <- function(prediction, actual){
  sum(abs(actual - prediction))/length(actual)
}
mae(lilies$pred1, lilies$flowers)
mae(lilies$pred2, lilies$flowers)
mae(lilies$pred3, lilies$flowers)

glm(flowers~vegetative+gopher, data = lilies, family=gaussian(link="log"), start=coef(lilymod2))
coef(lilymod3)

library(MuMIn)

model.sel(lilymod1,lilymod3,lilymod4,lilymod5)
