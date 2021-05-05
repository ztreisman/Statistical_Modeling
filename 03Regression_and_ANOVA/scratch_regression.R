
n <- 5000
betas <- numeric(n)
capt_b <- logical(n)
for(i in 1:n){
  x <- runif(100)
  y <- rnorm(100, mean = 1 + 2*x, sd = 0.5)
  lmi <- lm(y~x)
  betas[i] <- lmi$coefficients[2]
  capt_b[i] <- (2 > (coef(summary(lmi))[2,1]-2*coef(summary(lmi))[2,2])) & (2 < (coef(summary(lmi))[2,1]+2*coef(summary(lmi))[2,2])) 
  print(c(coef(summary(lmi))[2,1]-2*coef(summary(lmi))[2,2], coef(summary(lmi))[2,1]+2*coef(summary(lmi))[2,2]))
}
mean(capt_b)
hist(betas)


tadpoles <- read.csv("data/tadpoles.csv")
confint(lm5a)
coef(lm5a)
ests <- coef(lm5a)[2:8]
tad_model <- data.frame(var.labels=factor(names(ests), levels=names(ests)), ests, low95 = confint(lm5a)[-1,1], up95 = confint(lm5a)[-1,2])

ggplot(tad_model, aes(var.labels, ests))+
  geom_pointrange(aes(ymin=low95, ymax=up95))+
  geom_hline(yintercept=0, linetype = "dashed", color = "red")+
  labs(x = "Term", y =  "Effect")

ggplot(tadpoles, aes(fac1, response, color = fac2))+
  facet_wrap(vars(fac2))
  

table(streams$stream,streams$zinc)  
lm.streams <- lm(diversity~zinc*stream, data = streams)
Anova(lm.streams)

summary(lm(log(smass)~log(ssl)*loc, data = shiners))

lm.mass <- lm(mass.diff ~  smass + block*loc*temp*food, 
              data=na.omit(shiners))
Anova(lm.mass)

plot(lm.mass$residuals~smass, data = shiners[complete.cases(shiners),], col=loc, pch=as.numeric(food))

plot(sizepc1~ssl, data = shiners)

table(shiners$block, shiners$loc, shiners$temp, shiners$food)

shiners$sizepc1 <- prcomp(shiners[,c("ssl", "smass")], scale = TRUE)$x[,1]

shiners <- na.omit(shiners)
nrow(shiners)

lm.mass2 <- lm(mass.diff ~  loc+temp+food+loc:temp, data=shiners)
Anova(lm.mass2)

summary(lm.mass2)

