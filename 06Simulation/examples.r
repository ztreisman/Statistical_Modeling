library(ggplot2)
library(glmmTMB)

set.seed(3)

# A linear function model with normal errors:
N <- 50  
x1 <- 1:N # Define inputs
x2 <- runif(N, 0, 5)
a <- 2; b1 <- 1; b2 <- -3 # Set parameters
y_det <- a + b1*x1 +b2*x2 # Compute signal part of output
y <- rnorm(N, mean = y_det, sd = 2) # Create noise around the signal
data1 <- data.frame(x1=x1, x2=x2, y=y)
ggplot(data1, aes(x1,y, color=x2))+
  geom_point()
write.csv(data1, file = "Simulation/data1.csv")

# An exponential function model with poisson errors:
N <- 50  
x1 <- runif(N, min = 0, max = 5) # Define inputs
k <- 5 # Set parameters
x2 <- sample(c(0,1), N, replace = TRUE) # Define inputs
a <- 3; b1 <- -0.2; b2 <- 0.5  # parameters depend on input
y_det <- exp(a+b1*x1+b2*x2) # Compute signal part of output
y <- rpois(N, lambda = y_det) # Create noise
data2 <- data.frame(x1=x1,y=y,x2=factor(x2))
ggplot(data2, aes(x1,y, color=x2))+
  geom_point() #+ 
  geom_function(fun = function(x) (20)/(1+x), aes(color="0")) +
  geom_function(fun = function(x) (10)/(2+x), aes(color="1"))
  
glm2 <- glm(y~x1+x2, data = data2, family = poisson())  
summary(glm2)

write.csv(data2, file = "Simulation/data2.csv")


# A binary categorical output

N <- 50  
x1 <- runif(N, min = 0, max = 5) # Define inputs
x2 <- sample(c(0,1), N, replace = TRUE) # Define inputs
a <- -5; b1 <- 3; b2 <- -2 # parameters depend on input
y_det <- plogis(a+b1*x1+b2*x2) # Compute signal part of output
y <- rbinom(N, size = 1, prob = y_det) # Create noise
data3 <- data.frame(x1=x1,x2=factor(x2),y=y)
ggplot(data3, aes(x1,y, color=x2))+
  geom_point()+
  geom_line(aes(y=y_det))

write.csv(data3, file = "Simulation/data3.csv")


# lots of variables

N <- 50  
x1 <- runif(N, min = 0, max = 2) # Define inputs
x2 <- runif(N, min = 0, max = 2) # Define inputs
x3 <- runif(N, min = 0, max = 2) # Define inputs
x4 <- runif(N, min = 0, max = 2) # Define inputs
x5 <- runif(N, min = 0, max = 2) # Define inputs
x6 <- runif(N, min = 0, max = 2) # Define inputs

a <- 2; b1 <- 3; b2 <- 1; b3 <- 1.5; b4 <- 2; b5 <- -2; b6 <- 0 # parameters depend on input
y_det <- a+b1*x1+b2*x2+b3*x3+b4*x4+b5*x5+b6*x6 # Compute signal part of output
y <- rnorm(N, mean = y_det, sd=3) # Create noise
data4 <- data.frame(x1=x1, x2=x2, x3=x3,x4=x4,x5=x5,x6=x6,y=y)
ggplot(data4, aes(x1,y))+
  geom_point()
ggplot(data4, aes(x2,y))+
  geom_point()
ggplot(data4, aes(x3,y))+
  geom_point()
ggplot(data4, aes(x4,y))+
  geom_point()
ggplot(data4, aes(x5,y))+
  geom_point()
ggplot(data4, aes(x6,y))+
  geom_point()
lm4 <- lm(y~x1+x2+x3+x4+x5+x6, data=data4)
summary(lm4)

write.csv(data4, file = "Simulation/data4.csv")


# lots of variables, nbinom

N <- 50  
x1 <- runif(N, min = 0, max = 2) # Define inputs
x2 <- runif(N, min = 0, max = 2) # Define inputs
x3 <- runif(N, min = 0, max = 2) # Define inputs
x4 <- runif(N, min = 0, max = 2) # Define inputs
x5 <- runif(N, min = 0, max = 2) # Define inputs
x6 <- runif(N, min = 0, max = 2) # Define inputs

k <- 2 # Set parameters

a <- 2; b1 <- 0.3; b2 <- 1; b3 <- 1.3; b4 <- 2; b5 <- -2; b6 <- 0 # parameters depend on input
y_det <- exp(a+b1*x1+b2*x2+b3*x3+b4*x4+b5*x5+b6*x6) # Compute signal part of output
y <- rnbinom(N, mu = y_det, size = k) # Create noise
data5 <- data.frame(x1=x1, x2=x2, x3=x3,x4=x4,x5=x5,x6=x6,y=y)
ggplot(data5, aes(x1,y))+
  geom_point()
ggplot(data5, aes(x2,y))+
  geom_point()
ggplot(data5, aes(x3,y))+
  geom_point()
ggplot(data5, aes(x4,y))+
  geom_point()
ggplot(data5, aes(x5,y))+
  geom_point()
ggplot(data5, aes(x6,y))+
  geom_point()
glm5 <- glmmTMB(y~x1+x2+x3+x4+x5+x6, data=data5, family = nbinom2)
summary(glm5)

write.csv(data5, file = "Simulation/data5.csv")


# lots of variables, interactions

N <- 50  
x1 <- runif(N, min = 0, max = 2) # Define inputs
x2 <- runif(N, min = 0, max = 2) # Define inputs
x3 <- runif(N, min = 0, max = 2) # Define inputs
x4 <- runif(N, min = 0, max = 2) # Define inputs
x5 <- runif(N, min = 0, max = 2) # Define inputs
x6 <- runif(N, min = 0, max = 2) # Define inputs

a <- 2; b1 <- 3; b2 <- 1; b3 <- 1.5; b4 <- 2; b5 <- -2; b6 <- 0 # parameters depend on input
b12 <- -2; b13 <- 1
y_det <- a+b1*x1+b2*x2+b3*x3+b4*x4+b5*x5+b6*x6+b12*x1*x2+b13*x1*x3 # Compute signal part of output
y <- rnorm(N, mean = y_det, sd=3) # Create noise
data6 <- data.frame(x1=x1, x2=x2, x3=x3,x4=x4,x5=x5,x6=x6,y=y)
ggplot(data6, aes(x1,y))+
  geom_point()
ggplot(data6, aes(x2,y))+
  geom_point()
ggplot(data6, aes(x3,y))+
  geom_point()
ggplot(data6, aes(x4,y))+
  geom_point()
ggplot(data6, aes(x5,y))+
  geom_point()
ggplot(data6, aes(x6,y))+
  geom_point()
lm4 <- lm(y~x1+x2+x3+x4+x5+x6, data=data4)
summary(lm4)

write.csv(data6, file = "Simulation/data6.csv")
