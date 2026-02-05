library(ggplot2)
ggplot(mtcars,aes(mpg))+geom_histogram()
t.test(gear~am, data=mtcars).

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
