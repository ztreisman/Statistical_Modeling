# Chapter 8 Lab: Decision Trees

# Fitting Classification Trees
#install.packages("tree") # I had to update R to 3.6 before I could do this.
library(tree)
library(ISLR)

# Work with the Carseats data. What does it take to sell a lot of (simulated) car seats? 
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
head(Carseats)

# Build a tree to decide if sales will be high. Don't use the Sales variable. That's cheating.
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=2) # pretty = how much to abbreviate. See ShelveLoc. 
tree.carseats # Note the legend at the top of the output.

# Cross validate to see how good the model is.
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200

# Try pruning the tree
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats) # k is the parameter we called alpha
cv.carseats # dev is misclassification rate (out of 200)
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b") # the 9 node tree is best
plot(cv.carseats$k,cv.carseats$dev,type="b") 
prune.carseats=prune.misclass(tree.carseats,best=9) # prune the tree to 9 nodes
plot(prune.carseats)
text(prune.carseats,pretty=3)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200

# Confirm that 9 leaves really is best. A bigger tree does worse.
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

# Fitting Regression Trees

# Let's regress some trees on Boston. We will determine that big houses in wealthy neighborhoods cost more.
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

# Should we prune the tree?
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b') # guess not.

# But we could anyway.
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

# Use the unpruned tree for prediction
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1) # Points on the line are correct predictions
mean((yhat-boston.test)^2)

# Bagging and Random Forests

library(randomForest)
set.seed(1)

# Random Forest using all predictors is bagging. (mtry = 13)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1) # Points on the line are correct predictions.
mean((yhat.bag-boston.test)^2)

# Try fewer trees
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# Now for a real random forest
set.seed(1)
# by default mtry = p/3 for regression or sqrt(p) for classification. 
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
plot(yhat.rf, boston.test)
abline(0,1) # Look any better?
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

# Boosting


library(gbm)
set.seed(1)
# to do classification, use distribution="bernoulli"
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2) # This model is of comparable quality to the random forest

# We can change the shrinkage parameter (lambda in the text)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

### Exercise:

#   Try different tree based methods on the Auto data set. Can you predict origin? Cylinders? Quarter mile times? How do these 
#   models compare to other models we have encountered earlier?

