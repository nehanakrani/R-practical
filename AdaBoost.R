#AdaBoost - Implementation
require(gbm)
require(MASS)
# package with the boston housing dataset
# separating training and test data
train=sample(1:506,size=374)
Boston.boost = gbm(medv ~ . , data = Boston[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
Boston.boost
summary(Boston.boost) # Summary gives a table of Variable Importance and a plot of Variable Importance

# var    rel.inf
# lstat     lstat 44.4709322
# rm           rm 28.3174351
# dis         dis  6.1154733
# crim       crim  3.9574905
# nox         nox  3.8338020
# ptratio ptratio  3.7126196
# black     black  2.9017281
# age         age  2.8652224
# tax         tax  1.8783952
# indus     indus  1.0544516
# rad         rad  0.5196584
# chas       chas  0.1936369
# zn           zn  0.1791547


# Plot of Response variable with lstat variable
plot(Boston.boost,i="lstat")
# Inverse relation with lstat variable
plot(Boston.boost,i="rm") 
# as the average number of rooms increases the the price increases



n.trees = seq(from=100 ,to=10000, by=100) # no of trees-a vector of 100 values
# Generating a Prediction matrix for each Tree
predmatrix<-predict(Boston.boost, Boston[-train,], n.trees = n.trees)
dim(predmatrix) # dimentions of the Prediction Matrix
# > dim(predmatrix) # dimentions of the Prediction Matrix
# [1] 132 100


test.error <- with(Boston[-train,], apply( (predmatrix-medv)^2,2, mean))
head(test.error) # contains the Mean squared test error for each of the 100 trees averaged


# head(Test.error)
#      100      200      300      400      500      600
# 25.86012 18.01882 15.75270 14.78562 13.86204 13.43472


# Plotting the test error vs number of trees
plot(n.trees , test.error, pch=19,col="blue", xlab="Number of Trees", ylab="Test Error", main="Performance of Boosting on Test Set")
# Adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random Forest fitted on the same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
