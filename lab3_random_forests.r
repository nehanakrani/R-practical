# Step 1  ------------
#Mydata <- read(bank.additional, header= TRUE, sep= " ; ")
Mydata <- bank.additional
Mydata <- subset(Mydata, select = -c(duration) )

# Split data into training (80%) and validation (20%)
A <- sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
Train <- Mydata[A,]  # train data
Val <- Mydata[-A,] # validation data

# Step 2  ------------
# load the library randomForest
library(randomForest) 
# build a Random Forest of 10 DTs with outcome y on the train data
rf <- randomForest(as.factor(y) ~ ., data = Train, ntree=50, mtry = 15, sampsize=c(800,400))

# plot importance of variables
varImpPlot(rf) 
plot(rf)
print(rf)

# Step 3  ------------
Yt=predict(rf,Val)# predict results on the validation data

conf.matrix <- table(Val$y, Yt)  # build a confusion matrix
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix))
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix))
print(conf.matrix)
acc=mean(Val$y==Yt) # accuracy


# Step 3  ------------
#Performance (Confusion Matrix) 
tp=sum(Val$y=='yes' & Yt=='yes')/sum(Val$y=='yes') # true positive rate
tn=sum(Val$y=='no' & Yt=='no')/sum(Val$y=='no') # true negative rate
fp=sum(Val$y=='no' & Yt=='yes')/sum(Val$y=='no') # false positive rate
fn=sum(Val$y=='yes' & Yt=='no')/sum(Val$y=='yes') # false negative rate
sprintf("Accuracy:%.2f, TP:%.2f, TN:%.2f",acc,tp,tn)# formatted printing
sprintf("Error rates: FP:%.2f, FN:%.2f",fp,fn)


# The performance (rates of False Positive and False Negative errors as well as the prediction accuracy) of RF depends on the following settings:
# 1 Number of DTs
# 2 Number of variables tried for each split
# 3 Number of data samples taken from each class
# To maximise the performance the above settings are changed during experiments  


## build a Random Forest of 50 DTs with outcome y on the train data
#rf <- randomForest(y ~ ., data = Train, ntree=50)

# Step 5  ------------
#Adjusting the number of variables
#rf <- randomForest(y ~ ., data = Train, ntree=50, mtry = 15)



# Step 5  ------------
#Adjusting the number of data samples
# build a Random Forest of 50 DTs, mtry=15, 
# and taking 800 samples from "no" and 400 from "yes" outcomes
# rf <- randomForest(y ~ ., data = Train, ntree=10, mtry=15, sampsize=c(800,400))
