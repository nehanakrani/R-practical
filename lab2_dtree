#lab2 work: Settings for Decision Trees: R Tutorial

# Step 1
# Load Bankmarketing data
Mydata <- bank.additional
Mydata <- subset(Mydata, select = -c(duration) )
summary(bank.additional)
# output summary(bank.additional) ass in introduction took info from
#  age                 job            marital                    education        default         housing           loan            contact     
#  Min.   :17.00   admin.     :10422   divorced: 4612   university.degree  :12168   no     :32588   no     :18622   no     :33950   cellular :26144  
#  1st Qu.:32.00   blue-collar: 9254   married :24928   high.school        : 9515   unknown: 8597   unknown:  990   unknown:  990   telephone:15044  
#  Median :38.00   technician : 6743   single  :11568   basic.9y           : 6045   yes    :    3   yes    :21576   yes    : 6248                    
#  Mean   :40.02   services   : 3969   unknown :   80   professional.course: 5243                                                                    
#  3rd Qu.:47.00   management : 2924                    basic.4y           : 4176                                                                    
#  Max.   :98.00   retired    : 1720                    basic.6y           : 2292                                                                    
#                  (Other)    : 6156                    (Other)            : 1749                                                                    
#      month       day_of_week    duration         campaign          pdays          previous            poutcome      emp.var.rate      cons.price.idx 
#  may    :13769   fri:7827    Min.   :   0.0   Min.   : 1.000   Min.   :  0.0   Min.   :0.000   failure    : 4252   Min.   :-3.40000   Min.   :92.20  
#  jul    : 7174   mon:8514    1st Qu.: 102.0   1st Qu.: 1.000   1st Qu.:999.0   1st Qu.:0.000   nonexistent:35563   1st Qu.:-1.80000   1st Qu.:93.08  
#  aug    : 6178   thu:8623    Median : 180.0   Median : 2.000   Median :999.0   Median :0.000   success    : 1373   Median : 1.10000   Median :93.75  
#  jun    : 5318   tue:8090    Mean   : 258.3   Mean   : 2.568   Mean   :962.5   Mean   :0.173                       Mean   : 0.08189   Mean   :93.58  
#  nov    : 4101   wed:8134    3rd Qu.: 319.0   3rd Qu.: 3.000   3rd Qu.:999.0   3rd Qu.:0.000                       3rd Qu.: 1.40000   3rd Qu.:93.99  
#  apr    : 2632               Max.   :4918.0   Max.   :56.000   Max.   :999.0   Max.   :7.000                       Max.   : 1.40000   Max.   :94.77  
#  (Other): 2016                                                                                                                                       
#  cons.conf.idx     euribor3m      nr.employed     y        
#  Min.   :-50.8   Min.   :0.634   Min.   :4964   no :36548  
#  1st Qu.:-42.7   1st Qu.:1.344   1st Qu.:5099   yes: 4640  
#  Median :-41.8   Median :4.857   Median :5191              
#  Mean   :-40.5   Mean   :3.621   Mean   :5167              
#  3rd Qu.:-36.4   3rd Qu.:4.961   3rd Qu.:5228              
#  Max.   :-26.9   Max.   :5.045   Max.   :5228              
                                             
str(bank.additional) #slide ppt check more
barplot(table(bank.additional$job))
barplot(table(bank.additional$y))

A = sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
Train<-Mydata[A,]
Val<-Mydata[-A,]


library(rpart)  
# build a DT with outcome y on the train data
complexity=0.001
mtree <- rpart(y ~ ., data = Train, method="class", control = rpart.control(cp=complexity) )

#plot tree 
plot(mtree)
text(mtree, pretty=FALSE, cex=.6)  

# Step 3 Confusion Matrix
Yt=predict(mtree,Val,type="class") # predict results on the validation data
conf.matrix <- table(Val$y, Yt)  # build a confusion matrix
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix))
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix))
print(conf.matrix)


#Performance - Accuracy and Error Rates
acc=mean(Val$y==Yt) # accuracy
tp=sum(Val$y=='yes' & Yt=='yes')/sum(Val$y=='yes') # true positive rate
tn=sum(Val$y=='no' & Yt=='no')/sum(Val$y=='no') # true negative rate
fp=sum(Val$y=='no' & Yt=='yes')/sum(Val$y=='no') # false positive rate
fn=sum(Val$y=='yes' & Yt=='no')/sum(Val$y=='yes') # false negative rate
sprintf("Accuracy:%.2f, TP:%.2f, TN:%.2f",acc,tp,tn)# formatted printing
sprintf("Error rates: FP:%.2f, FN:%.2f",fp,fn)


#output
#sprintf("Accuracy:%.2f, TP:%.2f, TN:%.2f",acc,tp,tn)# formatted printing
#[1] "Accuracy:0.90, TP:0.18, TN:0.99"
#> sprintf("Error rates: FP:%.2f, FN:%.2f",fp,fn)
#[1] "Error rates: FP:0.01, FN:0.82"



# build a DT using complexity parameter 
complexity=0.001
mtree <- rpart(y ~ ., data = Train, method="class", control = rpart.control(cp=complexity))
#change line 38 to 40 and put here and make mtree to replace empty one # build a DT with outcome y on the train data
#complexity=0.001
#mtree <- rpart(y ~ ., data = Train, method="class", control = rpart.control(cp=complexity) )


library(rpart) 
# build a DT using complexity and split size 
complexity=0.001
split_size=200 
depth=5
error_costs=matrix(c(0,2,1,0))

mtree <- rpart(y ~ ., data = Train, method="class", control = rpart.control(cp=complexity, minsplit=split_size))
#plot tree 
plot(mtree)
text(mtree, pretty=FALSE, cex=.6)

#last in try with change depth size and run


#slide in step 7
#error_costs=matrix(c(0,2,1,0))
#mtree <- rpart(y ~ ., data = Train, method="class", control = rpart.control(minsplit=split_size, cp=complexity, maxdepth=depth), parms=list(loss=error_costs))



#NOTE ass:
#->change parameter with 80 to 70 to 30 ass task
# Split data into training (80%) and validation (20%)
#A = sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
#->split size change and run con matrix
