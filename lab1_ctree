# 1st line required in R Cloud, delete if using RStudio
# Step 1
# Load marketing data
# Mydata = read.csv("bank-additional.csv", sep = ";")
# Remove variable “duration”
# Mydata <- subset(Mydata, select = -c(duration) )
# Split data into training (80%) and validation (20%)

str(bank.additional)
bank.additional <- subset(bank.additional, select = -c(duration) )

set.seed(123)
dp <- sample(2, nrow(bank.additional), replace = TRUE, prob = c(0.8,0.2))
str(bank.additional)
training <- bank.additional[dp==1,]
training <- bank.additional[dp==2,]

#same work as above code ref
# A = sort(sample(nrow(Mydata), nrow(Mydata)*.8)) 
# Train<-Mydata[A,]  # train data
# Val<-Mydata[-A,] # validation data

library(party)
tree <- ctree(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed,data = training)
tree
plot(tree)
