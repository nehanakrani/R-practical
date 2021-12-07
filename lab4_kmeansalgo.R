# load iris data
library(datasets)
data(iris)
library(help = "datasets")
str(iris)
summary(iris)
# view first 10 rows of iris data
head(iris, n=10)
# plot the data by variables 3 and 4
plot(iris[, 3:4], pch=21)

# set seed for random number generator
set.seed(23)
# k-means with 3 clusters, max 10 iterations
irisCluster <- kmeans(iris[, 3:4], 3, iter.max=10, nstart=1, algorithm="Lloyd")
# plot clustering results
plot(iris[,3:4], pch=21, col=c("red","green3","blue")[irisCluster$cluster])
# plot centroids as âXâ
points(irisCluster$centers,pch=88,bg="black")

irisCluster
# K-means clustering with 3 clusters of sizes 50, 54, 46

# Cluster means:
#   Petal.Length Petal.Width
# 1     1.462000    0.246000
# 2     4.292593    1.359259
# 3     5.626087    2.047826

# Clustering vector:
#   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
#  [79] 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 2 3 3 2 2 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3

# Within cluster sum of squares by cluster:
# [1]  2.02200 14.22741 15.16348
#  (between_SS / total_SS =  94.3 %)

# Available components:

# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"  



Mydata <- bank.additional
Mydata <- subset(Mydata, select = -c(duration) )
str(Mydata)
summary(Mydata)
# view first 10 rows of Mydata
head(Mydata, n=10)
# plot the data by variables 3 and 4
plot(bank.additional[, 3:4], pch=21)

set.seed(23)
# k-means with 3 clusters, max 10 iterations
irisCluster <- kmeans(bank.additional[, 1:1], 3, iter.max=10, nstart=1, algorithm="Lloyd")
# plot clustering results
plot(bank.additional[,1:1], pch=21, col=c("red","green3","blue")[irisCluster$cluster])
# plot centroids as "X"
points(irisCluster$centers,pch=88,bg="black")




#bank.additiona$y=factor(bank.additional, levels = c('No', 'Yes'), labels = c(1,2))
  
#WIP

x <- c("red", "green", "blue") # c denotes a vector
x <- c(4, 7, 6, 5, 2, 8) # <- means to assign
x[c(TRUE, FALSE)] # prints 4 6 2
x <- 1:10
x[c(TRUE, FALSE)] <- 1 
print(x)# prints 1  2  1  4  1  6  1  8  1 10

# a list includes variables x,y,z
L <- list(x = c(1:5), y = c("a", "b", "c"), z ="green")
L[[2]] # content of variable y
L$y
L[c(1, 3)] # content of variables x and z
L[c("x", "y")]


d <- data.frame(x = 1:10, y = letters[1:10], z = LETTERS[1:10])
d[1] # content of variable x
d[c("x", "z")]
d[d$x > 3, "y"] # 'd''e''f''g''h''i''j'
d[d$x > 3, "y", drop=FALSE] # keep the output as dataframe
d[2, ] # 2 b B



typeof(c("abc", "def")) 
## [1] "character"
typeof(1:10) 
## [1] "integer"
typeof(c(pi, exp(1))) 
## [1] "double"



f <- factor(c("a", "b", "a", "a", "c"))# categories
levels(f) 
## [1] "a" "b" "c"


library(stringr) # includes R package “stringr”
str_trim("  hello world ") # [1] "hello world"


gender <- c("M", "male ", "Female", "fem") # a char vector
grepl("[fF]", gender) # lower case of upper case “f” matches the elements of “gender”   
## [1] FALSE  FALSE  TRUE  TRUE


age <- c(23, 16, NA) # a vector
mean(age) 
## [1] NA
mean(age, na.rm = TRUE) # avoid NA
## [1] 19.5


is.finite(c(1, Inf, NaN, NA)) 
## [1]  TRUE FALSE FALSE FALSE


is.special <- function(x){if (is.numeric(x)) !is.finite(x) else is.na(x)}
person   # a data   set with columns “age” and “height”

sapply(person, is.special)
age height
## [1,] FALSE  FALSE
## [2,] FALSE  FALSE
## [3,] FALSE  FALSE
## [4,] FALSE  TRUE


