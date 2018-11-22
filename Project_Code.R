###########
#
# Machine Learning Project
#
###########


Read in the data.
```{r}

library(data.table)
library(neuralnet)
library(caTools)
library(plyr)
library(boot)
setwd("/Users/matttrombley/Desktop/Machine Learning/Project/")
load("MLProjectData.Rdata")
ML <- MLProjectData
rm(MLProjectData)

```

# Re-code dataset with numerical variables to work for analysis
```{r}

for (i in 1:nrow(ML)) {
   for (j in 62:85) {
      if (ML[i,j] == TRUE) {
         ML[i,j] <- 1
      } else {
         ML[i,j] <- 0
      }
   }
}
new <- matrix(0,nrow(ML),17)
ML <- cbind(ML,new)
for (i in 1:nrow(ML)) {
   if (ML[i,60] == "A") {
      ML[i,87] <- 1
   } else if (ML[i,60] == "B") {
      ML[i,88] <- 1
   } else if (ML[i,60] == "C") {
      ML[i,89] <- 1
   } else if (ML[i,60] == "D") {
      ML[i,90] <- 1
   } else if (ML[i,60] == "E") {
      ML[i,91] <- 1
   }
}
for (i in 1:nrow(ML)) {
   if (ML[i,61] == "A") {
      ML[i,92] <- 1
   } else if (ML[i,61] == "B") {
      ML[i,93] <- 1
   } else if (ML[i,61] == "C") {
      ML[i,94] <- 1
   } else if (ML[i,61] == "D") {
      ML[i,95] <- 1
   } else if (ML[i,61] == "E") {
      ML[i,96] <- 1
   } else if (ML[i,61] == "F") {
      ML[i,97] <- 1
   } else if (ML[i,61] == "G") {
      ML[i,98] <- 1
   } else if (ML[i,61] == "H") {
      ML[i,99] <- 1
   } else if (ML[i,61] == "I") {
      ML[i,100] <- 1
   } else if (ML[i,61] == "J") {
      ML[i,101] <- 1
   } else if (ML[i,61] == "K") {
      ML[i,102] <- 1
   } else if (ML[i,61] == "L") {
      ML[i,103] <- 1
   }
}
ML <- ML[,c(1:59,87:103,62:86)]
colnames(ML)[60:76] <- c("cat1A","cat1B","cat1C","cat1D","cat1E","cat2A","cat2B","cat2C","cat2D","cat2E","cat2F","cat2G","cat2H","cat2I","cat2J","cat2K","cat2L")
rm(new)

```

Applying neural networks to the data.
```{r}

set.seed(12345)
scaleRange <- function(x) {
   return((x-min(x))/(max(x)-min(x)))
}
deScale <- function(x,y) {
   return(x*(max(y)-min(y)) + min(y))
}
split <- sample.split(ML, SplitRatio = 0.8)
train_NN <- ML[split,]
valid_NN <- ML[!split,]
ML_norm <- as.data.frame(lapply(ML, scaleRange))
train_norm_NN <- ML_norm[split,]
valid_norm_NN <- ML_norm[!split,]
n <- names(ML)
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))
nnet1 <- neuralnet(f, train_norm_NN, hidden=1, linear.output=TRUE)
#plot(nnet1)
nnet1Pred <- compute(nnet1, valid_norm_NN[,1:100])
nnet1Pred <- deScale(nnet1Pred$net.result, ML$target)

plot(valid_NN$target, nnet1Pred, col='blue', pch=16)
abline(0,1)

#nnet1Rsq = cor(nnet1PredRescale,valid_NN$target)
#nnet1MAPE = mean((nnet1PredRescale-valid_NN$target)/valid_NN$target)
#cat("1 Hidden Unit R-squared after Rescaling:", nnet1Rsq)
#cat("1 Hidden Unit MAPE after Rescaling:", nnet1MAPE)

```
 
Cross-validating the neural net.
```{r}

nlayer <- 2
cv.error <- NULL
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
    index <- sample(1:nrow(ML),round(0.9*nrow(ML)))
    train.cv <- ML_norm[index,]
    test.cv <- ML_norm[-index,]
    nn <- neuralnet(f,data=train.cv,hidden=nlayer,linear.output=T)
    pr.nn <- compute(nn,test.cv[,1:100])
    pr.nn <- deScale(pr.nn$net.result, ML$target)
    test.cv.r <- deScale(test.cv$target, ML$target)
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    pbar$step()
}

cv.error <- cv.error[1:k]
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

```



Write out resulting file.
```{r}

fwrite("Orange7.csv")

```
