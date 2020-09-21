library(openxlsx)
library(reshape2)
library(e1071)
library(sampling)
library(randomForest)
library(dplyr)
library(neuralnet)
library(nnet)
library(mFilter)
library(pspline)
## Load
Seed <- 1
S <- paste("S",1:5, sep = "")
trainlevel <- c(2,4,7,12,15,17,19,22,26,30,33,35)
C <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T",
       "U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","0")
data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
Para <- openxlsx::read.xlsx("Ans1(table1).xlsx", rowNames = T)
IsChannel <- openxlsx::read.xlsx("Ans2(table1)(3).xlsx",rowNames = T, colNames = T)
IsChannel <- ifelse(IsChannel=="TRUE",TRUE,FALSE)
IS <- apply(IsChannel,2,function(x){
  if(sum(x)<=2) FALSE else TRUE
})
set.seed(Seed)
L <- order(rnorm(length(trainlevel)))
train <- dplyr::filter(.data = data, Type == "Train" & Char %in% trainlevel[L[1:6]])
test <- dplyr::filter(.data = data, Type == "Train" & Char %in% trainlevel[L[7:12]])
train[,-1:-5] <- apply(train[,-1:-5],2,as.numeric )
test[,-1:-5] <- apply(test[,-1:-5],2,as.numeric )
RightM <- matrix(0,nrow = 5,ncol = 11)
rownames(RightM) <- S
colnames(RightM) <- 0:-10
Channel <- rep(NA,20)
temp <- colnames(data)[-1:-6]
for(i in seq(1,length(temp),28)){
  Channel[i%/%28+1] <- paste0(temp[i:(i+27)],collapse = " + ")
}
formula <- paste0("Y ~ ", paste0(Channel[IS],collapse = " + " ), collapse = " + ")
Ans <- matrix(nrow = 6, ncol = 10)
rownames(Ans) <- c(S,"总计")
colnames(Ans) <- c(1:10)
getCharR <- function(ans){
  for(q in seq(1,dim(ans)[1],60)){
    temp <- ans[q:(q+59),]
    t <- c(NA,NA)
    rowM <- temp[temp$Flash<=6,c("S","Flash","prey")]
    colM <- temp[temp$Flash>=7,c("S","Flash","prey")]
    rowS <- dcast(rowM, S ~ Flash, function(x) sum(x))[,-1]
    colS <- dcast(colM, S ~ Flash, function(x) sum(x))[,-1]
    t[1] <- names(which.max(rowS))[1]
    t[2] <- names(which.max(colS))[1]
    t <- as.numeric(t)
    ans[q:(q+59),"PreChar"] <- putRC(t[1],t[2])
    ans[q:(q+59),"Y"] <- ifelse(rep(1:12,5) %in% t,1,0)
  }
  return(ans)
}
putRC <- function(R,C){
  return((R-1)*6+C-6)
}
for(i in S){
  para <- Para[i,]
  trainT <- train[train[,"S"]==i,]
  testT <- test[test[,"S"]==i,]
  set.seed(Seed)
  fit <- nnet(
    as.formula(formula),
    data = train,
    size = 3,
    rang=as.numeric(para["rang"]),
    decay=as.numeric(para["decay"]),
    linout=F,
    maxit=10000,
    MaxNWts=10000)
  prey <- predict(fit,testT)
  temp4 <- cbind(testT,prey)
  temp5 <- getCharR(temp4)
  test[test[,"S"]==i,]$Char <- temp5$PreChar
  test[test[,"S"]==i,]$Y <- temp5$Y
}
train <- rbind(train,test)
test <- dplyr::filter(.data = data, Type == "Test")
for(i in S){
  para <- Para[i,]
  trainT <- train[train[,"S"]==i,]
  testT <- test[test[,"S"]==i,]
  set.seed(Seed)
  fit <- nnet(
    as.formula(formula),
    data = train,
    size = 3,
    rang=as.numeric(para["rang"]),
    decay=as.numeric(para["decay"]),
    linout=F,
    maxit=10000,
    MaxNWts=10000)
  prey <- predict(fit,testT)
  temp4 <- cbind(testT,prey)
  ans <- cbind(testT,prey)
  for(q in seq(1,dim(ans)[1],60)){
    print(q)
    temp <- ans[q:(q+59),]
    ############ Case 2
    t <- c(NA,NA)
    rowM <- temp[temp$Flash<=6,c("S","Flash","prey")]
    colM <- temp[temp$Flash>=7,c("S","Flash","prey")]
    rowS <- dcast(rowM, S ~ Flash, function(x) sum(x))[,-1]
    colS <- dcast(colM, S ~ Flash, function(x) sum(x))[,-1]
    t[1] <- names(which.max(rowS))[1]
    t[2] <- names(which.max(colS))[1]
    t <- as.numeric(t)
    ans[q:(q+59),"PreChar"] <- C[putRC(t[1],t[2])]
  }
  for(q in seq(1,dim(ans)[1],60)){
    Ans[i,(q%/%60)+1] <- ans[q,"PreChar"]
  }
}
write.xlsx(Ans,"Ans3(table1).xlsx",row.names = T, col.names = T)
