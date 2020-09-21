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
data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
Para <- read.xlsx("Ans1(table1).xlsx", rowNames = T)
set.seed(Seed)
L <- order(rnorm(length(trainlevel)))
train <- dplyr::filter(.data = data, Type == "Train" & Char %in% trainlevel[L[1:6]])
test <- dplyr::filter(.data = data, Type == "Train" & Char %in% trainlevel[L[7:12]])
train[,-1:-5] <- apply(train[,-1:-5],2,as.numeric )
test[,-1:-5] <- apply(test[,-1:-5],2,as.numeric )
RightM <- matrix(0,nrow = 5,ncol = 11)
rownames(RightM) <- S
colnames(RightM) <- 0:-10
Channel <- matrix(nrow = 5,ncol = 20)
rownames(Channel) <- S
temp <- colnames(data)[-1:-6]
for(i in seq(1,length(temp),28)){
  Channel[,i%/%28+1] <- paste0(temp[i:(i+27)],collapse = " + ")
}
ChannelTrace <- matrix(0,nrow = 5,ncol = 10)
rownames(ChannelTrace ) <- S
IsChannel <- matrix(T,nrow = 5,ncol = 20)
rownames(IsChannel) <- S
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
  }
  return(ans)
}
putRC <- function(R,C){
  return((R-1)*6+C-6)
}
for(i in S){
  j <- 0
  formula <- paste0("Y ~ ", paste0(Channel[i,],collapse = " + " ), collapse = " + ")
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
  R <- sum(temp5$Char==temp5$PreChar)
  N <- dim(temp5)[1]
  RightM[i,j+1] <- (R/N)
  j <- j + 1
  repeat{
    if(j>10) break
    for(k in 1:20){
      if(IsChannel[i,k] == F) next
      else{
        IsChannel[i,k] = FALSE
        formula <- paste0("Y ~ ", paste0(Channel[i,IsChannel[i,]],collapse = " + " ), collapse = " + ")
        IsChannel[i,k] = TRUE
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
        R <- sum(temp5$Char==temp5$PreChar)
        N <- dim(temp5)[1]
        print(paste0("R=",R,",N=",N,",R/N=",R/N))
        if((R/N)>=RightM[i,j] & ((RightM[i,j+1]==0) | (R/N)>RightM[i,j+1])){
          RightM[i,j+1] <- (R/N)
          ChannelTrace[i,j] <- k
        }
      }
    }
    if (RightM[i,j+1]!=0){
      IsChannel[i,ChannelTrace[i,j]] <- FALSE
      j <- j + 1
    }
    else break
  }
}
write.xlsx(RightM,"Ans2(table1)(1)",row.names = T, col.names = T)
write.xlsx(ChannelTrace,"Ans2(table1)(2).xlsx",row.names = T, col.names = T)
write.xlsx(IsChannel,"Ans2(table1)(3).xlsx",row.names = T, col.names = T)

# Start Predicting (Dispersed Case)

data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
Para <- openxlsx::read.xlsx("Ans1(table1).xlsx",rowNames = T )
C <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T",
       "U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","0")
Ans <- matrix(nrow = 6, ncol = 10)
rownames(Ans) <- c(S,"总计")
colnames(Ans) <- c(1:10)
IsChannel <- openxlsx::read.xlsx("Ans2(table1)(3).xlsx",rowNames = T, colNames = T)
IsChannel <- ifelse(IsChannel=="TRUE",TRUE,FALSE)
Channel <- matrix(nrow = 5,ncol = 20)
rownames(Channel) <- S
temp <- colnames(data)[-1:-6]
for(i in seq(1,length(temp),28)){
  Channel[,i%/%28+1] <- paste0(temp[i:(i+27)],collapse = " + ")
}
train <- dplyr::filter(.data = data, Type == "Train")
test <- dplyr::filter(.data = data, Type == "Test")
for(i in S){
  formula <- paste0("Y ~ ", paste0(Channel[i,IsChannel[i,]],collapse = " + " ), collapse = " + ")
  para <- Para[i,]
  trainT <- train[train[,"S"]==i,]
  testT <- test[test[,"S"]==i,]
  set.seed(Seed)
  fit <- nnet(
    as.formula(formula),
    data = trainT,
    size = 3,
    rang=as.numeric(para["rang"]),
    decay=as.numeric(para["decay"]),
    linout=F,
    maxit=10000,
    MaxNWts=10000)
  prey <- predict(fit,testT)
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
write.xlsx(data.frame(Ans),"Ans2(table2).xlsx",row.names = T, col.names = T)
data.frame(Ans)

# Start Predicting (Centralization Case)
data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
Para <- openxlsx::read.xlsx("Ans1(table1).xlsx",rowNames = T )
C <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T",
       "U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","0")
Channel <- matrix(nrow = 5,ncol = 20)
rownames(Channel) <- S
temp <- colnames(data)[-1:-6]
for(i in seq(1,length(temp),28)){
  Channel[,i%/%28+1] <- paste0(temp[i:(i+27)],collapse = " + ")
}
Ans <- matrix(nrow = 6, ncol = 10)
rownames(Ans) <- c(S,"总计")
colnames(Ans) <- c(1:10)
IsChannel <- openxlsx::read.xlsx("Ans2(table1)(3).xlsx",rowNames = T, colNames = T)
IsChannel <- ifelse(IsChannel=="TRUE",TRUE,FALSE)
IS <- apply(IsChannel,2,function(x){
  if(sum(x)<=2) FALSE else TRUE
})
train <- dplyr::filter(.data = data, Type == "Train")
test <- dplyr::filter(.data = data, Type == "Test")
for(i in S){
  formula <- paste0("Y ~ ", paste0(Channel[i,IS],collapse = " + " ), collapse = " + ")
  para <- Para[i,]
  trainT <- train[train[,"S"]==i,]
  testT <- test[test[,"S"]==i,]
  set.seed(Seed)
  fit <- nnet(
    as.formula(formula),
    data = trainT,
    size = 3,
    rang=as.numeric(para["rang"]),
    decay=as.numeric(para["decay"]),
    linout=F,
    maxit=10000,
    MaxNWts=10000)
  prey <- predict(fit,testT)
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
write.xlsx(data.frame(Ans),"Ans2(table3).xlsx",row.names = T, col.names = T)
data.frame(Ans)

