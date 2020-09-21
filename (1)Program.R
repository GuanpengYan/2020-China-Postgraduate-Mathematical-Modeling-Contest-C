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
Seed = 1
Kfold <- 6
getCharR <- function(ans){
  for(q in seq(1,dim(ans)[1],60)){
    print(q)
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
trainlevel <- c(2,4,7,12,15,17,19,22,26,30,33,35)
S <- paste("S",1:5, sep = "")
C <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T",
       "U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","0")

data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
train <- list()
test <- list()
set.seed(Seed)
L <- order(rnorm(length(trainlevel)))
for(i in seq(1,length(L),round(length(L)/Kfold))){
  iT <- L[i:min((i+1),length(L))]
  temp <- filter(.data = data, Type == "Train", Char %in% trainlevel[-iT])
  train <- c(train,list(temp))
  temp <- filter(.data = data, Type == "Train", Char %in% trainlevel[iT])
  test <- c(test,list(temp))
}
formula <- 
  as.formula(paste0("Y~",paste0(colnames(data)[-1:-6],collapse = "+"),collapse = ""))
Para <- matrix(0,nrow = 5,ncol = 5)
rownames(Para) <- S
colnames(Para) <- c("rang","decay","R","N","accuracy")
# Search Parameters
for(i in S){
  trainT <- train
  testT <- test
  for(j in 1:6){
    trainT[[j]] <- dplyr::filter(trainT[[j]],S==i)
    testT[[j]] <- dplyr::filter(test[[j]],S==i)
  }
  for(rang  in seq(0,1,0.2)){
    for(decay in seq(0,1,0.1)){
      R <- 0
      N <- 0
      for(j in 1:6){
        set.seed(Seed)
        fit <- nnet(as.formula(formula) ,data=trainT[[j]],size=3,rang=rang,decay = decay,linout=F,maxit=10000,MaxNWts=10000)
        prey <- predict(fit,testT[[j]])
        temp4 <- cbind(testT[[j]],prey)
        temp4 <- getCharR(temp4)
        R <- R + sum(temp4$Char==temp4$PreChar)
        N <- N + dim(temp4)[1]
      }
      if((R/N)>Para[i,"accuracy"]){
        Para[i,"rang"] <- rang
        Para[i,"decay"] <- decay
        Para[i,"R"] <- R
        Para[i,"N"] <- N
        Para[i,"accuracy"] <- R/N
      }
      if((R/N)==1) break
    }
    if((R/N)==1) break
  }
}
write.xlsx(Para,"Ans1(table1).xlsx",row.names = T, col.names = T)
# Predict
data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
Para <- openxlsx::read.xlsx("Ans1(table1).xlsx",rowNames = T )
Ans <- matrix(nrow = 6, ncol = 11)
rownames(Ans) <- c(S,"总计")
colnames(Ans) <- c(1:10,"权重")
for(i in S){
  train <- filter(.data = data, Type == "Train", S == i)
  test <- filter(.data = data, Type == "Test", S == i)
  para <- Para[i,]
  set.seed(Seed)
  fit <- nnet(
    as.formula(formula),
    data=train,
    size=3,
    rang=as.numeric(para["rang"]),
    decay=as.numeric(para["decay"]),
    linout=F,
    maxit=10000,
    MaxNWts=10000)
  prey <- predict(fit,test)
  ans <- cbind(test,prey)
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
Ans[1:5,11] <- Para[,"accuracy"]/sum(Para[,"accuracy"])
Ans[6,11] <- sum(as.numeric(Ans[1:5,12]))
write.xlsx(data.frame(Ans),"Ans1(table2).xlsx",row.names = T, col.names = T)
