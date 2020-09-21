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
data <- openxlsx::read.xlsx("Data Use.xlsx",sheet = 1,colNames = T)
Seed = 1
Kfold <- 2
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
Seq <- c(0,0.2,0.4,0.6,0.8,1)
Accuracy <- matrix(nrow = length(Seq), ncol =length(Seq))
rownames(Accuracy) <- Seq
colnames(Accuracy) <- Seq
# Search Parameters
for(rang  in 1:length(Seq)){
  for(decay in 1:length(Seq)){
    R <- 0
    N <- 0
    for(i in S){
      for(j in 1:1){
        trainT <- dplyr::filter(train[[j]],S==i)
        testT <- dplyr::filter(test[[j]],S==i)
        set.seed(Seed)
        fit <- nnet(
          as.formula(formula),
          data=trainT,
          size=3,
          rang=Seq[rang],
          decay = Seq[decay],
          linout=F,
          maxit=10000,
          MaxNWts=10000)
        prey <- predict(fit,testT)
        temp4 <- cbind(testT,prey)
        temp4 <- getCharR(temp4)
        R <- R + sum(temp4$Char==temp4$PreChar)
        N <- N + dim(temp4)[1]
      }
    }
    Accuracy[rang,decay] <- R/N
  }
}
Accuracy
plot(x = Seq, y = Accuracy[1,],type = "l",col = 2,xaxt = "n")
axis(1, at =Seq, labels = Seq) 
lines(x = Seq, y = Accuracy[2,],type = "l", col = 3)
lines(x = Seq, y = Accuracy[3,],type = "l", col = 4)
lines(x = Seq, y = Accuracy[4,],type = "l", col = 5)
lines(x = Seq, y = Accuracy[5,],type = "l", col = 6)
write.xlsx(Accuracy,"Sensibility.xlsx",row.names = T, col.names = T)
