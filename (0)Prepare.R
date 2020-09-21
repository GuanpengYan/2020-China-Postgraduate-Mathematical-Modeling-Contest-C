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
trainlevel <- c(2,4,7,12,15,17,19,22,26,30,33,35)
testlevel <- c(13,6,25,28,9,-1,-2,-3,-4)
C <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T",
       "U","V","W","X","Y","Z","1","2","3","4","5","6","7","8","9","0")
S <- paste("S",1:5, sep = "")
result <- NULL
result0 <- NULL
result2 <- NULL
colStandardization <- function(data){
  result <- apply(data, 2, function(x){
    max <- max(x)
    min <- min(x)
    (x-min)/(max-min)
  })
  return(result)
}
getRC <- function(tab,i){
  if(i<=0) return(c(0,0))
  else{
    row <- (tab[i] - 1)%/%6+1
    col <- (tab[i] - 1)%%6 +7
  }
  return(c(row,col))
}

start <- 10
end <- 149
by <- 1
result <- NULL
result2 <- NULL
# Get Train Data
for(k in 1:5){
  for(i in 1:length(trainlevel)){
    print(i)
    data <- read.xlsx(paste0(S[k],"/",S[k],"_train_data.xlsx"),sheet = i,colNames = F)
    data <- apply(data,2,function(x) x)
    data <- data.frame(apply(data,2,function(x) x))
    id <- as.numeric(rownames(data))
    data <- cbind(id,data)
    head(data)
    event <- data.frame(read.xlsx(paste0(S[k],"/",S[k],"_train_event.xlsx"),sheet = i,colNames = F))
    head(event)
    colnames(event) <- c("Flash", "x")
    head(event)
    event <- dplyr::filter(.data = event, Flash<100)
    temp <- NULL
    for(j in 1:dim(event)[1]){
      temp2 <- merge(event[j,2],event[j,2]+seq(start,end,by))
      temp2$Time <- 1:length(seq(start,end,by))
      colnames(temp2)[2] <- "id"
      temp3 <- merge(temp2,data)
      head(temp3)
      for(l in paste("X",1:20,sep = "")){
        loess.fit <- loess(as.formula(paste(l,"~id",sep= "")),data=temp3,span=0.3)
        temp3[,l] <- predict(loess.fit)
      }
      temp <- rbind(temp, temp3)
    }
    colnames(temp)
    temp[,paste("X",1:20,sep = "")] <- 
      colStandardization(temp[,paste("X",1:20,sep = "")] )
    event$Group <- sort(rep(1:5,12))
    temp <- merge(event,temp)
    data2 <- temp
    head(data2)
    RC <- getRC(trainlevel,i)
    data2$Y <- apply(data2["Flash"],1,function(x){
      if(x==RC[1]||x==RC[2]) 1 else 0
    })
    head(data2)
    data2$Char <- trainlevel[i]
    data2$S <- S[k]
    temp3 <- melt(data2,measure = paste0("X",1:20,""))
    temp3 <- temp3[,c(7,8,3,2,6,5,9,10)]
    head(temp3)
    formula <- paste0("Char+S+Group+Flash+Y","~","variable+Time",collapse = "+")
    temp4 <- dcast(temp3,as.formula(formula))
    head(temp4)
    result <- rbind(result,temp4)
  }
}
# Get Test Data
for(k in 1:5){
  if(k %in% c(1,4,5)) testlevel <- c(testlevel,0)
  for(i in 1:length(testlevel)){
    print(i)
    data <- read.xlsx(paste0(S[k],"/",S[k],"_test_data.xlsx"),sheet = i,colNames = F)
    data <- apply(data,2,function(x) x)
    data <- data.frame(apply(data,2,function(x) x))
    id <- as.numeric(rownames(data))
    data <- cbind(id,data)
    head(data)
    event <- data.frame(read.xlsx(paste0(S[k],"/",S[k],"_test_event.xlsx"),sheet = i,colNames = F))
    head(event)
    colnames(event) <- c("Flash", "x")
    head(event)
    event <- dplyr::filter(.data = event, Flash<100)
    temp <- NULL
    for(j in 1:dim(event)[1]){
      temp2 <- merge(event[j,2],event[j,2]+seq(start,end,by))
      temp2$Time <- 1:length(seq(start,end,by))
      colnames(temp2)[2] <- "id"
      temp3 <- merge(temp2,data)
      head(temp3)
      for(l in paste("X",1:20,sep = "")){
        loess.fit <- loess(as.formula(paste(l,"~id",sep= "")),data=temp3,span=0.4)
        temp3[,l] <- predict(loess.fit)
      }
      temp <- rbind(temp, temp3)
    }
    colnames(temp)
    temp[,paste("X",1:20,sep = "")] <- 
      colStandardization(temp[,paste("X",1:20,sep = "")] )
    event$Group <- sort(rep(1:5,12))
    temp <- merge(event,temp)
    data2 <- temp
    head(data2)
    RC <- getRC(testlevel,i)
    data2$Y <- apply(data2["Flash"],1,function(x){
      if(x==RC[1]||x==RC[2]) 1 else 0
    })
    head(data2)
    data2$Char <- testlevel[i]
    data2$S <- S[k]
    temp3 <- melt(data2,measure = paste0("X",1:20,""))
    temp3 <- temp3[,c(7,8,3,2,6,5,9,10)]
    head(temp3)
    formula <- paste0("Char+S+Group+Flash+Y","~","variable+Time",collapse = "+")
    temp4 <- dcast(temp3,as.formula(formula))
    head(temp4)
    result2 <- rbind(result2,temp4)
  }
  if(k %in% c(1,4,5)) testlevel <- testlevel[1:9]
}
# Combine and Output
data4 <- result
train <- data4[,c(1:5,seq(6,dim(data4)[2],5))]
data5 <- result2
test <- data5[,c(1:5,seq(6,dim(data4)[2],5))]
train$Type <- "Train"
test$Type <- "Test"
write.xlsx(rbind(train,test)[,c("Type",colnames(train)[1:length(colnames(train))-1])],"Data Use.xlsx", row=F, col =T)
