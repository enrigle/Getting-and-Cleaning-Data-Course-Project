download.file("https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda", destfile="E:/SystemFolders/Documents/samsungData.rda", method="auto")
load("E:/SystemFolders/Documents/samsungData.rda")

table(samsungData$activity)

#sujeto 1, average acceleration
par(mfrow=c(1,2))
numericActivity <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==1]
plot(samsungData[samsungData$subject==1,1],pch=19,col=numericActivity,ylab=names(samsungData)[1])
plot(samsungData[samsungData$subject==1,2],pch=19,col=numericActivity,ylab=names(samsungData)[1])
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)

#Plotting max acceleration for the first subject
par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==1,10],pch=19,col=numericActivity,ylab=names(samsungData)[10])
plot(samsungData[samsungData$subject==1,11],pch=19,col=numericActivity,ylab=names(samsungData)[11])

#Clustering based just on maxim acceleration
source("http://dl.dropbox.com/u/7710864/courseraPublic/myplclust.R")
distanceMatrix <- dist(samsungData[samsungData$subject==1,10:10])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)

#Single value descomposition
par(mfrow=c(1,2))
plot(svd1$u[,1],col=numericActivity,pch=19)
plot(svd1$u[,2],col=numericActivity,pch=19)
maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(samsungData[samsungData$subject==1,c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)
prediction
#know the data, per columns 
#Trained, validate, test.
head(samsungData[561:563])
set.seed(123)
trainset = samsungData[samsungData$subject == c(1,3,5,6),]
testset = samsungData[samsungData$subject == c(27,28,29,30),]
#Tree
#elimino los duplicados primeramente 
colnames(trainset)
trainset <- data.frame(trainset)
colnames(trainset)
#aplico el tree para el train set
samsungTreeTrain<-rpart(activity ~ ., data=trainset)
summary(samsungTreeTrain)
#creo el grafico de árbol
pdf(file="TrainTree.pdf",height=7,width=15)
plot(samsungTreeTrain,main="Train Tree");text(samsungTreeTrain)
dev.off()
#Build a tree

treetrain <- rpart(activity ~ tBodyAcc.std...X + tGravityAcc.mean...X + tGravityAcc.arCoeff...X.1 + tGravityAcc.max...Y + fBodyAcc.max...X + tBodyAccJerk.max...X + tBodyGyroMag.arCoeff..1,data=trainset)
#Plot errors
printcp(treetrain)
plotcp(treetrain)
# prune the tree
pruneTreeTrain <- prune(treetrain, cp=treetrain$cptable[which.min(treetrain$cptable[,"xerror"]),"CP"])
# plot the pruned tree
pdf(file="pruneTrainTree.pdf",height=8,width=15)
plot(pruneTreeTrain, uniform=TRUE, main="Pruned Classification Tree");text(pruneTreeTrain, use.n=TRUE, all=TRUE, cex=.8)
dev.off()
# predict
predict(pruneTreeTrain, type="class")
table(trainset$activity,predict(pruneTreeTrain, type="class"))
printcp(treetrain)
plotcp(treetrain)
plotcp(treetrain)
pruneTreeTrain <- prune(treetrain, cp=treetrain$cptable[which.min(treetrain$cptable[,"xerror"]),"CP"])
table(trainset$activity,predict(pruneTreeTrain, type="class"))
testset1 <- testset[1:328,]
bagTree <- bagging(as.factor(activity) ~.,data=trainset,coob=TRUE)
print(bagTree)
rForestTrain <- randomForest(as.factor(activity) ~ tBodyAcc.std...X + tBodyAccJerk.max...X+tGravityAcc.arCoeff...X.1+tGravityAcc.max...Y+tGravityAcc.mean...X+tGravityAcc.mean...Y+tGravityAcc.energy...X+angle.Y.gravityMean.+tGravityAcc.max...Y, data=trainset)
print(rForestTrain)
TrainSamsung.lm <- lm(as.numeric(as.factor(activity)) ~ tBodyAcc.std...X + tBodyAccJerk.max...X+tGravityAcc.arCoeff...X.1+tGravityAcc.max...Y+tGravityAcc.mean...X+tGravityAcc.mean...Y+tGravityAcc.energy...X+angle.Y.gravityMean.+tGravityAcc.max...Y, data=trainset)
trainsetbt <- cbind(trainset,resid=rstudent(TrainSamsung.lm),fit=fitted(TrainSamsung.lm))
