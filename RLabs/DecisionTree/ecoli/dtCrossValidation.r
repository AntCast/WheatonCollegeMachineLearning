library(rpart)										#include the rpart library
set.seed(as.integer(Sys.time()))					#set the seed for a random generator using the current time
indata <- read.csv("newEcoli.csv", header=FALSE)		#read the input file into indata
rand <- sample(nrow(indata))						#generate a random order for indata's rows
indata <- indata[rand,]								#scramble indata based on this random order
classData <- indata[1]								#store a mapping between classifications and original line numbers or row names(classData is a data frame)
minimumSplit <- 10									#set the minSplit to be 10
minimumBucket <- minimumSplit/3						#minBucket will be 3
k <- 10												#use 10 folds for validation
numElems <- nrow(indata)/k							#find the size of each fold
valid <- 0											#initialize valid to be 0
for(i in 1:k)	#loop once for each fold
{
	testData <- indata[(i*numElems-numElems+1):(i*numElems), ]	#pull out the test fold
	modelData <- indata[1:(i*numElems-numElems+1) , ]			#pull out everything before the test data for the model
	temp <- indata[(i*numElems):nrow(indata), ]					#pull out everything after the test data
	modelData <- rbind(modelData, temp)							#and put it in the model

	#create the model using modelData
	fit <- rpart(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8, method="class", data=modelData, control=rpart.control(minsplit = minimumSplit, minBucket = minimumBucket) )
	plot(fit, uniform=TRUE, main="CART for Ecoli Data")			#plot the model
	text(fit, use.n=TRUE, all=TRUE, cex=.8)						#make it look a bit nicer
	post(fit, file="tree.ps", title="CART for Ecoli Data")		#output a file of the plot

	preds <- predict(fit, testData)								#predict classifications for the test data
	predFrame <- as.data.frame(preds)							#store it in a data frame
	predCols <- colnames(predFrame)								#get the possible predicted values
	corCount <- 0												#initialize the number correct to 0
	for(j in 1:nrow(predFrame))									#loop over each prediction
	{
		rowNum <- row.names(predFrame[j, ])				#find the row name
		maxCol <- which.max(predFrame[j,])				#find which value the instance was predicted to be
		if(classData[rowNum,] == predCols[maxCol])		#if the predicted value matches the actual classification
		{
			corCount <- corCount + 1					#update the correct count
		}
	}
	corCount <- corCount / numElems						#calculate the average correct for this fold
	valid <- valid + corCount							#add it to valid
}

valid <- valid / k			#claculate the average correctness of all folds
print(valid)				#print it out
