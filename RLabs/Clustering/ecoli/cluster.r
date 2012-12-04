getColor <<-function(n)
#function to color each leaf node's label so we can see any oddities in the dendrogram
{
	if(is.leaf(n))		#check to see if n is a leaf node
	{
	a<-attributes(n)	#store n's attributes in a

	for(j in 1:length(uniqueClasses))				#loop over all possible classifications
	{
		label <- attr(n, "label")					#store n's label
		if(labelData[label,] == uniqueClasses[j])	#if the label = the current classification
		{
			i <- j									#set i = to j
		}
	}
		attr(n, "nodePar") <-
			c(a$nodePar, list(lab.col =myCols[i]))  #color the label based on i's value
	}
	n	#return the node
}


indata <- read.csv("newEcoli.csv", header=FALSE) 	#read the input file into indata
uniqueClasses <- unique(indata[,1])				#find all unique classifications
labelData <- indata[1]							#store a mapping between instances and classifications
indata[1] <- NULL								#remove the column containing classifications
d <- dist(indata)								#find the distance matrix for indata
h <- hclust(d)									#cluster based on the distance matrix

dend <- as.dendrogram(h)						#store the results as a dendrogram

numGroups <- length(uniqueClasses)				#find how many classifications there are
myCols <-grDevices::rainbow(numGroups)			#generate a color pallette containing that many colors
i <- 0											#set i to 0 by default
dend <- dendrapply(dend, getColor)				#use dendrapply to color all leaf nodes' labels
plot(dend)										#plot the result
pdf("graph.pdf")
plot(dend)
dev.off()

