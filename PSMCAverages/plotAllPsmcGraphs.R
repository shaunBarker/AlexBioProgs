setwd("/home/alex/Desktop/Analyses/151217a_FromShaun/ShaunSimulatedData/")

testData<-read.table("testPdf.0.txt",header=FALSE)
plot(testData$V1,testData$V2,"s",log="x",xaxt="n",lwd=1, ylim=c(0,2),xlab="Years ago",ylab= expression(paste("Effective population size x10"^"4")))
filelist = list.files(pattern = "*.txt")
n=length(filelist)
#cumulativeMean<-vector(length=19)
#cumulativePosition<-vector(length=19)
#dataPosition<-matrix(nrow=19)
#dataValue<-matrix(nrow=19)
dataPosition = NULL
dataValue = NULL



# FIX THIS
lines(testData$V1,testData$V2,"s", col="red")


library(matrixStats)
medianPopPos<-rowMedians(dataPosition,na.rm =TRUE )
medianPopEsts<-rowMedians(dataValue, na.rm=TRUE)
meanPopPos<-rowMeans(dataPosition,na.rm=TRUE)
meanPopEsts<-rowMeans(dataValue, na.rm=TRUE)
#lines(cumulativePosition,cumulativeMean,col="blue","s",lwd=5)
plot(testData$V1,testData$V2,"s",log="x",xaxt="n",col="red",lwd=3)
lines(medianPopPos,medianPopEsts,col="green","s",lwd=3) # off by a little bit
lines(meanPopPos,meanPopEsts,col="blue","s",lwd=3) # definitely not

### Alex adding things in...
# try harmonic mean
H_Mean_PopPos = apply(dataPosition, 1, function(z) 1/mean(1/z))
H_Mean_PopEsts = apply(dataValue, 1, function(z) 1/mean(1/z))
lines(H_Mean_PopPos,H_Mean_PopEsts,col="yellow","s",lwd=3) #  nope

#G_Mean_PopPos = apply(dataPosition, 1, function(z) prod(z)^(1/length(z)))
G_Mean_PopPos = NULL
cumulative = 1

for (i in 1:dim(dataPosition)[1]) { # rows
  for (j in 1:dim(dataPosition)[2]) { # columns 
    cumulative = cumulative * dataPosition[i,j]^(1/dim(dataPosition)[2]) # cumulatively find the geometric mean
  }
  G_Mean_PopPos[i] = cumulative # the geometric mean for row i is ...
  cumulative = 1 # reset
}

####
# test
x=matrix(c(1,3,4,5,7,9),ncol=3)
geom_mean_1 = ( prod(x[1,]) )^(1/length(x[1,]))
geom_mean_2 = ( prod(x[2,]) )^(1/length(x[2,]))

G_Mean_PopPos = NULL
cumulative = 1
for (i in 1:dim(x)[1]) { # rows
  for (j in 1:dim(x)[2]) { # columns 
    cumulative = cumulative * x[i,j]^(1/dim(x)[2]) # cumulatively find the geometric mean
  }
  G_Mean_PopPos[i] = cumulative # the geometric mean for row i is ...
  cumulative = 1 # reset
}


####

G_Mean_PopEsts = apply(dataValue, 1, function(z) prod(z)^(1/dim(dataPosition)[2]))
lines(G_Mean_PopPos,G_Mean_PopEsts,col="purple","s",lwd=3) #  nope


ticks <- seq(0, 6, by=1)
labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
axis(1, at=c(1, 10, 100, 1000,10000,100000,1000000), labels=labels)
