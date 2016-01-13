###################### ORIGINAL CODE
eval_popsize = function(pos, x, y){ 
  xIndex = length(which(x <= pos))
  return(y[xIndex])
}

BlX = c(1,3,4,4.5,6,7.5,8,9)
BlY = c(1,1,1,7,4,4,4,4)

GrX = c(1.5,3.5,5,5.5,6,7,8,10)
GrY = c(2,2,2,7,3,4,2,10)

out = NULL
absdifference = function(xpos){
  n=length(xpos)
  for (i in 1:n) {
    out[i] = abs( eval_popsize(xpos[i],BlX,BlY) - eval_popsize(xpos[i],GrX,GrY) )
  }
  return(out)
}

integrate(absdifference,2,8.5)

###################### NEW CODE
# set up functions
eval_popsize = function(pos, x, y){ 
  xIndex = length(which(x <= pos))
  return(y[xIndex])
}

out = NULL
absdifference = function(xpos,d1,d2,d3,d4){
  n=length(xpos)
  for (i in 1:n) {
    out[i] = abs( eval_popsize(xpos[i],d1,d2) - eval_popsize(xpos[i],d3,d4) )
  }
  return(out)
}

setwd("/home/alex/Desktop/Simulations/SplittingChromosomes/binarySplitData1Chromsome30Mbp/")
filelist = list.files(pattern = "*.txt")
filelist = filelist[-3] # remove the fullData file

fullData = read.table("fullData.psmcfa.psmc.plot.0.txt",header=FALSE)

Results = data.frame() # for storing results
# check errors for each one
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,20000,3000000, d1=fullData$V1, d2=fullData$V2, d3=data.infile$V1, d4=data.infile$V2, subdivisions=10000) # the 20000 and 300000 are the limits suggested by the paper
  int_and_info = c(integration_val$value,integration_val$abs.error)
  Results = rbind(Results,int_and_info)
}
colnames(Results) = c("Integral","Abs.Error")
rownames(Results) = c(filelist)

OrdResults = Results[order(Results$Integral),]

SplitOrder = c(1/2,1/4,1/8,1/32,1/16,1/64,1/256,1/128,1/1024,1/512)
IntVSplit.lm = lm(log(OrdResults$Integral) ~ log(SplitOrder))
summary(IntVSplit.lm)

plot(log(SplitOrder),log(OrdResults$Integral))
abline(IntVSplit.lm)

plot(fitted(IntVSplit.lm),residuals(IntVSplit.lm))
qqnorm(residuals(IntVSplit.lm))

###############################

Results = data.frame() # a matrix for storing results
data.infile = read.table("eighthData.psmcfa.psmc.plot.0.txt" ,header=FALSE)
integration_val = integrate(absdifference,20000,3000000, d1=fullData$V1, d2=fullData$V2, d3=data.infile$V1, d4=data.infile$V2, subdivisions=10000) # the 20000 and 300000 are the limits suggested by the paper
int_and_info = c(integration_val$value,integration_val$abs.error)
Results = rbind(Results,int_and_info)
Results
colnames(Results) = c("Integral","Abs.Error")
rownames(Results) = 
