######################### MAX OF MINS VERSION

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
# filelist = filelist[-3] # remove the fullData file

fullData = read.table("fullData.psmcfa.psmc.plot.0.txt",header=FALSE)

mins = rep(0,length(filelist))
i = 1
# find the max of mins > 20kyr
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  pos = min(which(data.infile$V1 > 20000))
  mins[i] = data.infile$V1[pos]
  i = i+1
}
max_of_mins = max(mins)

Results = data.frame() # for storing results
# check errors for each one
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,max_of_mins,3000000, d1=fullData$V1, d2=fullData$V2, d3=data.infile$V1, d4=data.infile$V2, subdivisions=10000) # the 20000 and 300000 are the limits suggested by the paper
  int_and_info = c(integration_val$value,integration_val$abs.error)
  Results = rbind(Results,int_and_info)
}
colnames(Results) = c("Integral","Abs.Error")
rownames(Results) = c(filelist)

OrdResults_MaxOfMins = Results[order(Results$Integral),]
OrdResults_MaxOfMins
SplitOrder = c(1,1/2,1/4,1/8,1/32,1/16,1/64,1/256,1/128,1/1024,1/512)
plot(SplitOrder,OrdResults_MaxOfMins$Integral)


IntVSplit.lm = lm(log(OrdResults$Integral) ~ log(SplitOrder))
summary(IntVSplit.lm)

plot(log(SplitOrder),log(OrdResults$Integral))
abline(IntVSplit.lm)

plot(fitted(IntVSplit.lm),residuals(IntVSplit.lm))
qqnorm(residuals(IntVSplit.lm))

######################### MAX OF MINS + LOG TIME VERSION
logmins = rep(0,length(filelist))
i = 1
# find the max of mins > 20kyr
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  pos = min(which(data.infile$V1 > 20000))
  logmins[i] = log(data.infile$V1[pos])
  i = i+1
}
logmax_of_mins = max(logmins)

logResults = data.frame() # for storing results

for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,logmax_of_mins,log(3000000), d1=log(fullData$V1), d2=fullData$V2, d3=log(data.infile$V1), d4=data.infile$V2, subdivisions=10000) # the 20000 and 300000 are the limits suggested by the paper
  int_and_info = c(integration_val$value,integration_val$abs.error)
  logResults = rbind(logResults,int_and_info)
}
colnames(logResults) = c("Integral","Abs.Error")
rownames(logResults) = c(filelist)

logOrdResults_MaxOfMins = logResults[order(logResults$Integral),]
logOrdResults_MaxOfMins
logSplitOrder = c(1,1/2,1/4,1/8,1/32,1/16,1/64,1/256,1/128,1/1024,1/512)
plot(logSplitOrder,logOrdResults_MaxOfMins$Integral)

######################### MAX OF MINS + MIN of MAXS VERSION

mins = rep(0,length(filelist))
maxs = rep(0,length(filelist))
i = 1
# find the max of mins > 20kyr, min of maxs > 30 Myr
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  pos_min = min(which(data.infile$V1 > 20000))
  pos_max = max(which(data.infile$V1 < 3000000))
  mins[i] = data.infile$V1[pos_min]
  maxs[i] = data.infile$V1[pos_max]
  i = i+1
}
max_of_mins = max(mins)
min_of_maxs = min(maxs)

Results = data.frame() # for storing results
# check errors for each one
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,max_of_mins,min_of_maxs, d1=fullData$V1, d2=fullData$V2, d3=data.infile$V1, d4=data.infile$V2, subdivisions=10000) 
  int_and_info = c(integration_val$value,integration_val$abs.error)
  Results = rbind(Results,int_and_info)
}
colnames(Results) = c("Integral","Abs.Error")
rownames(Results) = c(filelist)

OrdResults_MaxOfMins_MaxofMins = Results[order(Results$Integral),]
OrdResults_MaxOfMins_MaxofMins
SplitOrder = c(1,1/2,1/4,1/8,1/32,1/16,1/64,1/256,1/128,1/1024,1/512)
plot(SplitOrder,OrdResults$Integral)

######################### MAX OF MINS + MIN OF MAXS + LOG TIME VERSION
logmins = rep(0,length(filelist))
logmaxs = rep(0,length(filelist))

i = 1
# find the max of mins > 20kyr
for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  pos_min = min(which(data.infile$V1 > 20000))
  pos_max = max(which(data.infile$V1 < 3000000))
  logmins[i] = log(data.infile$V1[pos_min])
  logmaxs[i] = log(data.infile$V1[pos_max])
  
  i = i+1
}
logmax_of_mins = max(logmins)
logmin_of_maxs = min(logmaxs)

logResults = data.frame() # for storing results

for (infile in filelist) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,logmax_of_mins,logmin_of_maxs, d1=log(fullData$V1), d2=fullData$V2, d3=log(data.infile$V1), d4=data.infile$V2, subdivisions=10000) # the 20000 and 300000 are the limits suggested by the paper
  int_and_info = c(integration_val$value,integration_val$abs.error)
  logResults = rbind(logResults,int_and_info)
}
colnames(logResults) = c("Integral","Abs.Error")
rownames(logResults) = c(filelist)

logOrdResults_MaxOfMins_MinOfMaxs = logResults[order(logResults$Integral),]
logOrdResults_MaxOfMins_MinOfMaxs
logSplitOrder = c(1,1/2,1/8,1/4,1/32,1/16,1/64,1/256,1/128,1/512,1/1024)
plot(logSplitOrder,logOrdResults_MaxOfMins_MinOfMaxs$Integral)
