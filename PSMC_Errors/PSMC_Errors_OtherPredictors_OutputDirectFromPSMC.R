######################### MAX OF MINS + MIN OF MAXS + LOG TIME VERSION 


# 30 Mbp


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

#############################################33
################################3

# the 30 Mbp test
setwd("/home/alex/Desktop/Simulations/SplittingChromosomes/binarySplitData1Chromosome30Mbp/extractPSMCoutput/")
filelist30 = list.files(pattern = "*.txt")
# filelist = filelist[-3] # remove the fullData file

fullData30 = read.table("fullData.psmcfa.psmc.txt",header=FALSE)

logmins = rep(0,length(filelist30))
logmaxs = rep(0,length(filelist30))

i = 1
# find the max of mins > 20kyr
for (infile in filelist30) {
  data.infile = read.table(infile,header=FALSE)
  pos_min = min(which(data.infile$t_k > 20000))
  pos_max = max(which(data.infile$t_k < 3000000))
  logmins[i] = log(data.infile$t_k[pos_min])
  logmaxs[i] = log(data.infile$t_k[pos_max])
  
  i = i+1
}
logmax_of_mins = max(logmins)
logmin_of_maxs = min(logmaxs)

logResults = data.frame() # for storing results

for (infile in filelist30) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,logmax_of_mins,logmin_of_maxs, d1=log(fullData30$V1), d2=fullData30$V2, d3=log(data.infile$V1), d4=data.infile$V2, subdivisions=10000) 
  int_and_info = c(integration_val$value,integration_val$abs.error)
  logResults = rbind(logResults,int_and_info)
}
colnames(logResults) = c("Integral","Abs.Error")
rownames(logResults) = c(filelist30)

logOrdResults_MaxOfMins_MinOfMaxs = logResults[order(logResults$Integral),]
Results30 = logOrdResults_MaxOfMins_MinOfMaxs
Results30

powerOrder = c(0,1,3,2,5,4,6,8,7,9,10) # take from logOrdResults...
logSplitOrder = unlist(lapply(powerOrder, function(x){1/2^x}))

#logSplitOrder2 = c(1,1/2,1/8,1/4,1/32,1/16,1/64,1/256,1/128,1/512,1/1024)

# is the split order important, or is it the number of bases in the chunks?
#powers = c(0:10)
#numbers = lapply(powers, function(x){1/2^x})

num_bases_in_order30 = unlist(lapply(logSplitOrder, function(x){ceiling(30*10^6*x)})) # note: this ceiling command is technically not quite correct. For example, if the sequence was AAACCCG, the first split would be into AAAC CCG, while my command is assuming a split into 2*4 base long sequences. But the numbers are so large it won't really matter.
plot(num_bases_in_order30,Results30$Integral)

# plot(logSplitOrder,logOrdResults_MaxOfMins_MinOfMaxs$Integral)

################################################################################ THE 20Mbp TEST
###############################################################################################
# the 20 Mbp test
setwd("/home/alex/Desktop/Simulations/SplittingChromosomes/binarySplitData1Chromosome20Mbp/")
filelist20 = list.files(pattern = "*.txt")
# filelist = filelist[-3] # remove the fullData file

fullData20 = read.table("20Mbp_run.ms.psmcfa.psmc.txt",header=FALSE)

logmins = rep(0,length(filelist20))
logmaxs = rep(0,length(filelist20))

i = 1
# find the max of mins > 20kyr
for (infile in filelist20) {
  data.infile = read.table(infile,header=TRUE)
  pos_min = min(which(data.infile$V1 > 20000))
  pos_max = max(which(data.infile$V1 < 3000000))
  logmins[i] = log(data.infile$V1[pos_min])
  logmaxs[i] = log(data.infile$V1[pos_max])
  
  i = i+1
}
logmax_of_mins = max(logmins)
logmin_of_maxs = min(logmaxs)

logResults = data.frame() # for storing results

for (infile in filelist30) {
  data.infile = read.table(infile,header=FALSE)
  integration_val = integrate(absdifference,logmax_of_mins,logmin_of_maxs, d1=log(fullData20$V1), d2=fullData20$V2, d3=log(data.infile$V1), d4=data.infile$V2, subdivisions=10000) 
  int_and_info = c(integration_val$value,integration_val$abs.error)
  logResults = rbind(logResults,int_and_info)
}
colnames(logResults) = c("Integral","Abs.Error")
rownames(logResults) = c(filelist20)

logOrdResults_MaxOfMins_MinOfMaxs = logResults[order(logResults$Integral),]
Results20 = logOrdResults_MaxOfMins_MinOfMaxs
Results20

powerOrder = c(0,1,3,2,5,4,6,8,7,9,10) # take from logOrdResults...
logSplitOrder = unlist(lapply(powerOrder, function(x){1/2^x}))

#logSplitOrder2 = c(1,1/2,1/8,1/4,1/32,1/16,1/64,1/256,1/128,1/512,1/1024)

# is the split order important, or is it the number of bases in the chunks?
#powers = c(0:10)
#numbers = lapply(powers, function(x){1/2^x})

num_bases_in_order30 = unlist(lapply(logSplitOrder, function(x){ceiling(30*10^6*x)})) # note: this ceiling command is technically not quite correct. For example, if the sequence was AAACCCG, the first split would be into AAAC CCG, while my command is assuming a split into 2*4 base long sequences. But the numbers are so large it won't really matter.
plot(num_bases_in_order30,Results30$Integral)