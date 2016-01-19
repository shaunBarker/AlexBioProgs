# Error analysis based upon PSMC_Errors_OtherPredictors_OutputDirectFromPSMC.R

# set up functions

# this function will tell you PSMC's population size estimate at time "pos" given the x (time) and y (relative pop) data
eval_popsize = function(pos, x, y){ 
  xIndex = length(which(x <= pos))
  return(y[xIndex])
}

# this function will tell you the absolute difference in between two curves A and B at position "xpos". we will later integrate this to get an area between two curves
absdifference = function(xpos,d1,d2,d3,d4){ # d1, d2 are x and y of curve A. d1, d2 are x and y of curve B. you want to compare curves A and B.
  out = NULL
  n=length(xpos)
  for (i in 1:n) {
    out[i] = abs( eval_popsize(xpos[i],d1,d2) - eval_popsize(xpos[i],d3,d4) )
  }
  return(out)
}

################################################################################
# 30 Mbp #######################################################################
################################################################################
# ms command:
# msHOT-lite 2 1 -t 30000 -r 6000 30000000 -eN 0.01 0.1 -eN 0.06 1 -eN 0.2 0.5 -eN 1 1 -eN 2 2 -l 
# to convert ms time (first number after -eN) to N_0 generations, multiply by 4
# the second number after -eN is the population relative to N_0
# Therefore, our true times and true populations are

TrueTimes = 4*c(0.01,0.06,0.2,1,2)
TruePops = c(0.1,1,0.5,1,2)

### error stuff

setwd("/home/alex/Desktop/Simulations/SplittingChromosomes/binarySplitData1Chromosome30Mbp/extractPSMCoutput/")
filelist30 = list.files(pattern = "*.txt")

logmins = rep(0,length(filelist30)+1)
logmaxs = rep(0,length(filelist30)+1)

i = 1
# find the max of mins and min of maxs. These will be our upper and lower limits of integration
for (infile in filelist30) {
  data.infile = read.table(infile,header=TRUE)
  logmins[i] = log(min(data.infile$t_k/2)) # gotta divide by 2 to scale PSMC time output to N_0 generations. THIS IS IMPORTANT OTHERWISE THE PROGRAMS STUFF! MUST DO WHENEVER THERE IS A t_k
  logmaxs[i] = log(max(data.infile$t_k/2))
  
  i = i+1
}
logmins[length(filelist30)+1] = log(min(TrueTimes)) # add true value
logmaxs[length(filelist30)+1] = log(max(TrueTimes)) # add true value

logmax_of_mins = max(logmins)
logmin_of_maxs = min(logmaxs)

logResults = data.frame() # for storing results

for (infile in filelist30) {
  data.infile = read.table(infile,header=TRUE)
  #print(infile)
  # from d1 onwards is extra parameters
  integration_val = integrate(absdifference,logmax_of_mins,logmin_of_maxs, d1=log(TrueTimes), d2=TruePops, d3=log(data.infile$t_k/2), d4=data.infile$lambda_k, subdivisions=10000) # need to divide t_k by 2 as said above
  int_and_info = c(integration_val$value,integration_val$abs.error)
  logResults = rbind(logResults,int_and_info)
}
colnames(logResults) = c("Integral","Abs.Error")
rownames(logResults) = c(filelist30)

logOrdResults_MaxOfMins_MinOfMaxs = logResults[order(logResults$Integral),]
Results30 = logOrdResults_MaxOfMins_MinOfMaxs
Results30

powerOrder = c(0,1,3,2,4,5,6,10,9,8,7) # take from Results30
SplitOrder = unlist(lapply(powerOrder, function(x){1/2^x})) # lapply: applies a function to every component of a vector and returns a list. unlist makes the list a vector. kinda like a for loop but better!

#logSplitOrder2 = c(1,1/2,1/8,1/4,1/32,1/16,1/64,1/256,1/128,1/512,1/1024)

# is the split order important, or is it the number of bases in the chunks?
#powers = c(0:10)
#numbers = lapply(powers, function(x){1/2^x})

num_bases_in_order30 = unlist(lapply(SplitOrder, function(x){ceiling(30*10^6*x)})) # note: this ceiling command is technically not quite correct. For example, if the sequence was AAACCCG, the first split would be into AAAC CCG, while my command is assuming a split into 2*4 base long sequences. But the numbers are so large it won't really matter.
plot(num_bases_in_order30,Results30$Integral)

################################################################################
# 20 Mbp #######################################################################
################################################################################
# ms command:
# msHOT-lite 2 1 -t 20000 -r 4000 20000000 -eN 0.01 0.1 -eN 0.06 1 -eN 0.2 0.5 -eN 1 1 -eN 2 2 -l  
# to convert ms time (first number after -eN) to N_0 generations, multiply by 4
# the second number after -eN is the population relative to N_0
# Therefore, our true times and true populations are

TrueTimes = 4*c(0.01,0.06,0.2,1,2) # true pops are the same
TruePops = c(0.1,1,0.5,1,2)

### error stuff

setwd("/home/alex/Desktop/Simulations/SplittingChromosomes/binarySplitData1Chromosome20Mbp/")
filelist20 = list.files(pattern = "*.txt")

logmins = rep(0,length(filelist20)+1)
logmaxs = rep(0,length(filelist20)+1)

i = 1
# find the max of mins and min of maxs. These will be our upper and lower limits of integration
for (infile in filelist30) {
  data.infile = read.table(infile,header=TRUE)
  logmins[i] = log(min(data.infile$t_k/2)) # gotta divide by 2 to scale PSMC time output to N_0 generations. THIS IS IMPORTANT OTHERWISE THE PROGRAMS STUFF! MUST DO WHENEVER THERE IS A t_k
  logmaxs[i] = log(max(data.infile$t_k/2))
  
  i = i+1
}
logmins[length(filelist30)+1] = log(min(TrueTimes30)) # add true value
logmaxs[length(filelist30)+1] = log(max(TrueTimes30)) # add true value

logmax_of_mins = max(logmins)
logmin_of_maxs = min(logmaxs)

logResults = data.frame() # for storing results

for (infile in filelist30) {
  data.infile = read.table(infile,header=TRUE)
  #print(infile)
  # from d1 onwards is extra parameters
  integration_val = integrate(absdifference,logmax_of_mins,logmin_of_maxs, d1=log(TrueTimes30), d2=TruePops30, d3=log(data.infile$t_k/2), d4=data.infile$lambda_k, subdivisions=10000) # need to divide t_k by 2 as said above
  int_and_info = c(integration_val$value,integration_val$abs.error)
  logResults = rbind(logResults,int_and_info)
}
colnames(logResults) = c("Integral","Abs.Error")
rownames(logResults) = c(filelist30)

logOrdResults_MaxOfMins_MinOfMaxs = logResults[order(logResults$Integral),]
Results30 = logOrdResults_MaxOfMins_MinOfMaxs
Results30

powerOrder = c(0,1,3,2,4,5,6,10,9,8,7) # take from Results30
SplitOrder = unlist(lapply(powerOrder, function(x){1/2^x})) # lapply: applies a function to every component of a vector and returns a list. unlist makes the list a vector. kinda like a for loop but better!

#logSplitOrder2 = c(1,1/2,1/8,1/4,1/32,1/16,1/64,1/256,1/128,1/512,1/1024)

# is the split order important, or is it the number of bases in the chunks?
#powers = c(0:10)
#numbers = lapply(powers, function(x){1/2^x})

num_bases_in_order30 = unlist(lapply(SplitOrder, function(x){ceiling(30*10^6*x)})) # note: this ceiling command is technically not quite correct. For example, if the sequence was AAACCCG, the first split would be into AAAC CCG, while my command is assuming a split into 2*4 base long sequences. But the numbers are so large it won't really matter.
plot(num_bases_in_order30,Results30$Integral)

