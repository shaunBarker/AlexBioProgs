######################
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
######################

setwd("/home/alex/Desktop/Analyses/151217a_FromShaun/ShaunSimulatedData/")
TestData<-read.table("testPdf.0.txt",header=FALSE)
TrueData <- read.table("100.ms.psmcfa.psmc.plot.0.txt",header=FALSE)

eval_popsize = function(pos, x, y){
  xIndex = length(which(x <= pos))
  return(y[xIndex])
}

BlX = c(1,3,4,4.5,6,7.5,8,9)
BlY = c(1,1,1,7,4,4,4,4)

GrX = c(1.5,3.5,5,5.5,6,7,8,10)
GrY = c(2,2,2,7,3,4,2,10)


####
T1X = c(0:3)
T1Y = c(0,1,1,0)
T2X = c(0:3)
T2Y = rep(0.5,4)

absdifference2 = function(xposs){
  abs( eval_popsize(xposs,T1X,T1Y) - eval_popsize(xposs,T2X,T2Y) )
}
integrate(absdifference2,0,3)


####

out = NULL
absdifference = function(xpos){
  n=length(xpos)
  for (i in 1:n) {
    out[i] = abs( eval_popsize(xpos[i],BlX,BlY) - eval_popsize(xpos[i],GrX,GrY) )
  }
  return(out)
}

for (i in 4:17) {
  print(absdifference(i/2))
}

y=c(0,1)
test = function(x) {
  return(y+x)
}

integrate(absdifference,2,8.5)
