###
# Working out an error for PSMC runs of small bits vs the true thing
###

# read some data in

# just use this for test and true data for now...
setwd("/home/alex/Desktop/Analyses/151217a_FromShaun/ShaunSimulatedData/")
TestData<-read.table("testPdf.0.txt",header=FALSE)
TrueData <- read.table("100.ms.psmcfa.psmc.plot.0.txt",header=FALSE)

# NEED TO FIGURE OUT WHERE THE CUT-OFF IS: Li & Durbin claim it's bw 20 kyr and 3 Myr

# get the first point from Test
ValidTestMinPos = which(TestData$V1 >= 20000 & TestData$V1 <= 3000000)[[1]]
ValidTestMinPos
ValidTestMin = TestData$V1[ValidTestMinPos]
ValidTestMin

# get the first point from True
ValidTrueMinPos = which(TrueData$V1 >= 20000 & TrueData$V1 <= 3000000)[[1]]
ValidTrueMinPos
ValidTrueMin = TrueData$V1[ValidTrueMinPos]
ValidTrueMin

ValueStatement = ValidTestMin > ValidTrueMin # which is the MOST ANCIENT of the two most recent valid first points
ValueStatement
if (ValueStatement == TRUE) { # TRUE means that Test is more ancient, FALSE means that True is more ancient
  Value = TestData$V2[ValidTestMinPos] # Take the population value of the Test run
} else {
    Value = TrueData$V2[ValidTestMinPos] # Take the population value of the True data
  }
Value

######
# Now we want to find the first place where we change in value
TestChange = which(x =/= Value)[[1]]

######

# PLAY WITH FUNCTIOMS
PiecewiseConstantFunc = function(InVec){
  n = length(InVec)
  
  for (v in 1:n) {
    
  }
}

plot( function(x){
  if ( x>=1 ) {x}
  ifelse( {x< 1 & x>0}, x,  -x) 
} -1, 2)

# need to find where we change in value

#while ()