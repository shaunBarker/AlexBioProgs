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

ValueStatement = ValidTestMin > ValidTrueMin # interested in max it cuts of anything where we can't make a comparison
ValueStatement
if (ValueStatement == TRUE) {
  Value = TestData$V2[ValidTestMinPos]
} else {
    Value = TrueData$V2[ValidTestMinPos]
  }
Value

######
# try brute force first...
TtestChange

######

# need to find where we change in value

#while ()