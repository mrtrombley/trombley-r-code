#Matt Trombley
#STAT 4020
#Project 6

#1) Create three vectors using functions

a <- rep(seq(-4,4,2),each=5)
b <- rep(seq(100,90,-2),3)
c <- rep(c("HIGH","LOW"),c(3,2))

#2) Create a data frame for the euro data

euro <- data.frame("Conversion"=c(13.76, 40.34, 1.96, 166.39, 5.95, 6.56, 0.79, 1936.27, 40.34, 2.20, 200.48),"Country"=c("ATS","BEF","DEM","ESP","FIM","FRF","IEP","ITL","LUF","NLG","PTE"))

#3a) Read the tdf xlsx file into R

library(xlsx)
tdfxlsx <- read.xlsx("tdf2012.xlsx",1)
  
#3b) Store the tdf data in a data frame

tdf <- read.csv("tdf2012.csv",head=T)
tdf[1:5,1:5]

#3c) Change column names in the tdf data frame

colnames(tdf)[1:4] <- c("RiderName","TeamName","BirthYear","RiderHeight")
tdf[1:5,1:5]

#3d) Create a new variable BMI in the tdf data frame

BMI <- tdf$weight/(tdf$RiderHeight/100)^2
tdf <- cbind(BMI,tdf) 
tdf[1:5,1:5]