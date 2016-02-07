#Matt Trombley
#STAT 4020
#Project 7
#4/14/2015

#1a) 

tdf <- read.csv('tdf2012.csv',header=T)
tdf[1:5,]

#1b) 

dim(tdf)

#1c) 

str(tdf)
tdf$birth <- as.factor(tdf$birth)
summary(tdf)

#1d)

firstspace <- regexpr(" ",tdf$rider)
tdf$lastname <- substring(as.character(tdf$rider),1,firstspace-1)

#1e)

tdf[is.na(tdf)] <- 0
tdf$stagesum <- rowSums(tdf[,7:27])

#1f) 

tdfnew <- tdf[tdf$team == "BMC RACING TEAM" | tdf$team == "GARMIN-SHARP" | tdf$team == "TEAM EUROPCAR",]





#2a) 

euro <- data.frame("Conversion"=c(13.76, 40.34, 1.96, 166.39, 5.95, 6.56, 0.79, 1936.27, 40.34, 2.20, 200.48),"Country"=c("ATS","BEF","DEM","ESP","FIM","FRF","IEP","ITL","LUF","NLG","PTE"))
eurocountry <- data.frame("Country"=c("Austria", "Belgian", "Cyprus", "Germany", "Estonia", "Spain", "Finland", "France", "Greece", "Ireland", "Italy", "Lithuania", "Luxemburg", "Latvia", "Monaco", "Malta", "Netherlands", "Portugal", "Slovenia", "Slovakia", "San Marino", "Vatican City"),"CurrencyCode"=c("ATS", "BEF", "CYP", "DEM", "EEK", "ESP", "FIM", "FRF", "GRD", "IEP", "ITL", "LTL", "LUF", "LVL", "MCF", "MTL", "NLG", "PTE", "SIT", "SKK", "SML", "VAL"))
euro[1:5,]
eurocountry[1:5,]