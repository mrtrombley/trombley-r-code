#1a) Read the data into R

tdf <- read.csv('tdf2012.csv',header=T)
tdf[1:5,]

#1b) Find the dimensions of the data frame

dim(tdf)

#1c) Summarize the data using str() and summary()

str(tdf)
tdf$birth <- as.factor(tdf$birth)
summary(tdf)

#1d) Create a variable for the last name of the riders

firstspace <- regexpr(" ",tdf$rider)
tdf$lastname <- substring(as.character(tdf$rider),1,firstspace-1)

#1e) Create a stagesum variable and print lastname, team, and stagesum

tdf[is.na(tdf)] <- 0
tdf$stagesum <- rowSums(tdf[,7:27])
tdf[1:5,c("lastname","team","stagesum")]

#1f) Create a new data frame for BMC, Garmin, and Europcar

tdfnew <- subset(tdf, team == "BMC RACING TEAM" | team == "GARMIN-SHARP" | team == "TEAM EUROPCAR")
tdfnew[1:5,]

#1g) Create a bar graph for the data set

tdfnew$team <- factor(tdfnew$team)
tdfnew$nation <- factor(tdfnew$nation)
team.freq <- table(tdfnew$team,tdfnew$nation)
team.relfreq <- 3*team.freq/sum(team.freq)
col.team <- c("red","green","blue")
barplot(team.relfreq,width=2,beside=T,col=col.team,main="Relative Frequency of Nations Represented in the\nTour de France in 2012 by Team",ylim=c(0,1),xlab="Country",ylab="Relative Frequency")
box()
bar.labels <- c("BMC RACING TEAM","GARMIN-SHARP","TEAM EUROPCAR")
legend("topleft",legend=bar.labels,col=col.team,pch=15,ncol=1)

#2a) Create two data frames: country and currency

euro <- data.frame("Conversion"=c(13.76, 40.34, 1.96, 166.39, 5.95, 6.56, 0.79, 1936.27, 40.34, 2.20, 200.48),"Currency"=c("ATS","BEF","DEM","ESP","FIM","FRF","IEP","ITL","LUF","NLG","PTE"))
eurocountry <- data.frame("Country"=c("Austria", "Belgian", "Cyprus", "Germany", "Estonia", "Spain", "Finland", "France", "Greece", "Ireland", "Italy", "Lithuania", "Luxemburg", "Latvia", "Monaco", "Malta", "Netherlands", "Portugal", "Slovenia", "Slovakia", "San Marino", "Vatican City"),"Currency"=c("ATS", "BEF", "CYP", "DEM", "EEK", "ESP", "FIM", "FRF", "GRD", "IEP", "ITL", "LTL", "LUF", "LVL", "MCF", "MTL", "NLG", "PTE", "SIT", "SKK", "SML", "VAL"))
euro[1:5,]
eurocountry[1:5,]

#2b) Sort the data sets by the variable currency

sort(euro$Currency)
sort(eurocountry$Currency)

#2c) Create a merged data set from the two data frames

datamerged <- merge(euro,eurocountry,by="Currency",all=T)
datamerged[1:5,]
