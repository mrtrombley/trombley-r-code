tdf <- read.csv('tdf2012.csv',header=T)

#1a) Create a new variable stagesum for the rider's data

tdf[is.na(tdf)] <- 0
tdf$stagesum <- rowSums(tdf[,7:27])
tdf[1:5,c("rider","team","stagesum")]

#1b) Create side-by-side box-and-whisker plots for stagesum by team name

tdfnew <- subset(tdf,team == "BMC RACING TEAM" | team == "GARMIN-SHARP" | team == "TEAM EUROPCAR")
tdfnew$team <- factor(tdfnew$team)
boxplot(tdfnew$stagesum ~ tdfnew$team,xlab="Team Name",ylab="Sum of Stage Times (seconds)")
title("Sum of Stage Times for Tour De France Riders by Team")

#1c) Create a scatterplot of the stage 1 times vs. sum of stage times

for (i in 1:length(levels(tdfnew$team))){
	teamdata <- tdfnew[tdfnew$team==levels(tdfnew$team)[i],]
	plot(teamdata$S1,teamdata$stagesum,xlim=c(17850,18250),ylim=c(230000,330000),xlab="",ylab="",
	yaxt="n",xaxt="n",col=i,pch=14+i)
	par(new=T)
}
axis(1,at=seq(17850,18250,50),labels=seq(17850,18250,50))
axis(2,at=seq(230000,330000,10000),labels=seq(230000,330000,10000))
mtext("Stage 1 Time (seconds)",side=1,font=1,line=3)
mtext("Sum of Stage Times (seconds)",side=2,font=1,line=3)
col.team=c(1,2,3)
pch.team=c(15,16,17)
legend("bottomright",legend=levels(as.factor(teamdata$team)),
col=col.team,pch=pch.team,ncol=1)
title("Stage 1 Time vs. Sum of Stage Times for\nTour De France Riders by Team")

#1d) Write a function to compute a confidence interval

cifun <- function(y,alpha){
	ybar <- mean(y)
	n <- length(y)
	s <- sd(y)
	t <- c(-1,1)*(qt((1-alpha)/2,n-1,lower.tail=F))
	interval <- ybar+t*(s/sqrt(n))
	return(interval)
}

#1e) Select observations using the sample function

tdfsample <- tdf[sample(1:nrow(tdf),15),]

#1f) Find a confidence interval using the data in 1e

cifun(tdfsample$stagesum,0.9)

#1g) Find a confidence interval with t.test for the data in 1e

t.test(tdfsample$stagesum,conf.level=0.9)
