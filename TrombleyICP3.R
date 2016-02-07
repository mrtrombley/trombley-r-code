# Matt Trombley
# STAT 4020
# April 27, 2015
# In-Class Project 3

setwd('/Users/matttrombley/Desktop/STAT 4020/ICP 3')

#1(a) Read the data into R, convert categorical variables to factors, print first 5 observations

nbasalary <- read.csv('nbasalaries.csv',header=F)
colnames(nbasalary) <- c('Name','Team','Salary','Season')
#Columns 1, 2, and 4 are already factors and do not need converting
nbasalary[1:5,]

#1(b) Create a position variable and print the first 5 observations

namegap <- regexpr(",",nbasalary$Name)
length <- nchar(as.character(nbasalary$Name),allowNA=TRUE)
nbasalary$Position <- substring(as.character(nbasalary$Name),namegap+2,length)
nbasalary[1:5,]

#1(c) Create a new factor variable to uncode the position

nbasalary$Uncoded.Position <- as.factor(ifelse(nbasalary$Position=="PG","Point Guard",ifelse(nbasalary$Position=="SG","Guard",ifelse(nbasalary$Position=="G","Guard",ifelse(nbasalary$Position=="SF","Small forward",ifelse(nbasalary$Position=="PF","Power forward",ifelse(nbasalary$Position=="C","Center","NA")))))))
nbasalary[1:5,]

#1(d) Use a for loop to calculate average salaries and place them in a matrix

playersalaries <- subset(nbasalary,Uncoded.Position!="NA")
avgsalary <- matrix(0,nrow=length(levels(nbasalary$Season)),ncol=length(levels(nbasalary$Uncoded.Position))-1)
colnames(avgsalary) <- c("Center","Guard","Point guard","Power forward", "Small forward")
rownames(avgsalary) <- c("2008 - 2009","2009 - 2010","2010 - 2011","2011 - 2012","2012 - 2013","2013 - 2014","2014 - 2015")
for (i in 1:length(levels(nbasalary$Season))){
	yearsub <- droplevels(subset(playersalaries,Season==levels(nbasalary$Season)[i]))
	avgvals <- tapply(yearsub$Salary,yearsub$Uncoded.Position,mean)/100000
	avgsalary[i,1] <- avgvals[1]
	avgsalary[i,2] <- avgvals[2]
	avgsalary[i,3] <- avgvals[3]
	avgsalary[i,4] <- avgvals[4]
	avgsalary[i,5] <- avgvals[5]
 }

#1(e) Create a graph of the data found in part D

col.year=c("black","red","green","blue","turquoise","pink","yellow")
barplot(avgsalary,beside=T,col=col.year,ylim=c(0,60),main="NBA Average Salaries by Season\nand Position", xlab="Season",ylab="Average Salary\n(Hundreds of Thousands of Dollars)")
box()
legend(10,60,legend=rownames(avgsalary),col=par(col.year))


