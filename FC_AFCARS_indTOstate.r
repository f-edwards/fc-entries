rm(list=ls())
setwd("AFCARS DATA LOCATION") 
require(foreign)
require(MASS)
require(data.table)
require(dplyr)
require(plyr)

### SET UP 2 MEASURES - First entries, annual caseload - annual caseload is 
### all children in AFCARS for given year - captures new interventions + long-term 

keeps<-c("state", "fipscode", "sex", "amiakn", 
	"asian", "blkafram", "hawaiipi", "white", "untodetm","hisorgin", 
	"clindis", "mr", "vishear", "phydis", "dsmiii", "othermed",
	"numplep", "manrem", "phyabuse", "sexabuse", "neglect",
	"aaparent", "daparent", "aachild", "dachild", "childdis",
	"chbehprb", "prtsdied", "prtsjail", "nocope", "abandmnt",
	"relinqsh", "housing", "curplset","placeout","casegoal", "lifelos",
	"dob", "ageatend", "inatend", "served","entered", "iswaiting", "istpr")

dropstates<-c(11, 72)
### CREATE VARIABLE VECTOR OF KEEPS - 

readcf<-function(filename, year){ ### Read in child file, drop all columns not in keeps
	names<-tolower(names(read.csv(filename, nrows=1)))
	m<-which(names%in%keeps)
	cfoutput<-fread(filename, select=m)
	setnames(cfoutput,old=names(cfoutput), new=tolower(names(cfoutput)))
	#cfoutput<-cfoutput[cfoutput$manrem!=1, ]  WEIRD PATTERNS IN NJ, PA - SOME STATES DON'T HAVE VOLUNTARY PLACEMENT
	cfoutput$year<-year
	return(cfoutput)
}

makesf<-function(cf){

	### Entries by race
	entered<-aggregate(entered~state+year,cf, sum)
	blkent<-aggregate(entered~state+year,cf[cf$blkafram==1,], sum)
	names(blkent)[3]<-"bent"
	went<-aggregate(entered~state+year, cf[(cf$white*(cf$hisorgin!=1)*(cf$blkafram!=1))==1,], sum)
	names(went)[3]<-"went"

	entered.nkin<-aggregate(entered~state+year, cf[cf$curplset!=2,], sum)
	names(entered.nkin)[3]<-"entered.nkin"


	### Caseloads by race as total by state, year
	cl<-aggregate((entered==0)~state+year, cf, sum)
	names(cl)[3]<-"cl"
	cl[,3]<-entered[,3]+cl[,3]
	bcl<-aggregate((entered==0)~state+year, cf[cf$blkafram==1,], sum)
	names(bcl)[3]<-"bcl"
	bcl[,3]<-blkent[,3]+bcl[,3]

	wcl<-aggregate((entered==0)~state+year, cf[(cf$white*(cf$hisorgin!=1)*(cf$blkafram!=1))==1,], sum)
	names(wcl)[3]<-"wcl"
	cl[,3]<-entered[,3]+cl[,3]
	wcl[,3]<-went[,3]+wcl[,3]


	### Current Setting by race
	cpl<-aggregate(cbind(curplset==1, curplset==2, curplset==3, curplset==4, 
		curplset==5, curplset==6, curplset==7, curplset==8)~state+year, cf, sum)
	bcpl<-aggregate(cbind(curplset==1, curplset==2, curplset==3, curplset==4, 
		curplset==5, curplset==6, curplset==7, curplset==8)~state+year, cf[cf$blkafram==1,], sum)
	wcpl<-aggregate(cbind(curplset==1, curplset==2, curplset==3, curplset==4, 
		curplset==5, curplset==6, curplset==7, curplset==8)~state+year, 
		cf[(cf$white*(cf$hisorgin!=1)*(cf$blkafram!=1))==1,], sum)
	plnames<-c("pre-adopt", "foster.rel", "foster.non.rel", "grphome", "inst", "indliv", "awol", "trialvis")

	names(cpl)[(ncol(cpl)-length(plnames)+1):ncol(cpl)]<-
		c("pl.pre-adopt", "pl.foster.rel", "pl.foster.non.rel", "pl.grphome", "pl.inst", "pl.indliv", "pl.awol", "pl.trialvis")

	names(bcpl)[(ncol(bcpl)-length(plnames)+1):ncol(bcpl)]<-
		c("b.pl.pre-adopt", "b.pl.foster.rel", "b.pl.foster.non.rel", "b.pl.grphome", "b.pl.inst", "b.pl.indliv", "b.pl.awol", "b.pl.trialvis")

	names(wcpl)[(ncol(wcpl)-length(plnames)+1):ncol(wcpl)]<-
		c("w.pl.pre-adopt", "w.pl.foster.rel", "w.pl.foster.non.rel", "w.pl.grphome", "w.pl.inst", "w.pl.indliv", "w.pl.awol", "w.pl.trialvis")
	
	###	Case Goal Counts by Race
	cgl<-aggregate(cbind(casegoal==1, casegoal==2, casegoal==3, casegoal==4, 
		casegoal==5, casegoal==6, casegoal==7, casegoal==99)~state+year, cf, sum)
	bcgl<-aggregate(cbind(casegoal==1, casegoal==2, casegoal==3, casegoal==4, 
		casegoal==5, casegoal==6, casegoal==7, casegoal==99)~state+year, cf[cf$blkafram==1,], sum)
	wcgl<-aggregate(cbind(casegoal==1, casegoal==2, casegoal==3, casegoal==4, 
		casegoal==5, casegoal==6, casegoal==7, casegoal==99)~state+year, 
		cf[(cf$white*(cf$hisorgin!=1)*(cf$blkafram!=1))==1,], sum)

	gnames<-c("cg.reunify", "cg.other.rel", "cg.adopt", "cg.long.fc", "cg.emanc", "cg.guard", "cg.noplan", "cg.na")
	
	names(cgl)[(ncol(cgl)-length(gnames)+1):ncol(cgl)]<-gnames

	names(bcgl)[(ncol(bcgl)-length(gnames)+1):ncol(bcgl)]<-
	c("b.cg.reunify", "b.cg.other.rel", "b.cg.adopt", "b.cg.long.fc", "b.cg.emanc", "b.cg.guard", "b.cg.noplan", "b.cg.na")

	names(wcgl)[(ncol(wcgl)-length(gnames)+1):ncol(wcgl)]<-
	c("w.cg.reunify", "w.cg.other.rel", "w.cg.adopt", "w.cg.long.fc", "w.cg.emanc", "w.cg.guard", "w.cg.noplan", "w.cg.na")

	### Removal reason - jail
	jail<-aggregate(prtsjail==1~state+year, cf[cf$entered==1,], sum)

	fc.merge<-list(entered, entered.nkin, blkent, went, cl, bcl, wcl, cpl, bcpl, wcpl, cgl, bcgl, wcgl, jail)

	fc.state<-join_all(fc.merge, by=c("state", "year"))

	return(fc.state)
}

### Child File Read

files<-c("afcarsfc2000.csv", "afcarsfc2001.csv", 
	"afcarsfc2002.csv","afcarsfc2003.csv","afcarsfc2004.csv","afcarsfc2005.csv",
	"afcarsfc2006.csv","afcarsfc2007.csv","afcarsfc2008.csv",
	"afcarsfc2009.csv","afcarsfc2010.csv","afcarsfc2011.csv")

beginyear<-1999

fc.temp<-list() ### Dump object as null list
fc.sf<-list()

### Loop to read in all files in "files" into fcchild
for(i in (1:length(files))){
	fc.temp[[i]]<-readcf(files[i], (beginyear+i))
}

#write.csv(rbind_all(fc.temp), file="fcchild.csv")

for(i in (1:length(files))){
	fc.sf[[i]]<-makesf(fc.temp[[i]])
	print(i)
}
fcstate<-rbind_all(fc.sf)

write.csv(rbind_all(fc.sf), "fcstate.csv")

rm(list=ls())
q(save="no")