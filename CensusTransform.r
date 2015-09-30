### To read and transform US Census and ACS data at state level for analysis of foster care entries
### Depends on data files available through Steven Ruggles, Katie Genadek, Ronald Goeken, Josiah Grover, and Matthew Sobek. Integrated Public Use Microdata Series: Version 6.0 [Machine-readable database]. Minneapolis: University of Minnesota, 2015.
### https://usa.ipums.org/usa/
### File used contains the following samples: [2000 1%, 2001 ACS, 2002 ACS, 2003 ACS, 2004 ACS,
### 2005 ACS, 2006 ACS, 2007 ACS 3yr, 2008 ACS 3yr, 2009 ACS 3yr, 2010 ACS 3yr, 2011 ACS 3yr]
### File contains the following variables: [YEAR, DATANUM, SERIAL, HHWT, STATEFIP, GQ, RENTGRS,
### PERNUM, PERWT, NCHILD, MOMLOC, POPLOC, STEPPOP, RELATE, RELATED, SEX, AGE, RACE, RACED, HISPAN,
### HISPAND, EDUC, EDUCD, EMPSTAT, EMPSTATD, LABFORCE, POVERTY, GCHOUSE]

rm(list=ls())
setwd("LOCATION OF IPUMS FILE") 
library(data.table)

### Function to transform individual-level IPUMS data into state-level counts for 
### study-relevant demographic variables. 
### See IPUMS codebooks for census/ACS variable definitions and values
state<-function(x){
	state<-unique(x$STATEFIP)
	year<-rep(x$YEAR[1], length(state))

	pop<-tapply(x$PERWT, x$STATEFIP, sum)
	blkpop<-tapply(x$PERWT*(x$RACE==2), x$STATEFIP, sum)
	whtpop<-tapply(((x$RACE==1)*x$PERWT*(x$HISPAN==0)), x$STATEFIP, sum)

	child<-tapply(((x$AGE<18)*x$PERWT), x$STATEFIP, sum)
	blkchild<-tapply(((x$AGE<18)*(x$RACE==2)*x$PERWT), x$STATEFIP, sum)
	whtchild<-tapply(((x$AGE<18)*(x$RACE==1)*x$PERWT*(x$HISPAN==0)), x$STATEFIP, sum)

	adult<-pop-child
	blkadult<-blkpop-blkchild
	whtadult<-whtpop-whtchild

	pov<-tapply(((x$POVERTY<=100)*x$PERWT), x$STATEFIP, sum)
	childpov<-tapply((x$POVERTY<=100)*(x$AGE<18)*x$PERWT, x$STATEFIP, sum)
	blkpov<-tapply((x$POVERTY<=100)*(x$RACE==2)*x$PERWT, x$STATEFIP, sum)
	whtpov<-tapply((x$POVERTY<=100)*(x$RACE==1)*(x$HISPAN==0)*x$PERWT, x$STATEFIP, sum)
	blkchildpov<-tapply((x$POVERTY<=100)*(x$AGE<18)*(x$RACE==2)*x$PERWT, x$STATEFIP, sum)
	whtchildpov<-tapply((x$POVERTY<=100)*(x$RACE==1)*(x$HISPAN==0)*(x$AGE<18)*x$PERWT, x$STATEFIP, sum)

	unemp<-tapply(((x$EMPSTAT==2)*x$PERWT), x$STATEFIP, sum)
	whtunemp<-tapply(((x$EMPSTAT==2)*
		(x$RACE==1)*(x$HISPAN==0)*x$PERWT), x$STATEFIP, sum)
	blkunemp<-tapply(((x$EMPSTAT==2)*
		(x$RACE==2)*x$PERWT), x$STATEFIP, sum)

	emp<-tapply((x$EMPSTAT==1)*x$PERWT, x$STATEFIP, sum)
	whtemp<-tapply((x$EMPSTAT==1)*
		(x$RACE==1)*(x$HISPAN==0)*x$PERWT, x$STATEFIP, sum)
	blkemp<-tapply((x$EMPSTAT==1)*
		(x$RACE==2)*x$PERWT, x$STATEFIP, sum)
		

	LessHS<-(tapply((x$EDUC<6)*(x$AGE>25)*(x$PERWT), x$STATEFIP, sum)/
		tapply((x$AGE>25)*(x$PERWT), x$STATEFIP, sum))
	wLessHS<-tapply((x$EDUC<6)*(x$AGE>25)*(x$PERWT)*
		(x$RACE==1)*(x$HISPAN==0), x$STATEFIP, sum)/
		tapply((x$AGE>25)*(x$PERWT)	*(x$RACE==1)*(x$HISPAN==0), x$STATEFIP, sum)
	bLessHS<-tapply((x$EDUC<6)*(x$AGE>25)*(x$PERWT)*
		(x$RACE==2), x$STATEFIP, sum)/
		tapply((x$AGE>25)*(x$PERWT)*(x$RACE==2), x$STATEFIP, sum)

	kidskincare<-tapply(((x$RELATE==9)|(x$RELATE==10))*
		(x$AGE<18)*x$PERWT, x$STATEFIP, sum)
	wkidskincare<-tapply(((x$RELATE==9)|(x$RELATE==10))*
		(x$RACE==1)*(x$HISPAN==0)*(x$AGE<18)*x$PERWT, x$STATEFIP, sum)
	bkidskincare<-tapply(((x$RELATE==9)|(x$RELATE==10))*
		(x$RACE==2)*(x$AGE<18)*x$PERWT, x$STATEFIP, sum)

	kids2par<-tapply((x$POPLOC!=0)*(x$STEPPOP==0)*(x$MOMLOC!=0)*
		(x$AGE<18)*x$PERWT, x$STATEFIP, sum)
	wkids2par<-tapply((x$POPLOC!=0)*(x$STEPPOP==0)*(x$MOMLOC!=0)*
		(x$RACE==1)*(x$HISPAN==0)*(x$AGE<18)*x$PERWT, x$STATEFIP, sum)
	bkids2par<-tapply((x$POPLOC!=0)*(x$STEPPOP==0)*(x$MOMLOC!=0)*
		(x$RACE==2)*(x$AGE<18)*x$PERWT, x$STATEFIP, sum)

	### CREATE SUM TOTAL RENT FOR ACS RESPONDENTS 0,1 as missing
	rent<-tapply((x$RENTGRS!=0)*(x$RENTGRS!=1)*(as.double(x$RENTGRS)), 
		x$STATEFIP, sum)
	### CREATE SUM OF TOTAL RENTERS IN ACS
	renter<-tapply((x$RENTGRS!=0)*(x$RENTGRS!=1), 
		x$STATEFIP, sum)
	### CREATE AVERAGE RENT BY STATE IN ACS
	avgrent<-rent/renter
	
	agg<-cbind(state, year, pop, whtpop, blkpop, child,  blkchild, whtchild, adult,
		blkadult, whtadult, pov, childpov, blkpov, whtpov, blkchildpov, whtchildpov,  
		unemp, whtunemp, blkunemp, emp, whtemp, blkemp, 
		LessHS, wLessHS, bLessHS,
		kidskincare,bkidskincare,wkidskincare,
		kids2par, wkids2par, bkids2par, avgrent)

	return(agg)
}


years<-(2000:2012)
popdat<-data.frame("state"=NA, "year"=NA, "pop"=NA, "whtpop"=NA, 
	"blkpop"=NA, "child"=NA,  "blkchild"=NA, "whtchild"=NA, "adult"=NA,
	"blkadult"=NA, "whtadult"=NA, "pov"=NA, "childpov"=NA, "blkpov"=NA, 
	"whtpov"=NA, "blkchildpov"=NA, "whtchildpov"=NA, "unemp"=NA, "whtunemp"=NA, 
	"blkunemp"=NA, "emp"=NA, "whtemp"=NA, "blkemp"=NA, 
	"LessHS"=NA, "wLessHS"=NA, "bLessHS"=NA,
	"kidskincare"=NA, "wkidskincare"=NA, "bkidskincare"=NA,
	"kids2par"=NA, "wkids2par"=NA, "bkids2par"=NA, "avgrent"=NA)

temp<-fread("usa_00029.csv", drop=c("DATANUM", "SERIAL", "HHWT", "PERNUM",
		"EDUCD","RELATED", "RACED", "HISPAND",	"EMPSTATD", "LABFORCE", "GCHOUSE"))
for(i in 1:length(years)){
	tempdat<-temp[temp$YEAR==years[i],]
	tempdat<-tempdat[!((tempdat$GQ==3)|(tempdat$GQ==4)),]
	popdat<-rbind(popdat, state(tempdat))
	rm(tempdat)
}

popdat<-na.omit(popdat)
### DROP DC
popdat<-popdat[popdat$state!=11,]



write.csv(popdat, "popdat.csv")



