### Code for multiple imputation, measure construction, and modeling 
### depends on FC_Read.r, FCFunctions.r, and access to data described in those files

rm(list=ls())
options(scipen=999)
set.seed(1)

require(MASS)
require(texreg)	
require(xtable)	
require(scales)
require(lme4)
require(Matrix)
require(plyr)
require(grid)
require(car)
require(arm)
require(lattice)
require(Amelia)
library(stargazer)
library(mice)
set.seed(1)

setwd("DATA LOCATION")
source("FCFunctions.r", print.eval=TRUE)
source("FC_Read.r", print.eval=TRUE)

### Drop unused measures to simplify imputation
fc<-fc[,-(18:57)]

bounds<-cbind(1:ncol(fc), 0, Inf)

m<-5
fc.int<-amelia(fc, m=m, idvars=c("state","adult", "blkadult", "whtadult",
 	"statename"), ts="year", cs="stname", bound=bounds, polytime=2, 
	empri=.05*nrow(fc),intercs=TRUE)

fc.int<-transform(fc.int, obs_n=1:nrow(fc))
fc.int<-transform(fc.int, unemprt=unemp/(unemp+emp))
fc.int<-transform(fc.int, childnot2par=1-(kids2par/child))
fc.int<-transform(fc.int, chpovrt=childpov/child) 
fc.int<-transform(fc.int, gsppercap=GSP*1000000/pop)
fc.int<-transform(fc.int, crime.pc=crime/pop)
fc.int<-transform(fc.int, pctblk=blkpop/pop)
fc.int<-transform(fc.int, incarrt=incartot/adult)
fc.int<-transform(fc.int, tanf.adeq=AFDCBen3/rpp)
fc.int<-transform(fc.int, wic.incl=WIC.par/childpov)
fc.int<-transform(fc.int, medicaid.incl=medicaidrec/pov)
fc.int<-transform(fc.int, snap.incl=SNAPRec/pov)
fc.int<-transform(fc.int, tanf.incl=AFDCRec/childpov)
fc.int<-transform(fc.int, ideo=inst6010_nom)
fc.int<-transform(fc.int, police.pc=police.ft.emp/pop)
fc.int<-transform(fc.int, welfare.pc=welfare.ft.emp/pop)
fc.int<-transform(fc.int, death.rt=death.sent/new.incar)



##########################
#Descriptives
#### THIS IS THE DESCRIPTIVES OF STATE-LEVEL AVERAGES OVER 2002-2011 

	deskeep<-c("entered", "cl",  "child", "pl.inst", "unemprt", 
		"childnot2par","food.insec",
		"chpovrt","LessHS",	"gsppercap", "ideo",  
		"crime.pc", "pctblk",
		"tanf.adeq","snap.incl", "medicaid.incl", "tanf.incl",
		"welfare.pc", "incarrt", "death.rt", "police.pc")

	desdat<-fc.int$imputations[[1]][,colnames(fc.int$imputations[[1]])%in%deskeep]
	desdat$entrt<-desdat$entered/desdat$child
	desdat$clrt<-desdat$cl/desdat$child
	desdat$instrt<-desdat$pl.inst/desdat$cl

desdat[,c(24, 23, 22, 21, 20, 19,13,11)]<-desdat[,c(24, 23, 22, 21, 20, 19,13,11)]*1000

destab<-xtable(descriptives(desdat), digits=2)
### For html file output
# print.xtable(destab, type="html", file="desctable.html") 

### Loop models over imputed data
ent.scale<-list(m)
inst.scale<-list(m)
for(i in (1:m)){
	ent.scale[[i]]<-glmer(entered~scale(unemprt)+scale(childnot2par)+
		scale(food.insec)+scale(chpovrt)+scale(LessHS)+
		scale(gsppercap)+
		scale(ideo)+scale(crime.pc)+scale(pctblk)+
		scale(tanf.adeq)+scale(snap.incl)+
		scale(medicaid.incl)+
		scale(tanf.incl)*scale(welfare.pc)+
		scale(incarrt)+
		scale(death.rt)+
		scale(police.pc)+
		(1|stname)+(1|year)+(1|obs_n), offset=log(child),  
		data=fc.int$imputations[[i]],
		family=poisson)

	inst.scale[[i]]<-glmer(pl.inst~scale(unemprt)+scale(childnot2par)+
		scale(food.insec)+scale(chpovrt)+scale(LessHS)+
		scale(gsppercap)+
		scale(ideo)+scale(crime.pc)+scale(pctblk)+
		scale(tanf.adeq)+scale(snap.incl)+
		scale(medicaid.incl)+
		scale(tanf.incl)*scale(welfare.pc)+
		scale(incarrt)+
		scale(death.rt)+
		scale(police.pc)+
		(1|stname)+(1|year)+(1|obs_n), offset=log(cl),  
		data=fc.int$imputations[[i]],
		family=poisson)
}

### Combine results over estimated models

b.out.1<-b.out.2<-NULL
se.out.1<-se.out.2<-NULL
for(i in 1:m) {
	b.out.1 <- rbind(b.out.1, fixef(ent.scale[[i]]))
	b.out.2 <- rbind(b.out.2, fixef(inst.scale[[i]]))
	se.out.1<-rbind(se.out.1, se.coef(ent.scale[[i]])$fixef)
	se.out.2<-rbind(se.out.2, se.coef(inst.scale[[i]])$fixef)
}

ent.results <- mi.meld(q = b.out.1, se = se.out.1)
inst.results <- mi.meld(q=b.out.2, se=se.out.2)

#### Combine results over estimated models for visualization 
#### of estimated random state, year intercepts for both entries 
### and institutionalization models

s.b.out<-y.b.out<-NULL
s.se.out<-y.se.out<-NULL
for(i in 1:m) {
	s.b.out <- rbind(s.b.out, t(ranef(ent.scale[[i]])$stname))
	y.b.out <- rbind(y.b.out, t(ranef(ent.scale[[i]])$year))
	s.se.out<-rbind(s.se.out, t(se.coef(ent.scale[[i]])$stname))
	y.se.out<-rbind(y.se.out, t(se.coef(ent.scale[[i]])$year))
}

state.ranef.ent <- mi.meld(q = s.b.out, se = s.se.out)
year.ranef.ent <- mi.meld(q= y.b.out, se= y.se.out)

s.b.out<-y.b.out<-NULL
s.se.out<-y.se.out<-NULL
for(i in 1:m) {
	s.b.out <- rbind(s.b.out, t(ranef(inst.scale[[i]])$stname))
	y.b.out <- rbind(y.b.out, t(ranef(inst.scale[[i]])$year))
	s.se.out<-rbind(s.se.out, t(se.coef(inst.scale[[i]])$stname))
	y.se.out<-rbind(y.se.out, t(se.coef(inst.scale[[i]])$year))
}

state.ranef.inst <- mi.meld(q = s.b.out, se = s.se.out)
year.ranef.inst <- mi.meld(q= y.b.out, se= y.se.out)

s.tab<-as.data.frame(cbind(t(state.ranef.ent$q.mi), t(state.ranef.ent$se.mi),
	t(state.ranef.inst$q.mi), t(state.ranef.inst$se.mi)))
names(s.tab)<-c("Entries", "SE", "Institutionalization", "SE")
s.table<-xtable(s.tab)
# print(s.table, type="html", file="re-state.html")

y.tab<-as.data.frame(cbind(t(year.ranef.ent$q.mi), t(year.ranef.ent$se.mi),
	t(year.ranef.inst$q.mi), t(year.ranef.inst$se.mi)))
names(y.tab)<-c("Entries", "SE", "Institutionalization", "SE")
y.table<-xtable(y.tab)
# print(y.table, type="html", file="re-year.html")

### For html file output of regression results
# t.out<-stargazer(list(ent.scale[[1]], inst.scale[[1]]), 
# 	coef=list(t(ent.results[[1]]), t(inst.results[[1]])), 
# 	se=list(t(ent.results[[2]]), t(inst.results[[2]])),
# 	out="mi-reg-main-new.html",
# 	style="asr",
# 	title="Parameter Estimates from Models of Foster Care Entries and Institutionalization",
# 	dep.var.labels=c("Entries", "Institutionalization"),
# 	covariate.labels=c("Unemployment", "Single Parent", "Food Insecurity",
#  		"Child Poverty", "Adults w/ Less than HS", "GDP per capita", "Legislative Ideology",
#  		"Crime per capita", "% Black Population", "TANF Benefit Lvls", "SNAP Enrollment",
#  		"Medicaid Enrollment","TANF Enrollment", "Welfare workers per cap",
#  		"Incarceration rate", "Death Sentences", "Police per cap", "TANFenroll*WelfWorkers",
#  		 "Intercept"))



### Robustness checks in appendix Table 1 
### Drop states with severe measurement error on parental 
### incarceration as foster care entry cause
drops<-c("NY", "IL", "WY", "ID")
fc.drop<-NULL
for(i in 1:5){
	fc.drop[[i]]<-fc.int$imputations[[i]]
	fc.drop[[i]]<-fc.drop[[i]][!(fc.drop[[i]]$stname%in%drops),]
	fc.drop[[i]]$obs_n<-1:nrow(fc.drop[[i]])
	fc.drop[[i]]$n.ent<-as.integer(fc.drop[[i]]$entered-fc.drop[[i]]$prtjail)
}

ent.rob<-list(m)
cl.rob<-list(m)

for(i in (1:m)){
	ent.rob[[i]]<-glmer(n.ent~scale(unemprt)+scale(childnot2par)+
		scale(food.insec)+scale(chpovrt)+scale(LessHS)+
		scale(gsppercap)+
		scale(ideo)+scale(crime.pc)+scale(pctblk)+
		scale(tanf.adeq)+scale(snap.incl)+
		scale(medicaid.incl)+
		scale(tanf.incl)*scale(welfare.pc)+
		scale(incarrt)+
		scale(death.rt)+
		scale(police.pc)+
		(1|stname)+(1|year)+(1|obs_n), offset=log(child),  
		data=fc.drop[[i]],
		family=poisson)

	cl.rob[[i]]<-glmer(cl~scale(unemprt)+scale(childnot2par)+
		scale(food.insec)+scale(chpovrt)+scale(LessHS)+
		scale(gsppercap)+
		scale(ideo)+scale(crime.pc)+scale(pctblk)+
		scale(tanf.adeq)+scale(snap.incl)+
		scale(medicaid.incl)+
		scale(tanf.incl)*scale(welfare.pc)+
		scale(incarrt)+
		scale(death.rt)+
		scale(police.pc)+
		(1|stname)+(1|year)+(1|obs_n), offset=log(child),  
		data=fc.drop[[i]],
		family=poisson)
}

b.out.3<-b.out.4<-NULL
se.out.3<-se.out.4<-NULL
for(i in 1:m) {
	b.out.3 <- rbind(b.out.3, fixef(ent.rob[[i]]))
	b.out.4 <- rbind(b.out.4, fixef(cl.rob[[i]]))
	se.out.3<-rbind(se.out.3, se.coef(ent.rob[[i]])$fixef)
	se.out.4<-rbind(se.out.4, se.coef(cl.rob[[i]])$fixef)
}


ent.rob.results <- mi.meld(q = b.out.3, se = se.out.4)
cl.results <- mi.meld(q=b.out.4, se=se.out.4)

### compute intraclass correlations for states and years
### as var(ranef)/(\sum var(ranefs) + var(residual))
# icc.s.ent<-icc.y.ent<-icc.s.inst<-icc.y.inst<-NULL
# for(i in 1:m){
# 	v.RE.st<-summary(ent.scale[[i]])$varcor$stname[1]
# 	v.RE.y<-summary(ent.scale[[i]])$varcor$year[1]
# 	v.RE.i<-summary(ent.scale[[i]])$varcor$obs_n[1]
# 	v.res<-var(resid(ent.scale[[i]]))
# 	icc.s.ent[i]<-v.RE.st/(v.RE.st+v.RE.y+v.RE.i+v.res)
# 	icc.y.ent[i]<-v.RE.y/(v.RE.st+v.RE.y+v.RE.i+v.res)
# }
# 
# for(i in 1:m){
# 	v.RE.st<-summary(inst.scale[[i]])$varcor$stname[1]
# 	v.RE.y<-summary(inst.scale[[i]])$varcor$year[1]
# 	v.RE.i<-summary(inst.scale[[i]])$varcor$obs_n[1]
# 	v.res<-var(resid(inst.scale[[i]]))
# 	icc.s.inst[i]<-v.RE.st/(v.RE.st+v.RE.y+v.RE.i+v.res)
# 	icc.y.inst[i]<-v.RE.y/(v.RE.st+v.RE.y+v.RE.i+v.res)
# }


### For html file output of results
# stargazer(list(ent.rob[[1]], cl.rob[[1]]), 
# 	coef=list(t(ent.rob.results[[1]]), t(cl.results[[1]])), 
# 	se=list(t(ent.rob.results[[2]]), t(cl.results[[2]])),
# 	out="mi-reg-rob.html",
# 	style="asr",
# 	title="Foster Care Entries and Institutionalization",
# 	dep.var.labels=c("Entries not attributed to parental incarceration", 
# 		"Caseload size"),
# 	covariate.labels=c("Unemployment", "Single Parent", "Food Insecurity",
#  		"Child Poverty", "Adults w/ Less than HS", "GDP per capita", "Legislative Ideology",
#  		"Crime per capita", "% Black Population", "TANF Benefit Lvls", "SNAP Enrollment",
#  		"Medicaid Enrollment","TANF Enrollment", "Welfare workers per cap",
#  		"Incarceration rate", "Death Sentences", "Police per cap", "TANFenroll*WelfWorkers",
#  		 "Intercept"))

#### another robustness check - longitudinal model with national time trend, state-level random slopes
# inst.long<-glmer(pl.inst~scale(unemprt)+scale(childnot2par)+
#                         scale(food.insec)+scale(chpovrt)+scale(LessHS)+
#                         scale(gsppercap)+
#                         scale(ideo)+scale(crime.pc)+scale(pctblk)+
#                         scale(tanf.adeq)+scale(snap.incl)+
#                         scale(medicaid.incl)+
#                         scale(tanf.incl)*scale(welfare.pc)+
#                         scale(incarrt)+
#                         scale(death.rt)+
#                         scale(police.pc)+I(year-2002)+
#                         (I(year-2002)|stname)+(1|obs_n), offset=log(cl),  
#                       data=fc.int$imputations[[1]],
#                       family=poisson)

