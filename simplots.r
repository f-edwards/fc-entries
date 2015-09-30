### Produce and plot counterfactual simulations using model results from FILENAME
### Depends on packages tile and simcf, not available on CRAN, but available from 
### Chris Adolph, Dept of Political Science, Univ of Washginton
### http://faculty.washington.edu/cadolph/?page=60

library(tile)
library(simcf)
library(RColorBrewer)

### Depends on model output from FCmodels.r
# source("FCmodels.r")

col <- brewer.pal(4,"Dark2")

sims<-10000

### remove entered, set constant=1
sim.lmer<-function(hyp, model){
hyp<-as.matrix(hyp)
hyp[,1]<-1

ci<-0.95

pe<-fixef(model) ## Extract FE Estimates
vc<-vcov(model) ### Estimate covar matrix
simbetas<-mvrnorm(sims, pe, vc) ### Simulate Betas from multivar normal

#### To get expected values (counts of entries), need to add log(offset) to 
#### right hand side, otherwise, getting rates per child pop
res<-list()
for(i in 1:sims){
	 simmu<-simbetas%*%hyp[i,] 
	 simy <- exp(simmu)
     simy <- sort(simy)
     res$pe <- c(res$pe, mean(simy))
     length.simy <- length(simy)
     low <- up <- NULL
        for (k in 1:length(ci)) {
            low <- c(low, simy[trunc((1 - ci[k])/2 * length.simy)])
            up <- c(up, simy[trunc((1 - (1 - ci[k])/2) * length.simy)])
        }
        res$lower <- rbind(res$low, low)
        res$upper <- rbind(res$up, up)
}

return(res)
}

sim.pe<-function(hyp, model){
hyp<-as.matrix(hyp)
hyp[,1]<-1

ci<-0.95

pe<-fixef(model) ## Extract FE Estimates
vc<-vcov(model) ### Estimate covar matrix
simbetas<-mvrnorm(sims, pe, vc) ### Simulate Betas from multivar normal

#### To get expected values (counts of entries), need to add log(offset) to 
#### right hand side, otherwise, getting rates per child pop
res<-list()
	simmu<-simbetas%*%hyp[1,] 
simy <- exp(simmu)
simy <- sort(simy)
res$pe <- c(res$pe, mean(simy))
length.simy <- length(simy)
low <- up <- NULL
for (k in 1:length(ci)) {
    low <- c(low, simy[trunc((1 - ci[k])/2 * length.simy)])
    up <- c(up, simy[trunc((1 - (1 - ci[k])/2) * length.simy)])
}
res$lower <- rbind(res$low, low)
res$upper <- rbind(res$up, up)

return(res)
}

### Duplicate formula object for model of interest

simdata<-extractdata(ent.scale[[1]], fc.int$imputations[[1]], na.rm=TRUE)
simdata<-simdata[,-(19:21)] ### Drop identifiers, leave only covars

sims<-10000

tanf.adeq.seq<-seq(min(scale(simdata$tanf.adeq)), max(scale(simdata$tanf.adeq)), length.out=sims)
snap.incl.seq<-seq(min(scale(simdata$snap.incl)), max(scale(simdata$snap.incl)), length.out=sims)
medicaid.incl.seq<-seq(min(scale(simdata$medicaid.incl)), max(scale(simdata$medicaid.incl)), length.out=sims)
tanf.incl.seq<-seq(min(scale(simdata$tanf.incl)), max(scale(simdata$tanf.incl)), length.out=sims)
welfare.pc.seq<-seq(min(scale(simdata$welfare.pc)), max(scale(simdata$welfare.pc)), length.out=sims)
incarrt.seq<-seq(min(scale(simdata$incarrt)), max(scale(simdata$incarrt)), length.out=sims)
death.rt.seq<-seq(min(scale(simdata$death.rt)), max(scale(simdata$death.rt)), length.out=sims)
police.pc.seq<-seq(min(scale(simdata$police.pc)), max(scale(simdata$police.pc)), length.out=sims)


n.s.t<-length(tanf.adeq.seq)
n.s.i<-length(incarrt.seq)

wel.low.bur<-cfMake(ent.scale[[1]], simdata, nscen=n.s.t)
wel.high.bur<-cfMake(ent.scale[[1]], simdata, nscen=n.s.t)
wel.gen<-cfMake(ent.scale[[1]], simdata, nscen=n.s.t)
ihyp<-cfMake(ent.scale[[1]], simdata, nscen=n.s.i)

est.1<-cfMake(ent.scale[[1]], simdata, nscen=n.s.t)

est.1<-cfChange(est.1, "(Intercept)", x=1, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(unemprt)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(childnot2par)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(food.insec)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(chpovrt)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(LessHS)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(gsppercap)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(ideo)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(crime.pc)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(pctblk)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(tanf.adeq)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(snap.incl)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(medicaid.incl)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(tanf.incl)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(welfare.pc)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(incarrt)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(death.rt)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(police.pc)", x=0, scen=(1:n.s.t))
est.1<-cfChange(est.1, "scale(tanf.incl):scale(welfare.pc)", x=0, scen=(1:n.s.t))


mean.e<-sim.pe(est.1$x, ent.scale[[1]])
### high welf scen for point est - fc entry
est.hwe<-cfChange(est.1, "scale(tanf.adeq)", x=1, scen=(1:n.s.t))
est.hwe<-cfChange(est.hwe, "scale(snap.incl)", x=1, scen=(1:n.s.t))
est.hwe<-cfChange(est.hwe, "scale(medicaid.incl)", x=1, scen=(1:n.s.t))
est.hwe<-cfChange(est.hwe, "scale(tanf.incl)", x=1, scen=(1:n.s.t))

hwe<-sim.pe(est.hwe$x, ent.scale[[1]])

### low welf scen
est.lwe<-cfChange(est.1, "scale(tanf.adeq)", x=-1, scen=(1:n.s.t))
est.lwe<-cfChange(est.lwe, "scale(snap.incl)", x=-1, scen=(1:n.s.t))
est.lwe<-cfChange(est.lwe, "scale(medicaid.incl)", x=-1, scen=(1:n.s.t))
est.lwe<-cfChange(est.lwe, "scale(tanf.incl)", x=-1, scen=(1:n.s.t))

lwe<-sim.pe(est.lwe$x, ent.scale[[1]])

### high pun scen - fc entry
est.hpe<-cfChange(est.1, "scale(incarrt)", x=1, scen=(1:n.s.t))
est.hpe<-cfChange(est.hpe, "scale(death.rt)", x=1, scen=(1:n.s.t))
est.hpe<-cfChange(est.hpe, "scale(police.pc)", x=1, scen=(1:n.s.t))
hpe<-sim.pe(est.hpe$x, ent.scale[[1]])

### low pun scen
est.lpe<-cfChange(est.1, "scale(incarrt)", x=-1, scen=(1:n.s.t))
est.lpe<-cfChange(est.lpe, "scale(death.rt)", x=-1, scen=(1:n.s.t))
est.lpe<-cfChange(est.lpe, "scale(police.pc)", x=-1, scen=(1:n.s.t))
lpe<-sim.pe(est.lpe$x, ent.scale[[1]])

mean.i<-sim.pe(est.1$x, inst.scale[[1]])
### high welf scen - inst
est.hwi<-cfChange(est.1, "scale(tanf.adeq)", x=1, scen=(1:n.s.t))
est.hwi<-cfChange(est.hwi, "scale(snap.incl)", x=1, scen=(1:n.s.t))
est.hwi<-cfChange(est.hwi, "scale(medicaid.incl)", x=1, scen=(1:n.s.t))
est.hwi<-cfChange(est.hwi, "scale(tanf.incl)", x=1, scen=(1:n.s.t))
est.hwi<-cfChange(est.hwi, "scale(welfare.pc)", x=1, scen=(1:n.s.t))
est.hwi<-cfChange(est.hwi, "scale(tanf.incl):scale(welfare.pc)", x=1, scen=(1:n.s.t))
hwi<-sim.pe(est.hwi$x, inst.scale[[1]])

### high pun scen - inst
est.hpi<-cfChange(est.1, "scale(incarrt)", x=1)
est.hpi<-cfChange(est.hpi, "scale(death.rt)", x=1)
est.hpi<-cfChange(est.hpi, "scale(police.pc)", x=1)
hpi<-sim.pe(est.hpi$x, inst.scale[[1]])


for (i in 1:n.s.t){

	wel.low.bur<-cfChange(wel.low.bur, "(Intercept)", x=1, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(unemprt)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(childnot2par)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(food.insec)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(chpovrt)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(LessHS)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(gsppercap)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(ideo)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(crime.pc)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(pctblk)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(tanf.adeq)", x=-1, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(snap.incl)", x=-1, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(medicaid.incl)", x=-1, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(tanf.incl)", x=-1, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(welfare.pc)", x=welfare.pc.seq[i], scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(incarrt)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(death.rt)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(police.pc)", x=0, scen=i)
	wel.low.bur<-cfChange(wel.low.bur, "scale(tanf.incl):scale(welfare.pc)", x=welfare.pc.seq[i]*(-1), scen=i)

}

for (i in 1:n.s.t){
	
	wel.high.bur<-cfChange(wel.high.bur, "(Intercept)", x=1, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(unemprt)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(childnot2par)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(food.insec)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(chpovrt)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(LessHS)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(gsppercap)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(ideo)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(crime.pc)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(pctblk)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(tanf.adeq)", x=1, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(snap.incl)", x=1, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(medicaid.incl)", x=1, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(tanf.incl)", x=1, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(welfare.pc)", x=welfare.pc.seq[i], scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(incarrt)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(death.rt)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(police.pc)", x=0, scen=i)
	wel.high.bur<-cfChange(wel.high.bur, "scale(tanf.incl):scale(welfare.pc)", x=1*welfare.pc.seq[i], scen=i)

}


for (i in 1:n.s.t){
	
	wel.gen<-cfChange(wel.gen, "(Intercept)", x=1, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(unemprt)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(childnot2par)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(food.insec)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(chpovrt)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(LessHS)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(gsppercap)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(ideo)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(crime.pc)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(pctblk)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(tanf.adeq)", x=tanf.adeq.seq[i], scen=i)
	wel.gen<-cfChange(wel.gen, "scale(snap.incl)", x=snap.incl.seq[i], scen=i)
	wel.gen<-cfChange(wel.gen, "scale(medicaid.incl)", x=medicaid.incl.seq[i], scen=i)
	wel.gen<-cfChange(wel.gen, "scale(tanf.incl)", x=tanf.incl.seq[i], scen=i)
	wel.gen<-cfChange(wel.gen, "scale(welfare.pc)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(incarrt)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(death.rt)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(police.pc)", x=0, scen=i)
	wel.gen<-cfChange(wel.gen, "scale(tanf.incl):scale(welfare.pc)", x=0, scen=i)

}



for (i in 1:n.s.t){
	
	ihyp<-cfChange(ihyp, "(Intercept)", x=1, scen=i)
	ihyp<-cfChange(ihyp, "scale(unemprt)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(childnot2par)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(food.insec)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(chpovrt)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(LessHS)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(gsppercap)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(ideo)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(crime.pc)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(pctblk)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(tanf.adeq)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(snap.incl)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(medicaid.incl)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(tanf.incl)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(welfare.pc)", x=0, scen=i)
	ihyp<-cfChange(ihyp, "scale(incarrt)", x=incarrt.seq[i], scen=i)
	ihyp<-cfChange(ihyp, "scale(death.rt)", x=death.rt.seq[i], scen=i)
	ihyp<-cfChange(ihyp, "scale(police.pc)", x=police.pc.seq[i], scen=i)
	ihyp<-cfChange(ihyp, "scale(tanf.incl):scale(welfare.pc)", x=0, scen=i)

}

wel.inst<-wel.gen
for(i in 1:n.s.t){
	wel.inst<-cfChange(wel.inst, "scale(welfare.pc)", x=welfare.pc.seq[i], scen=i)
	wel.inst<-cfChange(wel.inst, "scale(tanf.incl):scale(welfare.pc)", x=tanf.incl.seq[i]*welfare.pc.seq[i], scen=i)

}



### RUN SIMULATIONS

wel.low.sim<-sim.lmer(wel.low.bur$x, ent.scale[[1]])
wel.high.sim<-sim.lmer(wel.high.bur$x, ent.scale[[1]])
wel.gen.sim<-sim.lmer(wel.gen$x, ent.scale[[1]])
pun.sim<-sim.lmer(ihyp$x, ent.scale[[1]])

wel.inst.sim<-sim.lmer(wel.inst$x, inst.scale[[1]])
pun.inst.sim<-sim.lmer(ihyp$x, inst.scale[[1]])

### PREPARE FOR PLOTTING
### PAPER PLOTS
trace1<-lineplot(x=welfare.pc.seq, y=wel.low.sim$pe, 
	lower=wel.low.sim$lower, upper=wel.low.sim$upper, 
	col=col[1], plot=3)

trace2<-lineplot(x=welfare.pc.seq, y=wel.high.sim$pe,
	lower=wel.high.sim$lower, upper=wel.high.sim$upper, 
	col=col[1], plot=4)

trace3<-lineplot(x=tanf.incl.seq, y=wel.gen.sim$pe, 
	lower=wel.gen.sim$lower, upper=wel.gen.sim$upper, 
	col=col[1], plot=1)

trace4<-lineplot(x=incarrt.seq, y=pun.sim$pe, 
	lower=pun.sim$lower, upper=pun.sim$upper, 
	col=col[1], plot=2)

trace5<-lineplot(x=snap.incl.seq, y=wel.inst.sim$pe, 
	lower=wel.inst.sim$lower, upper=wel.inst.sim$upper, 
	col=col[1], plot=5)

trace6<-lineplot(x=incarrt.seq, y=pun.inst.sim$pe, 
	lower=pun.inst.sim$lower, upper=pun.inst.sim$upper, 
	col=col[1], plot=6)


tile(trace1,trace2, trace3, trace4,trace5, trace6,
	yaxis=list(at1=c(0, 0.002,0.004,  0.006, 0.008, 0.01), 
		labels1=c(0, 2, 4, 6, 8, 10),
		at2=c(0, .002,0.004,  0.006, 0.008, 0.01), 
		labels2=c(0, 2, 4, 6, 8, 10),
		at3=c(0, 0.002,0.004,  0.006, 0.008, 0.01), 
		labels3=c(0, 2, 4, 6, 8, 10),
		at4=c(0, 0.002,0.004,  0.006, 0.008, 0.01), 
		labels4=c(0, 2, 4, 6, 8, 10),
		at5=c(0,0.03,0.06, 0.09, 0.12, 0.15, 0.18), 
		labels5=c(0, 30, 60, 90, 120, 150, 180),
		at6=c(0,0.03,0.06, 0.09, 0.12, 0.15, 0.18), 
		labels6=c(0, 30, 60, 90, 120, 150, 180)
		),
	xaxis=list(at3=c(-2,-1,0,1,2),
		at4=c(-2,-1,0,1,2),
		at1=c(-1,0,1,2,3),
		at2=c(-1,0,1,2,3),
		at5=c(-1,0,1,2,3),
		at6=c(-1,0,1,2,3),
		labels3=c("-2sd", "-1", "Mean", "+1", "+2sd"),
		labels4=c("-2sd", "-1", "Mean", "+1", "+2sd"),
		labels1=c("-1sd", "Mean", "+1", "+2", "+3sd"),
		labels2=c("-1sd", "Mean", "+1", "+2", "+3sd"),
		labels5=c("-1sd", "Mean", "+1", "+2", "+3sd"),
		labels6=c("-1sd", "Mean", "+1", "+2", "+3sd")),
	xaxistitle=list(labels3="Welfare staff per cap (low generosity)",
		labels4="Welfare staff per cap (high generosity)",
		labels1="Welfare generosity",
		labels2="Punitiveness",
		labels5="Welfare generosity",
		labels6="Punitiveness"),
	yaxistitle=list(labels1="Foster care entry rate",
		labels2="",
		labels3="Foster care entry rate",
		labels4="",
		labels5="FC Institutionalization rate",
		labels6=""),
	plottitle=list(labels=c("(1)",
		"(2)",
		"(3)",
		"(4)",
		"(5)",
		"(6)")),
	RxC=c(3,2),
	output=list(file="entsim", type="png", width=7, height=7)
	)

