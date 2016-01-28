library(RColorBrewer)
library(ggplot2)
library(maps)

### Depends on transformed data from FCmodels.r
#source("FCmodels.r")

returnquant<-function(x){
	l<-5 ### number of quantiles
	temp<-cut_number(x,l)
	quant<-rep(NA, length(x))
	for(i in (1:l)){
		z<-which(levels(temp)[i]==temp)
		quant[z]<-i
		}
		return(as.factor(quant))
}

fc.count<-fc.int$imputations[[1]]

fcmeandes<-data.frame(
	"state"=aggregate((entered/child)*1000~stname, data=fc.count, FUN=mean)[[1]],
	"entrt"=aggregate((entered/child)*1000~state, data=fc.count, FUN=mean)[[2]],
	"clrt"=aggregate((cl/child)*1000~state, data=fc.count, FUN=mean)[[2]], 
	"inst"=aggregate((pl.inst/cl)*1000~state, data=fc.count, FUN=mean)[[2]],
	"hunger"=aggregate((food.insec)*1000~state, data=fc.count, FUN=mean)[[2]], 
	"chpov"=aggregate((chpovrt)*1000~state, data=fc.count, FUN=mean)[[2]],
	"LessHS"=aggregate((LessHS)*1000~state, data=fc.count, FUN=mean)[[2]], 
	"unemp"=aggregate((unemprt)*1000~state, data=fc.count, FUN=mean)[[2]], 
	"childnot2par"=aggregate((childnot2par)*1000~state, data=fc.count, FUN=mean)[[2]],
	"gsppercap"=aggregate((gsppercap)~state, data=fc.count, FUN=mean)[[2]],
	"ideo"=aggregate(ideo~state, data=fc.count, FUN=mean)[[2]], 
	"pctblk"=aggregate(pctblk~state, data=fc.count, FUN=mean)[[2]],
	"crime.pc"=aggregate((crime.pc)*1000~state, data=fc.count, FUN=mean)[[2]],
	"incarrt"=aggregate((incarrt)*1000~state, data=fc.count, FUN=mean)[[2]],
	"police.pc"=aggregate((police.pc)*1000~state, data=fc.count, FUN=mean)[[2]],
	"welfare.pc"=aggregate((welfare.pc)*1000~state, data=fc.count, FUN=mean)[[2]],
	"TANF"=aggregate(tanf.adeq~state, data=fc.count, FUN=mean)[[2]],
	"pun"=aggregate(incarrt~state, data=fc.count, FUN=mean)[[2]])

fc10<-fcmeandes
fc10$bentrt<-aggregate((bent/blkchild)*1000~stname, data=fc.count, FUN=mean)[[2]]
fc10$state<-aggregate(entered~state, FUN=sum, data=fc.count)[[1]]
fc10$statename<-aggregate(entered~statename, FUN=sum, data=fc.count)[[1]]
fipsconvert<-data.frame("state"=fc10$state, "names"=fc10$statename)

states<-map_data("state")
fipsconvert$region<-tolower(fipsconvert[,2])

n<-nrow(fc10)
fclong<-with(fc10, data.frame(region=rep(tolower(fc10$statename), 6),
	q=as.factor(c(returnquant(entrt), returnquant(inst), returnquant(TANF), returnquant(pun), returnquant(welfare.pc), returnquant(police.pc))),
	c=c(rep("Foster Care Entries per Child Population", n), rep("Percent of Foster Care Caseload Institutionalized", n), 
		rep("TANF Benefits (adjusted)", n), rep("Incarceration rate", n),
		rep("Welfare workers per capita", n),
		rep("Police per capita", n))
	))
fclong$c<-factor(fclong$c, levels=c("Foster Care Entries per Child Population", 
	"Percent of Foster Care Caseload Institutionalized", "TANF Benefits (adjusted)",
	"Incarceration rate", "Welfare workers per capita", "Police per capita"))
choro<-merge(states, fclong, by="region")
choro <- choro[order(choro$order), ]

MapPlot <- ggplot(choro,
 aes(x = long, y = lat, group = group, fill = q))
MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "gray20", size = 0.01) +
	scale_fill_brewer(palette = "Blues",
		name="Average\nState Value\n2002-2011", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))

MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.

MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
		panel.border = element_blank(), panel.background=element_blank())+
	scale_y_continuous(name="", breaks=NULL)+
	scale_x_continuous(name="", breaks=NULL)+
	theme(legend.title=element_text(size=10))+
	theme(legend.text=element_text(size=10))+
	theme(legend.position="bottom")+
	theme(legend.key.size= unit(0.3, "cm"))

MapPlot<- MapPlot + theme(strip.background=element_blank(), 
	strip.text.x=element_text(size=10),
	strip.text.y=element_blank())

MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)

MapPlot <- MapPlot + facet_wrap(~c, ncol=2)

print(MapPlot)

ggsave(plot = MapPlot, "FacetMap.png", h = 8, w = 8)

### FOR 1X2 ENTRY/POV MAPS FOR TALK

n<-nrow(fc10)
fclong<-with(fc10, data.frame(region=rep(tolower(fc10$statename), 2),
                              q=as.factor(c(returnquant(entrt), returnquant(chpov))),
                              c=c(rep("Foster Care Entries per Child Population", n), rep("Child Poverty Rate", n))
))
fclong$c<-factor(fclong$c, levels=c("Foster Care Entries per Child Population", 
                                    "Child Poverty Rate"))
choro<-merge(states, fclong, by="region")
choro <- choro[order(choro$order), ]

MapPlot <- ggplot(choro,
                  aes(x = long, y = lat, group = group, fill = q))
MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "gray20", size = 0.1) +
  scale_fill_brewer(palette = "Blues",
                    name="Average\nState Value\n2002-2011", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))

MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.

MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL)+
  scale_x_continuous(name="", breaks=NULL)+
  theme(legend.title=element_text(size=10))+
  theme(legend.text=element_text(size=10))+
  theme(legend.position="bottom")+
  theme(legend.key.size= unit(0.3, "cm"))

MapPlot<- MapPlot + theme(strip.background=element_blank(), 
                          strip.text.x=element_text(size=10),
                          strip.text.y=element_blank())

MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)

MapPlot <- MapPlot + facet_wrap(~c, ncol=2)

print(MapPlot)

ggsave(plot = MapPlot, "PovEntMap.pdf", h = 6, w = 8)

### For solo map of entry rates

n<-nrow(fc10)
fclong<-with(fc10, data.frame(region=rep(tolower(fc10$statename), 1),
                              q=as.factor(c(returnquant(entrt))),
                              c=c(rep("Foster Care Entries per Child Population", n))
))
fclong$c<-factor(fclong$c, levels=c("Foster Care Entries per Child Population"))
choro<-merge(states, fclong, by="region")
choro <- choro[order(choro$order), ]

MapPlot <- ggplot(choro,
                  aes(x = long, y = lat, group = group, fill = q))
MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "gray20", size = 0.1) +
  scale_fill_brewer(palette = "Blues",
                    name="Average Entries\nper 1,000 Children\n2002-2011", labels=c("1.7-2.9", "2.9-4.0 ", "4.0-4.8 ", "4.8-5.8 ", "5.8-8.7"))

MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.

MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL)+
  scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
  theme(legend.position="right")
  # theme(legend.key.size= unit(0.3, "cm"))

MapPlot<- MapPlot + theme(strip.background=element_blank(), 
                          strip.text.x=element_text(size=10),
                          strip.text.y=element_blank())

MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)

print(MapPlot)

ggsave(plot = MapPlot, "EntMap.pdf", h = 6, w = 8)


