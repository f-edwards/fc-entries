# fc-entries
This project contains the code necessary to perform the analyses reported in "Saving Children and Controling Families: Punishment Redistribution and Child Protection", forthcoming in the American Sociological Review.  This repository transforms individual-year level data from the Adoption and Foster Care Analysis and Reporting System (AFCARS) into state-year panel data that measure variation in state foster care systems between years.  It also contains scripts to conduct the analyses and visualizations reported in the paper.

You must run CensusTransform.r and FC_AFCARS_indTOstate.r before beginning the analysis to produce state-year panel .csv files.  The main analysis is produced using FCmodels.r.  To produce maps and simulation plots, run FCMapsFacet.r and simplots.r in the same R session in which you executed FCmodels.r.

## Data sources
The data used for this study are freely available to the research community, but may require data use agreements with archives to obtain them.

This analysis depends on a number of data sources.  First, all foster care data are provided by the [National Data Archive on Child Abuse and Neglect] (http://www.ndacan.cornell.edu/), and are available for academic research by request. 

Population data can be obtained from [IPUMS-USA] (https://usa.ipums.org/usa/).  The file used in this analysis contained the following samples: [2000 1%, 2001 ACS, 2002 ACS, 2003 ACS, 2004 ACS, 2005 ACS, 2006 ACS, 2007 ACS 3yr, 2008 ACS 3yr, 2009 ACS 3yr, 2010 ACS 3yr, 2011 ACS 3yr] and the following variables: [YEAR, DATANUM, SERIAL, HHWT, STATEFIP, GQ, RENTGRS,
PERNUM, PERWT, NCHILD, MOMLOC, POPLOC, STEPPOP, RELATE, RELATED, SEX, AGE, RACE, RACED, HISPAN,HISPAND, EDUC, EDUCD, EMPSTAT, EMPSTATD, LABFORCE, POVERTY, GCHOUSE]

Political ideology data (from Berry, Rinquist, Fording, and Hanson 1998) can be obtained from [Richard Fording] (https://rcfording.wordpress.com/state-ideology-data/), posted as "Updated Measures of Citizen and Government Ideology" on 10/14/15.

The National Prisoner Statistics are available to researchers through [ICPSR] (http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34540), as are the [FBI's Uniform Crime Reports](http://www.icpsr.umich.edu/icpsrweb/ICPSR/series/57/studies?sortBy=7&archive=ICPSR&q=allocated+state&searchSource=revise). 

Data on social welfare policy can be obtained from the [University of Kentucky Center for Poverty Research](http://www.ukcpr.org/data).

Data on state and local government payrolls are obtainable from the [U.S. Census Government Employment and Payroll series] (https://www.census.gov/govs/apes/).

Death sentences issued by state courts are compiled by [The Death Penalty Information Center](http://www.deathpenaltyinfo.org/death-sentences-united-states-1977-2008) from Bureau of Justice annual reports. 

Lastly, data on regional price parity by state can be obtained from the [Bureau of Economic Analysis](http://www.bea.gov/iTable/iTableHtml.cfm?reqid=70&step=30&isuri=1&7022=101&7023=8&7024=non-industry&7033=-1&7025=0&7026=xx&7027=-1&7001=8101&7028=1&7031=0&7040=-1&7083=levels&7029=101&7090=70).

I'm happy to advise on how to obtain these data if you have any difficulties. FEdwards@uw.edu
