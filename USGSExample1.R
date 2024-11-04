############################################

# load waterData package
library(waterData)

##################################
#importDVs function can be used to download data
#first choose which parameter to download
#USGS parameter code for streamflow is 00060
#depending on gage, user can download daily: mean, maximum, minimum, or median
#daily mean is usually statistics code 00003
#possible parameter codes can be found at http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes
#possible statistics codes can be found at http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table
#start date is indicated with sdate (if none specified,this is auto set to 1851-01-01)
#end date is indicated with edate (if none specified,this is auto set to current system date)

#import data
q05054000<-importDVs("05054000", code="00060", stat="00003", sdate="2000-01-01", edate="2010-12-31")

#view a subset of the data
#return first 6 rows for new data set to view a subset of the data

#staid is station ID. val is value. dates. qualcode is???
head(q05054000)

#head funciton checks to ensure station id is at least 8 characters long
#if no data are returned, possible issues: date syntax, station number, etc.
#if there are errors, can get the url with tellMeURL
my.URL<-tellMeURL("05054000", code="00060", stat="00003", sdate="2000-01-01", edate="2010-12-31")

#plot the data
#potential problems to check for: gaps in data collection, anomalous data values
data(exampleWaterData, package="waterData")
my.plot<-plotParam(badDataSet)
print(my.plot)

#######################################
#data cleanup
#cleanUp function identifies negative and zero values
> cleanUp(badDataSet, task="view")

#qualification codes we don't want: 
    #qualification codes of P" (provisional data subject to revision)
    #Ice (meaning the streamgage was aected by ice on this date)
    #and e (value has been estimated)
    #change to NA to indicate missing data
    #use only qualification code A: (approved for publication, processing and review completed))

#removing negative values or 0s
#replace with 0.1 or 0.01
#Streamflow anomalies are calculated using the base-10 logarithm. 
#therefore negative and zero values will cause errors and warnings. 
#To change the values, set the task argument to fix" as in the following example. 

q05054000Fix<-cleanUp(badDataSet, task="fix", replace=0.1)

my.plot<-plotParam(q05054000Fix, code="00060", stat="00003", logscale=TRUE)
print(my.plot)

#NA and 0 values have now been replaced with 0.1
#however, this is too low of a value
#zero values can be replaced with NA
#fillMiss can be used
q05054000Fix<-cleanUp(badDataSet, task="fix", replace=10)
my.plot<-plotParam(q05054000Fix, code="00060", stat="00003", logscale=TRUE)
print(my.plot)

#fillMIss estimates values to replace NAs
#if there are long periods missing values it might not be best to use
#if less than user-specified percent is missing (default 40%) and largest block is less than user # days (def 30)
#structTS will be used to fill in data
#tsSmooth is used to smooth time series

summary(misQ05054000)
my.newdata <- fillMiss(misQ05054000, block=30, pmiss=50, model="trend",  smooth=TRUE, log="y")

#####################################
#data summary
library(xtable)
#the following table gives summary statistics for daily streamflow series
my.xtable<-xtable(summaryStats(q05054000Fix, staid="05054000"), cap="Summary statistics for daily streamflow series. Begin, the beginning date for the series; End, the ending date for the series; n, the number of observations; NA, the number of missing values; Neg, the number of negative values; Min, the minimum value; Q1, the first quartile or 25th percentile; Med, the median value; Mean, the mean value; Q3, the third quartile or 75th percentile; Max, the maximum value; StdDev, the standard deviation; IQR, the interquartile range.")
print.xtable(my.xtable, size=c("scriptsize"))
#Figure 4: Plot of observed data, denoted by the black line in background, with data filled in for missing values, denoted by the green line in the foreground.

#transform hydrologic data with algorithms to approximate normality
par(cex.lab=.9, las=1, tcl=0.5, xaxs="r", yaxs="r", cex.axis=0.8)
qqnorm(q05054000Fix$val)
qqline(q05054000Fix$val)
# Figure 5: Quantile-quantile plot of stream fl ow to check for approximate normality. 
#Stream fow is not log transformed and it is not approximated well by a normal distribution.

par(cex.lab=.9, las=1, tcl=0.5, xaxs="r", yaxs="r", cex.axis=0.8)
qqnorm(log10(q05054000Fix$val))
qqline(log10(q05054000Fix$val))
#Figure 6: Quantile-quantile plot of the base-10 logarithm of stream ow to check for approximate normality. 
#This shows that log-transformed stream ow is approximately normally distributed, with some low outliers.

###############################
#site information
#siteInfo function 
my.sites <- c("05054000","05082500","05061000","05050000","05058700", "05267000", "06342500", "06478000", "06414000")
my.siteInfo <- siteInfo(my.sites)
xtable(my.siteInfo[order(my.siteInfo$staid),], cap="Information for select U.S. Geological Survey streamgage sites, sorted in downstream order.")
#this gives lat long info that can be used to plot one or more sites on a map

##################################
#making a map
library(maps)
library(mapdata)
par(las=1,tck=0.02,mar=c(0,0,0,0))
map('state', region=c('minnesota', '.*dakota'))
map('rivers',add=TRUE,col=4)
# label centered over gage site, jitter added to differentiate sites close together
mindif<-0
maxiterations<-30
iteration<-1
while (mindif<0.085) {
    y.offset<-as.numeric(my.siteInfo$lat)+runif(length(my.siteInfo$lat), 0.12,0.45)
    mindif <- min(diff(unique(sort.int(round(as.numeric(y.offset),digits=3))))) 
    iteration<-iteration + 1 
    if ( iteration >= maxiterations ) {
        mindif<-0.09c 
        message("No ideal jitter found. Some labels may conflict")
     }
    }
points(my.siteInfo$lng, my.siteInfo$lat, pch=19, col="green")
text(xy.coords(my.siteInfo$lng,y.offset),my.siteInfo$staid,cex=0.55)
box()
map.axes()

#can also print site url
siteInfoURL<-tellMeSiteURL("05054000")

#####################################
#anomalies 
#The time scales available in the package include a set of anomalies with the 1-year, 30-day, and 1-day time scales; 
#a set with the 100-day, 10-day, and 1-day time scales; 
#a set with the 30-day and 1-day time scales; 
#and a set with the 10-year, 5-year, 1-year, one-quarter-year (seasonal), 
#and 1-day time scales.
