# Get subset data
plotData = subset(plotData, SITE_COUNT == siteFilter)
# Convert date text field into date data type (example input data date format is YYYY-MM-DD)
plotData$useDate <- as.Date(as.character(plotData$useDate),format="%Y-%m-%d")
# Compute date range to use in plot
dateRange = c(min(plotData$useDate, na.rm=TRUE), max(plotData$useDate, na.rm=TRUE))
# Pick data to plot
grpName = "T01J750"
py = plotData$testerHour[plotData$testerName == grpName]
py5 = plotData$testerHour512[plotData$testerName == grpName]
px = plotData$useDate[plotData$testerName == grpName]
# plot x,y data
plot( px, py, xlim=dateRange, ylim=yRange, main=plotTitle, type="h", lwd=3, ylab="Used Hours", xlab="", col="blue", xaxt="n")
# Setup plot engine to overlay next plot (else it will create a new plot)
par(new=T)
plot( px, py5, xlim=dateRange, ylim=yRange, type="h", lwd=3, col="red", xaxt="n", ylab="")
axis.Date(1, at=seq(dateRange[1], dateRange[2], by="day"), format="%a %d-%m", las=2)
