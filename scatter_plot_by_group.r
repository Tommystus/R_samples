# Plot data by group symbol
# plot data has time, record number and site count, POT group
# Create grpName from data field and add it back
grpName = as.character(plotData$SITE_COUNT)
grpName = paste(grpName, plotData$POT, sep=';')
plotData$grpName = grpName
# Create main plot data 
pplotData = data.frame(yValue=plotData$idleTime,
	xValue=plotData$recNum, grpName=plotData$grpName,
	class=array(length(plotData$grpName)))
# Create a list of all unique group names and assign index to them by alphabetical order
grpFactor = as.factor(sort(unique(pplotData$grpName)))
# index the factor names
grpIdxList = c(grpFactor)
grpList = levels(grpFactor)
# Set class id for each group
for( grpIdx in grpIdxList)
{
	grpName = grpList[grpIdx]
	pplotData$class[pplotData$grpName == grpName] = grpIdx
}
symIdx = pplotData$class
# Plot data with each point colored by the group index.
# Make the ploting symbol a little bigger with cex parameter
plot( pplotData$xValue, pplotData$yValue, pch=symIdx, col=(symIdx+1), cex=1.2,
	main="plot title",xlab="x label", ylab="Y label")
