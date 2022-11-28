# Plot inspection wafer map using dots for dies and lines for reticle fields
#
#<INPUT name=recipeKey length=8 number label="Recipe Key">
#<INPUT name=inspTime length=8 optional label="Insp Time">
#<INPUT name=waferKey length=8 optional label="Wafer Key">
#<INPUT name=inkOut length=4 optional number label="Inkout Code (0 or 199)">
#<INPUT name=rxb length=8 optional number label="Reticle X Begin">
#<INPUT name=ryb length=8 optional number label="Reticle Y Begin">
#<INPUT name=rxs length=8 optional number label="Reticle X size">
#<INPUT name=rys length=8 optional number label="Reticle Y size">
#cat(paste("ryb=",ryb," rys=",rys),"\n");
#cat(paste("recipeKey=",recipeKey ),"\n");
#
# Provided variables:
#	outFile : unique image file name to write plot data to
#
baseUrl = paste(dbqSrv, "/wmapdata/", sep='');
inspMapUrl = paste(baseUrl,"inspRegion?X=csv&P1=",recipeKey, sep='')
defMapUrl = paste(baseUrl,"defectMap?X=csv&P1=",inspTime,"&P2=",waferKey, sep='')
if (!is.na(inkOut) & (inkOut == 199))
	defMapUrl = paste(baseUrl,"inkOutMap?X=csv&P1=",inspTime,"&P2=",waferKey, sep='')
if (plotDebug > 0)
	cat(inspMapUrl,"\n");

plotData = read.csv(inspMapUrl)
if (plotDebug > 0)
	attr(plotData,"names")

try( {defMapData = read.csv(defMapUrl) },silent=TRUE)

#setup plot area
jpeg(outFile, width=720, height=720, res=72, pointsize=12)
plotTitle = paste("Inspection Pattern for Recipe", recipeKey)
if (nrow(plotData) > 0)
{
	# Do the plot
	# Plot wafermap using defined XY dies with title and XY axis labels
	plot( plotData$INDEX_X, plotData$INDEX_Y, main=plotTitle, ylab="Y Index", xlab="X Index")

	# Overlay with various items
	#  - Plot the initial reticle reference point
	if (!(is.na(rxb) | is.na(ryb)))
	{
		points(rxb, ryb, type="p", pch=15, cex=1.5, col="Purple", lwd=2)
	}
	#  - Plot the defect dies
	if (nrow(defMapData) > 0)
	{
		points( defMapData$INDEX_X, defMapData$INDEX_Y, type="p", pch=20, col="red", lwd=3)
	}
	#  - Plot the reticle fields using lines
	#    - Draw the verticle lines
	if (!(is.na(rxb) | is.na(rxs)))
	{
		xseq = seq(rxb-.5, max(plotData$INDEX_X), by=rxs)
		ay = c(min(plotData$INDEX_Y), max(plotData$INDEX_Y))
		for (i in xseq)
		{
			lines(c(i,i),ay,col="skyblue3",lwd=2)
		}
		if ((rxb -rxs -.5) > min(plotData$INDEX_X))
		{
			xseq = seq(rxb -rxs -.5, min(plotData$INDEX_X), by=-rxs)
			for (i in xseq)
			{
				lines(c(i,i),ay,col="skyblue3",lwd=2)
			}
		}
	}
	#   - Draw the horizontal lines
	if (!(is.na(ryb) | is.na(rys)))
	{
		ax = c(min(plotData$INDEX_X), max(plotData$INDEX_X))
		if ((ryb-.5) < max(plotData$INDEX_Y))
		{
			yseq = seq(ryb-.5, max(plotData$INDEX_Y), by=rys)
			for (i in yseq)
			{
				lines(ax,c(i,i),col="skyblue4",lwd=2)
			}
		}
		if ((ryb -rys -.5) > min(plotData$INDEX_Y))
		{
			yseq = seq(ryb -rys -.5, min(plotData$INDEX_Y), by=-rys)
			for (i in yseq)
			{
				lines(ax,c(i,i),col="skyblue4",lwd=2)
			}
		}
	}

	#show data link
	if (nrow(plotData) < 3000)
		inspMapUrl = sub('X=csv','',inspMapUrl,perl=T);
	cat(paste("<br><a href=", inspMapUrl, ">wafermap dies</a>",sep=''))
	defMapUrl = sub('X=csv','',defMapUrl,perl=T);
	cat(paste(", <a href=", defMapUrl, ">Defect dies</a>",sep=''))
} else
{
	cat("<br>No data found","\n")
}
#get plot attributes
#plotParm = par()
