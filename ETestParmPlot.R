#<INPUT name=lotName length=20 label="Lot Name">
#<INPUT name=testName length=40 label="Test Name starts with">
#<INPUT name=dSLim length=1 optional label="Show spec limits (Y/N)">
#<INPUT name=dTLim length=1 optional label="Show trend limits (Y/N)">
#<INPUT name=gbSite length=1 optional label="Group By Site (Y/N)">
#<INPUT name=gbWafer length=1 optional label="Group By Wafer (Y/N)">
#<CONTROL icount=99>
#cat(paste("lotName=",lotName),"\n");
#
# All Parameter PPlot for Vendor Map
#
dbHostUrl = 'http://dataRestServer/vmap/';
baseUrl = paste(dbHostUrl,'lotParmResult?',sep='')

#Note:  use special escape char (`) for oracle like query
testName = gsub('_','`_',testName)
#URLencode doesn't work so escape the common stuff
testName = urlEscape(testName)

# Setup data/limit URL to extract data
dataUrl = paste(baseUrl,"X=csv&P1=",lotName,"&P2=",testName,"%",sep='')
specLimtUrl = paste(dbHostUrl,"lotParmLimit?P1=",lotName,sep='')
trendLimtUrl = paste(dbHostUrl,"lotTrendLimit?P1=",lotName,sep='')

if (plotDebug > 0)
	cat(dataUrl,"\n");
cat(paste("<br><a href=",dataUrl,">Data</a>",sep=''),"(");
cat(paste("<a href=",trendLimtUrl,">Trend</a>",sep=''),"/ ");
cat(paste("<a href=",specLimtUrl,">Spec</a> limits)",sep=''),"\n");

# Extract data
plotData = read.csv(dataUrl)
if (nrow(plotData) < 1)
	cat("\nNo data found for lot ",lotName," ",testName,"\n")

if (dSLim == 'Y')
{
	specLimtUrl = paste(dbHostUrl,"lotParmLimit?X=csv&P1=",lotName,sep='')
	specLimitData = read.csv(specLimtUrl)
	if (nrow(specLimitData) < 1)
		cat("\nNo limit data found for lot ",lotName,"\n")
}
if (dTLim == 'Y')
{
	trendLimtUrl = paste(dbHostUrl,"lotTrendLimit?X=csv&P1=",lotName,sep='')
	trendLimitData = read.csv(trendLimtUrl)
	if (nrow(trendLimitData) < 1)
		cat("\nNo trend limit data found for lot ",lotName,"\n")
}

if (plotDebug > 0)
	attr(plotData,"names")

# get the test names
tnameData = unique(subset(plotData,T,c('PARMCODE','TEST_NAME','TEST_UNITS')))
tnameData$TN = paste(tnameData$PARMCODE,"-",tnameData$TEST_NAME," (",tnameData$TEST_UNITS,")",sep='')

for(idx in 1:nrow(tnameData))
{
	tname = as.character(tnameData$TN[idx])
	pcode = tnameData$PARMCODE[idx]
	pplotData = subset(plotData, PARMCODE == pcode)

	if (gbSite == 'Y')
	{
		pplotData=data.frame(VALUE=pplotData$VALUE, GrpName=pplotData$DIE_NBR)
		grpTitle = "Die Num";
	} else
	{
		if (gbWafer == 'Y')
		{
			pplotData=data.frame(VALUE=pplotData$VALUE, GrpName=pplotData$SCRIBE_NBR)
			grpTitle = "Scribe";
		} else
		{
			pplotData=data.frame(VALUE=pplotData$VALUE, GrpName=c(''))
			grpTitle = "";
		}
	}

# Setup plot
	outFile = paste(baseFile, idx, ".jpeg", sep='')
	imgFile = paste(baseImgUrl, idx, ".jpeg", sep='')
	jpeg(outFile, width=640, height=480, res=72, pointsize=12)
# Expand the right plot area for legend
	par(xpd=T, mar=par()$mar+c(0,0,0,5))

# 	Setup the plot

	plotTitle = paste("PPlot ", tname);
	ppData <- data.frame(value=c(),pct=c(),class=c())
	grpFactor <- as.factor(sort(unique(pplotData$GrpName)))
	grpIdxList = c(grpFactor) # get the factor index
	grpList = levels(grpFactor)

	for( grpIdx in grpIdxList)
	{
		grpName = grpList[grpIdx]
		pv <- qqnorm(pplotData$VALUE[pplotData$GrpName == grpName], plot.it=F)
		q <- data.frame(value=pv$y,pct=pv$x, class=array(grpIdx,length(pv$x)))
		ppData <- rbind(ppData, q)
	}

	yRange = c(min(ppData$pct,na.rm=TRUE),max(ppData$pct,na.rm=TRUE))
# 	Setup X range so that it include the limits
	xRange = c(min(ppData$value,na.rm=TRUE),max(ppData$value,na.rm=TRUE))
	if (dSLim == 'Y')
	{
		plimData = subset(specLimitData, PARMCODE == pcode)
		spec.uLimit = plimData$ULIMIT[1]
		spec.lLimit = plimData$LLIMIT[1]
		if (!is.na(spec.uLimit) && (xRange[2] < spec.uLimit))
			xRange[2] = spec.uLimit + (xRange[2] - xRange[1])*.1
		if (!is.na(spec.lLimit) && (xRange[1] > spec.lLimit))
			xRange[1] = spec.lLimit - (xRange[2] - xRange[1])*.1
	}
	if (dTLim == 'Y')
	{
		plimData = subset(trendLimitData, PARMCODE == pcode)
		trend.uLimit = plimData$ULIMIT[1]
		trend.lLimit = plimData$LLIMIT[1]
		trend.target = plimData$TARGET[1]
		if (!is.na(trend.uLimit) && (xRange[2] < trend.uLimit))
			xRange[2] = trend.uLimit + (xRange[2] - xRange[1])*.1
		if (!is.na(trend.lLimit) && (xRange[1] > trend.lLimit))
			xRange[1] = trend.lLimit - (xRange[2] - xRange[1])*.1
	}

# 	Do the plot
	if ((gbSite == 'Y') || (gbWafer == 'Y'))
	{
		symIdx <- as.numeric(ppData$class)
		plot(ppData$value, ppData$pct, pch=symIdx,col=(symIdx+1),
			xlim=xRange,
			main=plotTitle,xlab="Values",ylab="Quantile")
		legPos=xy.coords(par()$usr[2],par()$usr[4])
		legend(legPos,cex=.8,legend=grpList,pch=grpIdxList,col=(grpIdxList+1),title=grpTitle)
	} else
	{
		plot(ppData$value, ppData$pct,
			xlim=xRange,
			main=plotTitle,xlab="Values",ylab="Quantile")
	}

#   Draw the spec limit
	drawLimitLine = function(lv,lvColor)
	{
		x = c(lv, lv)
		y = yRange
		return(lines(x,y,col=lvColor,lty=5,lwd=2))
	}
	if (dSLim == 'Y')
	{
		if (!is.na(spec.uLimit))
			drawLimitLine(spec.uLimit, 'red');
		if (!is.na(spec.lLimit))
			drawLimitLine(spec.lLimit, 'red');
	}
	if (dTLim == 'Y')
	{
		if (!is.na(trend.uLimit))
			drawLimitLine(trend.uLimit, 'orange');
		if (!is.na(trend.lLimit))
			drawLimitLine(trend.lLimit, 'orange');
		if (!is.na(trend.target))
			drawLimitLine(trend.target, 'blue');
	}

# 	Restore default clipping rect
	par(xpd=F, mar=par()$mar+c(0,0,0,-5))

	dummy = dev.off()
	tname = urlEscape(as.character(tnameData$TEST_NAME[idx]))
	oneDataUrl = paste(baseUrl,"X=csv&P1=",lotName,"&P2=",tname,sep='')
	oneChartUrl = paste("/cgi-bin/rplot.pl/map/vmapParmPlot?gbSite=Y&lotName=",lotName,"&testName=",tname,sep='')
	cat("<br><a href=", oneChartUrl, "><img src='",imgFile,"'></a>\n",sep='')
}


