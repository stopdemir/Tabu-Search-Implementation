library(rgeos)
library(rgdal)
library(maptools)     # also loads sp()
library(RColorBrewer) # creates nice color schemes
library(classInt)     # finds class intervals for continuous variables

Asia<-readOGR(dsn="data5",layer="ASIAASIA")
plot(Asia,col="lightgrey")

attributes(orcounty.shp@data)
plotvar <- orcounty.shp@data$POP1990
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

plot(orcounty.shp, xlim=c(-124.5, -115), ylim=c(42,47))
plot(orcounty.shp, col=colcode, add=TRUE)
title(main="Population 1990",
      sub="Quantile (Equal-Frequency) Class Intervals")
legend(-117, 44, legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=0.6, bty="n")

map("county", "oregon", fill=FALSE)
points(orstationc$lon, orstationc$lat)
text(orstationc$lon, orstationc$lat, labels=orstationc$station, col="red",
     cex=.8)