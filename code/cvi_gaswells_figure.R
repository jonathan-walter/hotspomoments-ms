# 
require(raster)
require(rgdal)
require(sp)
require(sf)
require(ggplot2)
library(RColorBrewer)
library(hotspomoments)
# bring in elevation data
dem <- raster("./data/gis/elevation_data/n39_w080_1arc_v2.tif")

#x11()
plot(dem)

# bring in watershed
weimer <- readOGR("./data/gis/weimer_watershed/wymerbasin.shp")

#x11()
plot(weimer)

# reproject the DEM
# Project Raster
dem_utm <- projectRaster(dem, crs = weimer)
## crop and mask
r2 <- crop(dem_utm, extent(weimer))
r3 <- mask(r2, weimer)

# plot the new clipped raster DEM
#x11()
plot(r3)

# now lets bring in the points where plots are
# plots <- read.csv("./data/gis/plot_gps_coord.csv")
plots <- read.csv("./data/gis/cvi_plots_short.csv")
names(plots)[1] <- "PLOT"
plots$X = plots$X * -1

coordinates(plots)=~X+Y
proj4string(plots)<- crs(dem)

plots_utm <-  spTransform(plots, crs(weimer))

plot(plots_utm)

# add extents
mid.ext <- raster::extent(c(xmin = 634600, xmax = 634900, ymin = 4330600, ymax = 4330900))

#x11()
plot(r3,  col = grey.colors(24), ext = mid.ext)
points(plots_utm, pch = 21, bg = 4,    # Symbol type and color
       col = plots_utm$X5cm  , # Symbol sizes
       add = TRUE)
points(plots_utm, pch = 21, bg = 4,    # Symbol type and color
       col = 4, # Symbol sizes
       add = TRUE)

# bring in gas well data
gas <- read.csv("./data/gas_well_data_2010_2012.csv")

gas5cm <- gas$X5cm[!is.na(gas$X5cm)]

hshm<-hshmid(gas5cm, side="upper", criteria = "ref.normal", thresh=0.95)

# 5 cm had 850 NA values

# make appro plot value
names(gas)[5] <- "plot_num"
gas$PLOT <- paste(toupper(substr(gas$ELEV, 1, 1)), toupper(substr(gas$VEG, 1, 1)), gas$plot_num, sep = "" )

# replace
gas$VEG <- gsub("canopy", "closed", gas$VEG)

# 
#x11()
ggplot(gas, aes(x = JD, y = X5cm, color = VEG))+
     geom_point()+
     xlab("Julian Day")+
     ylab("CO2 ppm")+
     stat_smooth(method = "loess")+
     facet_wrap(. ~ ELEV, nrow = 3)


str(gas)


################
# Color palette
pal_init <- hcl.colors(3, "Temps", alpha = 0.8)
pal1 <- brewer.pal(9, "YlOrBr")
cramp <- colorRampPalette(colors=c(pal1[1],pal1[5],pal1[9]))
pal <- cramp(11)

#x11()

png("fig_cvi_hshm.png", units="in", res=300, width=6.5, height=3.5)

par(mfrow=c(1,2), mar=c(3.1,3.1,1.1,0.5), mgp=c(1.9,0.7,0), oma=c(0,0,0,1.5))


hist(gas5cm, freq=FALSE, main="", xlab=expression(Soil~CO[2]~concentration~(ppm)))
xx<-seq(0,25000, by=100)
lines(xx, dnorm(xx, mean(gas$X5cm, na.rm=T), sd(gas$X5cm, na.rm=T)), lwd=1.5, lty=2)
points(gas5cm[hshm], rep(0, sum(hshm)), pch="+", cex=0.75)
mtext("a)", at=par("usr")[1], line=0.1)

par(mar=c(3.1,3.1,1.1,2.1))
plot(r3,  col = grey.colors(12), xlab="Easting (m)", ylab="Northing (m)", cex.axis=0.8)
plot(plots_utm,
     pch = c(21, 22, 23)[as.factor(plots_utm$VEG)], 
     bg = pal[plots_utm$X5cm+1],
     cex = 1, # Symbol size
     #pal = brewer.pal(6, "Set2"),      # Color palette
     add = TRUE)
legend("bottomleft", pch=c(21,22,23), legend=c("Closed","Open","Shrub"), bty="n",
       inset=c(0,0))
text(634300, 4331600, "Low")
text(635000, 4330700, "Mid")
text(635900, 4330500, "High")
mtext("Elevation (m)",4, cex=2/3, line=-0.18)
mtext("b)", at=par("usr")[1], line=0.1)

par(fig=c(0.75,0.93,0.86,0.89), new=TRUE, mgp=c(0.1,0.1,0))
image(x=0:10,z=matrix(0:10, ncol=1), col=pal, yaxt="n", xaxt="n", xlab="No. HSHM")
axis(1, at=c(0,10), tcl=-0.1)
mtext("No. HSHM", cex=3/4)

dev.off()

# legend("right",
#        xjust = 1,
#        y.intersp = 1.3,
#        bty = "n",
#        legend = seq(100, 500, 100),
#        col = "grey20",
#        pt.bg = 4,
#        pt.cex = log(seq(100, 500, 100) / 15),
#        pch = 21,
#        title = "Airports")



# 
# dem.df <- as.data.frame(r3, xy = TRUE)
# 
# #####
# #x11()
# ggplot(data = dem.df , aes(x = x, y = y, fill = n39_w080_1arc_v2))+
#      geom_raster() + 
#      scale_fill_manual(values = terrain.colors(3)) + 
#      coord_quickmap()
