#swan.raw is from the data access script
swan<-swan.raw[!is.na(swan.raw$DissolvedOxygen_Saturation),]

#Create a histogram of Swan Lake DO%sat data from all 17 sampling weeks and 98 sites (n=1665 measurements)
hist(swan$DissolvedOxygen_Saturation)
#Test for skewness in the DO%sat data
hshmtest(swan$DissolvedOxygen_Saturation, stat="skewness") #The distribution is significantly right skewed
#Identify the HSHM measurements in the data set
hshmid_swanDO<-hshmid(swan$DissolvedOxygen_Saturation, criteria = "ref.normal", thresh=0.95, side = "upper")
#Add points to the histogram to denote the HSHM measurements in the distribution
points(swan$DissolvedOxygen_Saturation[hshmid_swanDO], rep(0,sum(hshmid_swanDO)), pch=19, col="dodgerblue4")

#Add a column to the original data set that indicates if the measurement was an HSHM with "TRUE" or not with "FALSE"
swan$hshm = hshmid_swanDO


#evaluate if there are specific sampling dates for these HSHM
#YES - all HSHM measurements are confined to DOY = 149, 156, 184, 191 which were non-cyano bloom days
#These high values align well with the single-station high frequency sonde
plot(swan[swan$hshm==TRUE, "DissolvedOxygen_Saturation"], swan[swan$hshm==TRUE, "DOY"])
unique(swan[swan$hshm==TRUE, "DOY"]) 

#What percentage of the measurements on DOYs with HM were HS?
#DOY 149 - 5.1% of measurements were HS
length(swan[swan$DOY==149 & swan$hshm==TRUE,"DissolvedOxygen_Saturation"])/
length(swan[swan$DOY==149,"DissolvedOxygen_Saturation"])

#DOY 156 - 33.7% of measurements were HS
length(swan[swan$DOY==156 & swan$hshm==TRUE,"DissolvedOxygen_Saturation"])/
  length(swan[swan$DOY==156,"DissolvedOxygen_Saturation"])

#DOY 184 - 24.5% of measurements were HS
length(swan[swan$DOY==184 & swan$hshm==TRUE,"DissolvedOxygen_Saturation"])/
  length(swan[swan$DOY==184,"DissolvedOxygen_Saturation"])

#DOY 191 - 81.4% of measurements were HS
length(swan[swan$DOY==191 & swan$hshm==TRUE,"DissolvedOxygen_Saturation"])/
  length(swan[swan$DOY==191,"DissolvedOxygen_Saturation"])

#Swan Shapefile--------------------------------------------------------------------------------------------
library(sf)
library(ggplot2)



#Read in shapefile of Swan
#SwanOut <- read_sf("C:/Users/grace/Box/Hot Spomoments - UW/Case Studies/Swan_Shapefile/SwanShapefile.shp")
SwanOut <- read_sf("./Swan_Shapefile/SwanShapefile.shp")

#Remove the "Z" dimension of data leaving only XY 
SwanOut <- st_zm(SwanOut)

#Check geometry information; like projection and units
SwanOut$geometry

#Create polygon of shp
SwanOut <- st_polygonize(SwanOut)

#Assign UTM Zome 15N projection
SwanOut <- st_transform(SwanOut,crs = 6344)

#Check the projection
SwanOut$geometry

#Plot Swan Lake Outline
ggplot()+
  geom_sf(data = SwanOut)+
  coord_sf(expand = F)

SwanOutline <- SwanOut

plot(SwanOut$geometry, col = "white", axes = T)

grid <- SwanOut %>% 
  st_make_grid(cellsize = 25, square = TRUE, crs = 6344) # grid of points

#Assign coordinate system to grid
grid <- st_transform(grid, 6344)

#Clip grid to swan outline
grid <- st_intersection(SwanOut,grid)

#Plot trimed grid
ggplot()+
  geom_sf(data = grid)

plot(grid$geometry, col = "white", axes = T)

grid$geometry

#Convert to Lat Long
grid <- st_transform(grid, crs="+proj=longlat +datum=WGS84")
SwanOutline <- st_transform(SwanOut, crs="+proj=longlat +datum=WGS84")

grid$geometry
SwanOut$geometry

#SwanKrige-------------------
library(dplyr)
library(gstat)

#Summarise macro data
SwanMacro <- swan %>%
  dplyr::select(Latitude, Longitude, DOY, Macrophyte, Site)%>%
  filter(Macrophyte == 1) %>%
  group_by(Site)%>%
  summarise(macro_count = n(), Latitude = mean(Latitude), Longitude = mean(Longitude))

#10 weeks where macrophytes were recorded

#Cacluate site macrophyte occurance percentage
SwanMacro <- SwanMacro %>%
  mutate(MacroCountPer = macro_count/10)

#transform data to sf 
SwanMacro <- st_as_sf(SwanMacro, coords = c("Longitude", "Latitude"))

#Change crs
st_crs(SwanMacro) = crs="+proj=longlat +datum=WGS84"

#Check crs actually changed
SwanMacro$geometry

#formula and locations for interpolation
gs <- gstat(id = "Site", 
            formula = MacroCountPer~1,
            locations = SwanMacro,
            data = as(SwanMacro,"Spatial"),
            set = list(idp = 2))

#Actually interpolate 
idw <- predict(gs,grid)

#Assign color to DOY points
col <- c("DOY 149" = "#ffffcc",
         "DOY 156" = "#a1d99b",
         "DOY 184" = "#9ecae1",
         "DOY 191" = "#08519c",
         "No HSHM" = "white")

#Plot of Swan with Macrophyte interpolation and HSHM points------------------
#V2 with transparent points-----
head(swan)

swan <- swan %>% 
  group_by(Site) %>%
  mutate(hshm_count = sum(hshm))

Swan_M_HSHM <- ggplot()+
  geom_polygon(aes(SwanOutline[[6]][[1]][[1]][[1]][,1], 
                   SwanOutline[[6]][[1]][[1]][[1]][,2]), 
               color = "black", cex = 1) +
  xlab("Latitude") +
  ylab("Longitude") +
  geom_sf(data = idw, aes(fill = Site.pred), color = NA, show.legend = T)+
  
  scale_fill_gradientn(colors = grey.colors(40, start = 1, end = 0.1), 
                       limits = c(0,1), 
                       name = "Macrophytes",
                       labels = c("0%", "25%", "50%", "75%", '100%'),
                       guide = guide_colorbar(frame.colour = "black", 
                       ticks.colour=NULL)) + 
  scale_x_continuous(breaks = c(-94.85,-94.845,-94.84)) + 
  
  geom_point(aes(swan$Longitude[swan$hshm_count == "0"],
                 swan$Latitude[swan$hshm_count == "0"], 
                 color = "No HSHM", cex = 2), pch = 20, cex = 1.5)+
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==149],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==149], 
                 color = "DOY 149", cex = 4.5), pch=19, cex = 4, alpha = 0.5) +
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==156],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==156], 
                 color = "DOY 156", cex = 3.5), pch=19, cex = 3, alpha = 0.5) +
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==184],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==184], 
                 color = "DOY 184", cex = 3), pch=19, cex = 2.5, alpha = 0.5) +
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==191],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==191], 
                 color = "DOY 191", cex = 1.5), pch=19, cex = 1.5, alpha = 0.5) +
  
  scale_color_manual(values = col, name = "" )+
  guides(color = guide_legend(override.aes = list(size=c(4.5, 3.5, 2.5, 1.5, 2))))+
  theme_set(theme_bw()) +
  theme(legend.key.height = unit(17, "pt"),
        plot.margin = margin(10,10,10,10))

Swan_M_HSHM

#ggsave(filename = "C:/Users/dortiz4/Box/Hot Spomoments - UW/Case Studies/Swan_DO_HSHM_V2.pdf", plot = Swan_M_HSHM, width = 6.5, height = 5, dpi = 500)

#ggsave(filename = "C:/Users/dortiz4/Box/Hot Spomoments - UW/Case Studies/Swan_DO_HSHM_V2.png", plot = Swan_M_HSHM, width = 6.5, height = 5, dpi = 500)

#--------V3 multi-panel plot-------
library(cowplot)
library(ggspatial)

Swan_Macro_Loc <- ggplot()+
  geom_sf(data = idw, aes(fill = Site.pred), color = NA, show.legend = T)+
  geom_sf(data = SwanOutline, color = "black", fill = NA) +
  scale_fill_gradientn(colors = grey.colors(40, start = 1, end = 0.1), 
                       limits = c(0,1), 
                       name = "Macrophytes",
                       labels = c("0%", "25%", "50%", "75%", '100%'),
                       guide = guide_colorbar(frame.colour = "black", ticks.colour=NULL)) + 
  xlab("") +
  ylab("Longitude") +
  scale_x_continuous(breaks = c(-94.85,-94.845,-94.84)) + 
  geom_point(aes(swan$Longitude,
                 swan$Latitude, 
                 fill = "black", size = 0.5), pch = 19, size = 0.5, fill = "black")+
  labs(subtitle = "b)")+
  theme_set(theme_bw()) +
  theme(legend.key.height = unit(17, "pt"),
        axis.text.x = element_blank(), 
        axis.text = element_text(size = 8), 
        plot.subtitle = element_text(vjust = -10, hjust = 0.05),
        plot.margin = margin(1,1,1,1))

Swan_Macro_Loc


Swan_HSHM <- ggplot()+
  geom_sf(data = SwanOutline, color = "black", fill = "grey70") +
  xlab("Latitude") +
  ylab("Longitude") +
  
  scale_x_continuous(breaks = c(-94.85,-94.845,-94.84)) + 
  
  geom_point(aes(swan$Longitude[swan$hshm_count == "0"],
                 swan$Latitude[swan$hshm_count == "0"], 
                 fill = "No HSHM", cex = 0.75), pch = 21, cex = 0.7, color = "white")+
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==149],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==149], 
                 fill = "DOY 149", cex = 4.5), pch=16, cex = 3,  color = "#ffffcc") +
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==156],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==156], 
                 fill = "DOY 156", cex =3.25), pch=21, cex = 2.1,  color = "#a1d99b") +
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==184],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==184], 
                 fill = "DOY 184", cex = 2), pch=21, cex = 1.5,  color = "#9ecae1") +
  
  geom_point(aes(swan$Longitude[swan$hshm==TRUE & swan$DOY==191],
                 swan$Latitude[swan$hshm==TRUE & swan$DOY==191], 
                 fill = "DOY 191", cex = 0.75), pch=21, cex = 0.7,  color = "#08519c") +
  ggspatial::annotation_scale(location = "bl",bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "cm"), pad_y = unit(0.7, "cm"), height = unit(0.75, "cm"),
                                    style = ggspatial::north_arrow_minimal(fill = c("black"),line_col = "black"))+
  scale_fill_manual(values = col, name = NULL )+
  guides(fill = guide_legend(override.aes = list(size=c(3, 2.5, 2, 1.5, 1.5), color = "black", shape = 21)))+  
  labs(subtitle = "c)")+
  theme_set(theme_bw()) +
  theme(legend.key.height = unit(17, "pt"),
        legend.key.width = unit(6, "pt"),
        legend.text.align = 0,
        axis.text = element_text(size = 8), 
        plot.subtitle = element_text(vjust = -10, hjust = 0.05),
        plot.margin = margin(1,1,1,1))

Swan_HSHM

xx <- 0:400

par(mar=c(4.1,4.1,1.1,1.1), mgp=c(1.8,0.6,0), cex.axis=0.9, cex.lab=0.9)
hist(swan$DissolvedOxygen_Saturation, freq=FALSE, main="", xlab="Dissolved oxygen saturation")
lines(xx, dnorm(xx, mean(swan$DissolvedOxygen_Saturation, na.rm=T), sd(swan$DissolvedOxygen_Saturation, na.rm=T)), lwd=1.5, lty=2)
points(swan$DissolvedOxygen_Saturation[swan$hshm], rep(0, sum(swan$hshm)), pch="+", cex=0.7)
mtext("a)", at=par("usr")[1]+0.05*diff(par("usr")[1:2]), cex=0.9)
distro = recordPlot()

Swan_V3 <- cowplot::plot_grid(distro,Swan_Macro_Loc,Swan_HSHM, ncol = 1, align = "hv", axis="tblr")

Swan_V3

ggsave(filename = "Swan_DO_HSHM_V4.png", plot = Swan_V3, width =4, height = 6.8, dpi = 500)

ggsave(filename = "Swan_DO_HSHM_V4.pdf", plot = Swan_V3, width =4, height = 6.8, dpi = 500)

