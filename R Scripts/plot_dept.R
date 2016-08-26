# plot_dept.R
# Using ggplot library to make maps
# Map 1: timing of critical period
# Map 2: number of weather stations per ecotopo

-----

# Map 1: timing of critical period

library(sp)
library(raster)
library(ggplot2)
library(maptools)
library(rgeos)
library(mapproj)
library(ggmap)
library(RColorBrewer)

# Colombia GIS data from DIVA-GIS (http://www.diva-gis.org/)
nat_bound  <- shapefile("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_adm/COL_adm0.shp")
dept_bound <- shapefile("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_adm/COL_adm1.shp")

col_dept <- matrix(NA,32,2)
col_show <- vector(mode="logical",length=32)

col_dept[,1] <- dept_bound$ID_1
# Input categories by hand
col_dept[,2] <- c(0,2,0,0,0,3,2,0,0,1,3,0,0,1,0,0,1,3,3,0,1,4,0,4,2,0,5,0,1,4,0,0)
col_dept[,2] <- col_dept[,2]+1
date_ranges <- c("(None)","July 1 - June 30","January 1 - December 31","February 1 - January 31","June 1 - May 31","November 1 - October 31")
col_dept[,2] <- date_ranges[col_dept[,2]]

col_dept <- as.data.frame(col_dept)
names(col_dept) <- c("id","region")

dept.shp.f <- fortify(dept_bound, region = "ID_1")

merge.shp.coef<-merge(dept.shp.f, col_dept, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 

# Timing:
#1: Cauca, Cundinamarca, Huila, Narino, Tolima
#2: Antioquia, Caldas, Risaralda
#3: Boyaca, Magdalena, Cesar, Guajira
#4: Norte de Santander, Quindio, Valle
#5: Santander

my.cols <- brewer.pal(6, "Reds")

ggplot() +
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill = factor(region)), color = "black", size = 0.25) + 
  theme_nothing(legend = TRUE)+
  scale_fill_manual("Index Period", values = my.cols, guide = "legend") +
  coord_map()


----------------------------

# Map 2: number of weather stations per ecotopo
# Uses variables from Describe_CO_Data.R for ecotopos
# And HeatIndex.R and HeatIndexCenicafe.R for station coordinates

sta_coords <- SpatialPoints(stations_cc[,10:9],proj4string = CRS("+init=epsg:4326"))
sta_coords <- spTransform(sta_coords,crs(ecotopos))
sta_temp <- spTransform(S, crs(ecotopos))
sta_coords <- rbind(sta_coords,sta_temp)
ecoto_merge <- over(sta_coords,ecotopos)$Ecotopo

num_sta <- matrix(NA,87,2)

for (i in 1:87)
{
  num_sta[i,1] <- ecotopos$Ecotopo[i]
  num_sta[i,2] <- length(which(ecoto_merge==ecotopos$Ecotopo[i]))
}

num_sta <- as.data.frame(num_sta)
names(num_sta) <- c("id","num_sta")

ecotopos.gg <- spTransform(ecotopos,crs(nat_bound))
ecotopos.f <- fortify(ecotopos.gg,region="Ecotopo")
merge.shp.coef<-merge(ecotopos.f, num_sta, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 

ggplot() +
  geom_polygon(data = nat_bound.f, aes(x = long, y = lat, group = group))+
  geom_polygon(data = final.plot, aes(x = long, y = lat, group = group, fill=factor(num_sta)))+
  theme_nothing(legend = TRUE)+
  scale_fill_manual("# Stations", values = my.cols, guide = "legend") +
  coord_map()

length(which(num_sta==0))
