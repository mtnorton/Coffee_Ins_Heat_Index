# CountStations.R
# For basis risk analysis
# How many stations for different filters?

# IDEAM Stations
n_sta # total IDEAM stations
length(which(station_timing>0)) # in coffee growing areas
length(which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200)) # With appropriate elevation       
# Current to 2015
length(which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200&indexes1[1:410,33]>0))

length(which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200&indexes1[1:410,33]>0&(rowSums(indexes1[1:410,19:28])>0|rowSums(indexes2[1:410,19:28])>0)  ))

infocul_coords <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/data/infocul_coords_2013.csv")

infocul_sp <- SpatialPoints(coords=infocul_coords[,1:2],proj4string = CRS("+init=epsg:3116"))
infocul_sp_lat <- spTransform(infocul_sp,crs(ecotopos))

infocul_regions <- over(infocul_sp_lat,ecotopos)$Ecotopo
infocul_regions <- paste0(substr(infocul_regions,1,1),substr(infocul_regions,4,4))

#-------------------------------

relevant_stations <- all_sta_coords[which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200&indexes1[1:410,33]>0&(rowSums(indexes1[1:410,19:28])>0|rowSums(indexes2[1:410,19:28])>0))]
# sta_elevs[1:410]>500&sta_elevs[1:410]<2200&
SCC_temp <- spTransform(SCC,crs(relevant_stations))
relevant_stations <- spRbind(relevant_stations,SCC_temp)

distNN = get.knnx(coordinates(relevant_stations),infocul_coords[,1:2],k=1)

sta_elevs_relevant  <- c(sta_elevs[which(station_timing>0&indexes1[1:410,33]>0&(rowSums(indexes1[1:410,19:28])>0|rowSums(indexes2[1:410,19:28])>0))],stations_cc[,2])
elev_diff2sta <- infocul_coords[,3]-sta_elevs_relevant[distNN$nn.index]

plot(relevant_stations[unique(distNN$nn.index)],col="blue",add=T)
temp <- SpatialPointsDataFrame(infocul_sp[which(distNN$nn.dist<10000)],data=as.data.frame(rep(1,length(infocul_sp[which(distNN$nn.dist<10000)]))))
writeOGR(obj=temp, dsn="/Users/mtnorton/Dropbox/temp/info_yellow2.shp" ,layer="ywllow", driver="ESRI Shapefile")
temp <- SpatialPointsDataFrame(relevant_stations[unique(distNN$nn.index)],data=as.data.frame(rep(1,length(relevant_stations[unique(distNN$nn.index)]))))
writeOGR(obj=temp, dsn="/Users/mtnorton/Dropbox/temp/info_red.shp" ,layer="red", driver="ESRI Shapefile")

# Now count (for all stations)
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1A"))/length(which(infocul_regions=="1A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1B"))/length(which(infocul_regions=="1B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2A"))/length(which(infocul_regions=="2A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2B"))/length(which(infocul_regions=="2B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3A"))/length(which(infocul_regions=="3A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3B"))/length(which(infocul_regions=="3B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="4"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="4"))/length(which(infocul_regions=="4"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500))/length(infocul_regions)

length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1A"))/length(which(infocul_regions=="1A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1B"))/length(which(infocul_regions=="1B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2A"))/length(which(infocul_regions=="2A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2B"))/length(which(infocul_regions=="2B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3A"))/length(which(infocul_regions=="3A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3B"))/length(which(infocul_regions=="3B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="4"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="4"))/length(which(infocul_regions=="4"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100))/length(infocul_regions)

#----------------------------------------------------------

# Gov stations only

relevant_stations <- all_sta_coords[which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200&indexes1[1:410,33]>0&(rowSums(indexes1[1:410,19:28])>0|rowSums(indexes2[1:410,19:28])>0))]

distNN = get.knnx(coordinates(relevant_stations),infocul_coords[,1:2],k=1)

sta_elevs_relevant <- sta_elevs[which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200&indexes1[1:410,33]>0&(rowSums(indexes1[1:410,19:28])>0|rowSums(indexes2[1:410,19:28])>0))]
elev_diff2sta <- infocul_coords[,3]-sta_elevs_relevant[distNN$nn.index]

# Now count (for govt stations)
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1A"))/length(which(infocul_regions=="1A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1B"))/length(which(infocul_regions=="1B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2A"))/length(which(infocul_regions=="2A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2B"))/length(which(infocul_regions=="2B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3A"))/length(which(infocul_regions=="3A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3B"))/length(which(infocul_regions=="3B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="4"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="4"))/length(which(infocul_regions=="4"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500))/length(infocul_regions)

length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1A"))/length(which(infocul_regions=="1A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1B"))/length(which(infocul_regions=="1B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2A"))/length(which(infocul_regions=="2A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2B"))/length(which(infocul_regions=="2B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3A"))/length(which(infocul_regions=="3A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3B"))/length(which(infocul_regions=="3B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="4"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="4"))/length(which(infocul_regions=="4"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100))/length(infocul_regions)

#--------------------------------------------------

relevant_stations <- SCC_temp

distNN = get.knnx(coordinates(relevant_stations),infocul_coords[,1:2],k=1)

sta_elevs_relevant <- stations_cc[,2]
elev_diff2sta <- infocul_coords[,3]-sta_elevs_relevant[distNN$nn.index]

# Now count (for CC stations)
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1A"))/length(which(infocul_regions=="1A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="1B"))/length(which(infocul_regions=="1B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2A"))/length(which(infocul_regions=="2A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="2B"))/length(which(infocul_regions=="2B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3A"))/length(which(infocul_regions=="3A"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="3B"))/length(which(infocul_regions=="3B"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="4"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500&infocul_regions=="4"))/length(which(infocul_regions=="4"))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500))
length(which(distNN$nn.dist<10000&abs(elev_diff2sta)<500))/length(infocul_regions)

length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1A"))/length(which(infocul_regions=="1A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="1B"))/length(which(infocul_regions=="1B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2A"))/length(which(infocul_regions=="2A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="2B"))/length(which(infocul_regions=="2B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3A"))/length(which(infocul_regions=="3A"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="3B"))/length(which(infocul_regions=="3B"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="4"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100&infocul_regions=="4"))/length(which(infocul_regions=="4"))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100))
length(which(distNN$nn.dist<2000&abs(elev_diff2sta)<100))/length(infocul_regions)

#----------------------------------------------------------

rel_stations <- which(station_timing>0&sta_elevs[1:410]>500&sta_elevs[1:410]<2200&indexes1[1:410,33]>0&(rowSums(indexes1[1:410,19:28])>0|rowSums(indexes2[1:410,19:28])>0))

length(which(!is.na(screened_indexes1[rel_stations,19:28])))
length(which(!is.na(screened_indexes2[rel_stations,19:28])))
length(which(!is.na(screened_indexes_cc1[,13:24])))
length(which(!is.na(screened_indexes_cc2[,13:24])))

