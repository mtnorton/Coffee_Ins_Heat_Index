# ProductionGeography.R
# Catchall for working with the production data sets

#################
# 1. Use a Digital Elevation Model (DEM) to get the elevation at each of the production record points
#################

library(sp)
library(raster)
library(maptools)
library(FNN)
library(GISTools)
library(rgdal)

prod_coords <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/BaseProdCoords.csv")
cod_lote <- prod_coords[,1] # save for later

prod_coords_3116 <- SpatialPoints(prod_coords[,2:3],proj4string = CRS("+init=epsg:3116"))

colombia_DEM <- raster("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_msk_alt/COL_msk_alt.grd")
prod_coords <- spTransform(prod_coords_3116, crs(colombia_DEM))

plot(colombia_DEM)
plot(prod_coords,add=TRUE)

prod_elevs <- extract(colombia_DEM,coordinates(prod_coords))
write.csv(cbind(cod_lote,prod_elevs),"/Users/mtnorton/Coffee_Ins_Heat_Index/BaseProdElevs.csv")

# Note: Need to CSV into Stata if needed

#Steps from here:
# 1. Get department of each weather station (for timing).
# 2. Get nearest neighbor station for each lote.
# 3. sum(y_lote)/sum(areal) at each weather station/year
# 4. Filter by 2 km, 5 km, 10 km.
# 5. Filter by +/- 100m, 250m, 500m difference in altitude
# 6. Match screened indexes against production estimate for that year

#################
# 1. Get department of each weather station (for timing).
#################

S_prod <- SpatialPoints(sta_coords,CRS("+init=epsg:4326"))
S_prod <- spTransform(S_prod,crs(SCC))
all_sta_coords <- spRbind(S_prod,SCC)
all_sta_coords <- spTransform(all_sta_coords,crs(prod_coords_3116))

dept_bound <- shapefile("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_adm/COL_adm1.shp")
dept_bound$timing <- c(0,2,0,0,0,2,2,0,0,1,2,0,0,1,0,0,1,2,2,0,1,1,0,1,2,0,2,0,1,1,0,0)
dept_bound <- spTransform(dept_bound,crs(prod_coords_3116))

prod_timing <- over(all_sta_coords,dept_bound)$timing

#################
# 2. Get nearest neighbor station for each lote.
#################

distNN = get.knnx(coordinates(all_sta_coords),coordinates(prod_coords_3116),k=1)

baseprod <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/Data/BaseProdv7.csv")
#sta_elevs <- c(as.numeric(sta_output[-c(49,52),2]),stations_cc[,2])
sta_elevs <- c(sta_elevs,stations_cc[,2])

#################
# 3. Add index, cosecha, station number, distance, and elevation difference to each prod. record
#################

prod_builder <- matrix (NA, nrow(baseprod), 8)

# 4x2 matrix of cosecha timing, type as row, semestre as column
cosecha <- matrix(NA, 4, 2)

cosecha <- rbind(c(1,2),c(2,1),c(1,2),c(2,1))

baseprod[which(is.na(baseprod[,4])),4] <- 5 # Get rid of NAs

for (i in 1:nrow(baseprod))
{
  if (baseprod[i,4]<5)
  {
    prod_builder[i,1] <- baseprod[i,3] # cod_lote
    prod_builder[i,2] <- baseprod[i,1] # ao
    prod_builder[i,3] <- baseprod[i,2] # semestre
    prod_builder[i,4] <- cosecha[baseprod[i,4],baseprod[i,2]] # cosecha
    
    if (distNN$nn.index[i]<=410)
    {
      prod_builder[i,5] <- sta_nums[distNN$nn.index[i]] # station number
      if (baseprod[i,2]==1)
        {prod_builder[i,6] <- screened_indexes1[distNN$nn.index[i],baseprod[i,1]-1983]}
      else {prod_builder[i,6] <- screened_indexes2[distNN$nn.index[i],baseprod[i,1]-1983]}
    }
    else 
    {
      prod_builder[i,5] <- distNN$nn.index[i]-410 # No numbers for CC stations
      if (baseprod[i,2]==1)
      {prod_builder[i,6] <- screened_indexes_cc1[distNN$nn.index[i]-410,baseprod[i,1]-1988]}
      else {prod_builder[i,6] <- screened_indexes_cc2[distNN$nn.index[i]-410,baseprod[i,1]-1988]}
    }
    # index in year
    
    prod_builder[i,7] <- distNN$nn.dist[i]
    prod_builder[i,8] <- prod_elevs[i]-sta_elevs[distNN$nn.index[i]]
  }
}

prod_builderOld <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/data/Stata_prod_merge.csv")

prod_builder[which(is.na(prod_builder))] <- "."

colnames(prod_builder) <- c("cod_lote","ao","semestre","cosecha","sta_num","heat_index","distance","elev_diff")

write.csv(prod_builder,"/Users/mtnorton/Coffee_Ins_Heat_Index/data/Stata_prod_merge.csv")

#################
# OLD
# 3. sum(y_lote)/sum(areal) at each weather station/year for nearby stations
# 4. Filter by 2 km, 5 km, 10 km.
# 5. Filter by +/- 100m, 250m, 500m difference in altitude
#################

station_year <- matrix(NA, length(prod_timing), 10)
weighted_elev_diff <- matrix(NA, length(prod_timing), 10)
num_plots_sy <- matrix(NA, length(prod_timing), 10)

count_plots_in_range <- matrix(0, 9, 5)

distances <- c(2000,5000,10000) # in meters
elevations <- c(100,250,500)

for (e in 1:length(elevations))
{
  for (d in 1:length(distances))
  {
    for (i in 1:length(prod_timing))
    {
      if (!is.na(prod_timing[i])&(length(which(c(411:454)==i))>0))
      {
        to_sum <- which((distNN$nn.index==i)&(distNN$nn.dist<distances[d])&(baseprod$edad<16)&(baseprod$edad>1)&(abs(prod_elevs-sta_elevs[i])<elevations[e]))
        
        for (j in 1:10)
        {
          num_plots <- length(intersect(to_sum,which((baseprod$ao==j+2000))))
          
          num_plots_sy[i,j] <- num_plots
          
          if (num_plots==0) {count_plots_in_range[(e-1)*3+d,1] <- count_plots_in_range[(e-1)*3+d,1]+1}
          if ((num_plots<=5)&(num_plots>0)) {count_plots_in_range[(e-1)*3+d,2] <- count_plots_in_range[(e-1)*3+d,2]+1}
          if ((num_plots>5)&(length(num_plots)<=10)) {count_plots_in_range[(e-1)*3+d,3] <- count_plots_in_range[(e-1)*3+d,3]+1}
          if ((num_plots>10)&(length(num_plots)<=15)) {count_plots_in_range[(e-1)*3+d,4] <- count_plots_in_range[(e-1)*3+d,4]+1}
          if (num_plots>15) {count_plots_in_range[(e-1)*3+d,5] <- count_plots_in_range[(e-1)*3+d,5]+1}
          
          if (prod_timing[i]==2)
          {
            if (length(which(distNN$nn.index==i))>0)
            {
              station_year[i,j] <- sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000))),]$y_lote)/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000))),]$areal)
              weighted_elev_diff[i,j] <- sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000))),]$areal/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000))),]$areal)*(baseprod$elevation[intersect(to_sum,which((baseprod$ao==j+2000)))]-sta_elevs[i]))
            }
          }
          else
          {
            station_year[i,j] <- sum(baseprod[intersect(to_sum,which((baseprod$ao==j+1999)&(baseprod$semestre==2))),]$y_lote)/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+1999)&(baseprod$semestre==2))),]$areal)
            station_year[i,j] <- station_year[i,j] + sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000)&(baseprod$semestre==1))),]$y_lote)/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000)&(baseprod$semestre==1))),]$areal)
          }
        }
      }
      write.csv(station_year,paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/regress/station_year",distances[d],elevations[e],".csv"))
      write.csv(weighted_elev_diff,paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/regress/wed",distances[d],elevations[e],".csv"))
    }
  }
}

#################
# 6. Match screened indexes against production estimate for that year
#################

par(mfrow=c(3,3),mar=rep(2,4))

for (e in 1:length(elevations))
{
  for (d in 1:length(distances))
  {
    to_regress <- matrix(NA,1,4)
    station_year <- read.csv(paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/regress/station_year",distances[d],elevations[e],".csv"))
    weighted_elev_diff <- read.csv(paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/regress/wed",distances[d],elevations[e],".csv"))
    station_year <- as.matrix(station_year[,-1])
    weighted_elev_diff <- as.matrix(weighted_elev_diff[,-1])
      for (i in 1:length(prod_timing))
      {
        if (i <= 57) # govt stations
        {index_rec <- screened_indexes[i,19:28]} else {index_rec <- screened_indexes_cc[i-57,13:22]}
  
        for (j in 1:10)
        {
          if ((!is.na(station_year[i,j]))&(!is.na(index_rec[j])))
          {
            to_regress <- rbind(to_regress, cbind(index_rec[j],station_year[i,j],index_rec[j]^2,weighted_elev_diff[i,j]))
          }
        }
      }
    
    # Now fit quadratic function and plot it
    write.csv(to_regress,paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/regress/to_regress",distances[d],elevations[e],".csv"))
    
    now_regress <- summary(lm(to_regress[,2] ~ to_regress[,1] + to_regress[,3] + to_regress[,4]))
    
    plot(to_regress[,1:2],main=paste0("(",distances[d]/1000,"km/",elevations[e],"m,Rsq=",round(now_regress$r.squared,2),")"),xlab="Heat Index",ylab="Prod. Est.",col="gray",xlim=c(1400,3000))
    prev <- 0
    for (g in min(to_regress[,1],na.rm=TRUE):max(to_regress[,1],na.rm=TRUE))
    {
      line_height <- now_regress$coefficients[1] + now_regress$coefficients[2]*g + now_regress$coefficients[3]*(g^2)
      if (prev>0)
      {
        lines(c(prev,g),c(line_height_p,line_height))
      }
      prev <- g
      line_height_p <- line_height
    }
  }
}

# Export for ArcGIS

library(rgdal)
library(GISTools)

blue <- SpatialPointsDataFrame(coordinates(all_sta_coords[which(rowSums(num_plots_sy)>0)]),data=as.data.frame(sta_elevs[which(rowSums(num_plots_sy)>0)]),proj4string=crs(all_sta_coords))
red <- SpatialPointsDataFrame(coordinates(all_sta_coords[which(rowSums(num_plots_sy)==0)]),data=as.data.frame(sta_elevs[which(rowSums(num_plots_sy)==0)]),proj4string=crs(all_sta_coords))
yellow <- SpatialPointsDataFrame(coordinates(prod_coords_3116),data=as.data.frame(prod_elevs),proj4string=crs(prod_coords_3116))

writeOGR(obj=blue, dsn="/Users/mtnorton/Dropbox/temp/blue.shp" ,layer="blue", driver="ESRI Shapefile")
writeOGR(obj=red, dsn="/Users/mtnorton/Dropbox/temp/red.shp" ,layer="red", driver="ESRI Shapefile")
writeOGR(obj=yellow, dsn="/Users/mtnorton/Dropbox/temp/yellow.shp" ,layer="yellow", driver="ESRI Shapefile")

#--------------------------------------------

# OLD CODE_____ CAN IGNORE
#################
# 2. Create production record for every ecotopo/year
#################

ecotopos <- shapefile("/Users/mtnorton/Dropbox/Coffee Insurance/Data/coffeemission_data/ecotopos/POLIGONOS_ECOTOPOS_CAFETEROS.shp")

# Figure out which department each ecotopo is in

dept_bound <- shapefile("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_adm/COL_adm1.shp")
dept_bound$timing <- c(0,2,0,0,0,2,2,0,0,1,2,0,0,1,0,0,1,2,2,0,1,1,0,1,2,0,2,0,1,1,0,0)

# 0 = not coffee, 1 = use 2nd set previous year, 2 = use both in calendar 
# referring to the two sets of prod. data in Stata file

ecotopos <- spTransform(ecotopos,crs(dept_bound))

timing <- over(ecotopos,dept_bound)$timing

baseprod <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/BaseProd.csv")

ecotopo_year <- matrix (NA, 87, 10)

for (year in 2001:2010)
{
  for (ecot in 1:87)
  {
    if (timing[ecot]==2)
    {
      if (length(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==year)),]$y_lote)>0)
      {
        ecotopo_year[ecot,year-2000] <- sum(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==year)),]$y_lote)/sum(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==year)),]$areal)
      }
    }
    else
    {
      year_builder_ylote <- sum(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==(year-1))&(baseprod$semestre==2)),]$y_lote)
      year_builder_areal <- sum(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==(year-1))&(baseprod$semestre==2)),]$areal)
      year_builder_ylote <- year_builder_ylote + sum(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==(year))&(baseprod$semestre==1)),]$y_lote)
      year_builder_areal <- year_builder_areal + sum(baseprod[which((baseprod$ecotopo_lote==ecotopos$Ecotopo[ecot])&(baseprod$ao==(year))&(baseprod$semestre==1)),]$areal)
      ecotopo_year[ecot,year-2000] <- year_builder_ylote/year_builder_areal
    }
  }
}

#################
# 3. Compare heat index to ecotopo_year
#################

# Uses screened_indexes from HeatIndex.R

# Get ecotopos except for rows 49 & 52 of S(sta_coords)
S_eco <- SpatialPoints(sta_coords[-c(49,52),],proj4string = CRS("+init=epsg:4326"))
S_eco <- spTransform(S_eco,crs(ecotopos))
S_eco <- over(S_eco,ecotopos)$Ecotopo

ecotopo_heat_index <- matrix(NA, 87, 10)

for (year in 1:10)
{
  for (ecot in 1:87)
  {
    if (length(which(S_eco==ecotopos$Ecotopo[ecot]))>0)
    {
      ecotopo_heat_index[ecot,year] <- mean(screened_indexes[which(S_eco==ecotopos$Ecotopo[ecot]),year+19],na.rm=T)
    }
  }
}

# Match production data with heat indexes

to_regress <- matrix(NA,1,2)

for (j in 1:10)
{
  for (i in 1:87)
  if ((!is.na(ecotopo_year[i,j]))&(!is.na(ecotopo_heat_index[i,j])))
  {
    to_regress <- rbind(to_regress, cbind(ecotopo_year[i,j],ecotopo_heat_index[i,j]))
  }
}

plot(to_regress,xlab="mean(y_lote/areal)", ylab="Mean Heat Index",main="Correlating Production Data & Heat Indexes")

#################
# 4. Filter production records by distance/elevation from a weather station
#################

# Uses station coords S and SCC from HeatIndex.R and HeatIndexCenicafe.R

all_sta_coords <- spRbind(S,SCC)
all_sta_coords <- spTransform(all_sta_coords,crs(prod_coords_3116))

distNN = get.knnx(coordinates(all_sta_coords),coordinates(prod_coords_3116),k=1)
length(which(distNN$nn.dist<10000))


