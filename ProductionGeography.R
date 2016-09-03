# ProductionGeography.R
# Catchall for working with the production data sets

#################
# 1. Use a Digital Elevation Model (DEM) to get the elevation at each of the production record points
#################

library(sp)
library(raster)
library(maptools)
library(FNN)

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

S_prod <- SpatialPoints(sta_coords[-c(49,52),],crs(SCC))
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

#################
# 3. sum(y_lote)/sum(areal) at each weather station/year for nearby stations
# 4. Filter by 2 km, 5 km, 10 km.
# 5. Filter by +/- 100m, 250m, 500m difference in altitude
#################

baseprod <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/BaseProd.csv")

station_year <- matrix(NA, length(prod_timing), 10)

distances <- c(2000,5000,10000) # in meters

for (d in 1:length(distances))
{
  for (i in 1:length(prod_timing))
  {
    to_sum <- which((distNN$nn.index==i)&(distNN$nn.dist<distances[d]))
    for (j in 1:10)
    {
      if (prod_timing[i]==2)
      {
        if (length(which(distNN$nn.index==i))>0)
        {
          station_year[i,j] <- sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000))),]$y_lote)/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000))),]$areal)
        }
      }
      else
      {
        station_year[i,j] <- sum(baseprod[intersect(to_sum,which((baseprod$ao==j+1999)&(baseprod$semestre==2))),]$y_lote)/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+1999)&(baseprod$semestre==2))),]$areal)
        station_year[i,j] <- station_year[i,j] + sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000)&(baseprod$semestre==1))),]$y_lote)/sum(baseprod[intersect(to_sum,which((baseprod$ao==j+2000)&(baseprod$semestre==1))),]$areal)
      }
    }
  }
  write.csv(station_year,paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/station_year",distances[d],".csv"))
}

#################
# 6. Match screened indexes against production estimate for that year
#################

to_regress <- matrix(NA,1,2)

par(mfrow=c(2,2),mar=rep(4,4))

for (d in 1:length(distances))
{
  station_year <- read.csv(paste0("/Users/mtnorton/Coffee_Ins_Heat_Index/station_year",distances[d],".csv"))
    for (i in 1:length(prod_timing))
    {
      if (i <= 57) # govt stations
      {index_rec <- screened_indexes[i,19:28]} else {index_rec <- screened_indexes_cc[i-57,13:24]}

      for (j in 1:10)
      {
        if ((!is.na(station_year[i,j]))&(!is.na(index_rec[j])))
        {
          to_regress <- rbind(to_regress, cbind(index_rec[j],station_year[i,j]))
        }
      }
    }
  plot(to_regress,main=paste0("Distances < ",distances[d]," meters"),xlab="Heat Index",ylab="Prod. Est.")
}

summary(lm(to_regress[,2]~to_regress[,1]))

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


