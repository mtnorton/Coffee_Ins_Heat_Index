# HeatIndexUpdate.R
# Will create heat index for gov't stations (only)
# Will also provide station coordinates for gov't stations (only)
# Updated with 1) additional gov't data Sept 2016 2) two indexes instead of one
# Needs variables from plot_dept.R on department timing
# Some other code at bottom

# NOTE THAT NOT ALL OF THE CODE HAS BEEN UPDATED WITH THE 2 INDEX UPDATE

# Gov't and CC data is in different format, so code is different
# Here: Data is in months as a row, so read and add each row for annual sums

library(sp)
library(raster)

rawdata <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/data/IDEAM_06092016.csv")

rawdata <- rawdata[which(rawdata[,3]=="MEDIA"),-c(3,52)]  

# up to 32 years of data at 59 stations, matrix of 59x32

n_sta <- length(unique(rawdata[,1]))
sta_nums <- unique(rawdata[,1])

######################
# Station Coordinates
######################

sta_coords <- matrix(NA,n_sta,2)
sta_elevs <- vector("numeric",n_sta)

for (i in 1:n_sta)
{
  sta_coords[i,2] <- unique(rawdata[which(rawdata[,1]==sta_nums[i]),46])
  sta_coords[i,1] <- unique(rawdata[which(rawdata[,1]==sta_nums[i]),47])
  sta_elevs[i] <- unique(rawdata[which(rawdata[,1]==sta_nums[i]),48])
}


S <- SpatialPoints(sta_coords)
crs(S) <- CRS("+init=epsg:4326")

######################
# Get start timing by department
# NOT USED ANYMORE
# Now we calculate 2 different indexes on each station
######################
dept_bound <- shapefile("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_adm/COL_adm1.shp")

dept_bound$timing <- c(0,2,0,0,0,3,2,0,0,1,3,0,0,1,0,0,1,3,3,0,1,4,0,4,2,0,5,0,1,4,0,0)

S <- spTransform(S,crs(dept_bound))

station_timing <- over(S,dept_bound)$timing
dept_start_month <- c(7,2,2,7,2) # By months
dept_end_month <- c(2,9,9,2,9) # Month after end
start_month <- dept_start_month[station_timing]
end_month <- dept_end_month[station_timing]

# Expected missing per month
mo_exp_missing <- c(0,3,0,1,0,1,0,0,1,0,1,0)

######################
# Read CSV file & Calcuate indexes
######################

indexes1 <- matrix(NA, n_sta, 34)
indexes2 <- matrix(NA, n_sta, 34)
total_missing1 <- matrix(215, n_sta, 34)
total_missing2 <- matrix(212, n_sta, 34)
sta <- rawdata[1,1]
sta_count <- 1
#too_many_missing <- FALSE
init <- TRUE

for (i in (1:nrow(rawdata)))
{
  year <- rawdata[i,2]
  month <- rawdata[i,3]
  
  if (rawdata[i,1]!=sta) 
  {
    sta_count <- sta_count+1
    sta <- rawdata[i,1]
  }
  
  # Make it as own object
  mo_data <- as.numeric(rawdata[i,4:34])
          
  # Count missing
  num_missing <- length(which(mo_data==999990))
  num_data <- length(which(mo_data!=999990))
  # Look up # expected to be missing by month
  exp_missing <- mo_exp_missing[month]
  #if (rawdata[i,2]%%4==0 & month==2) {exp_missing <- exp_missing-1} 
          
  num_missing <- num_missing - exp_missing
  mo_data <- replace(mo_data,mo_data==999990,NA)
            
  # Calculate index here
  # Subtract 10 from the total
  # Needs to be adjusted based on whatever is decided about missing data
  mo_data <- mo_data-10
  mo_data_sum <- sum(mo_data,na.rm=TRUE)
  if ((num_missing > 1) && (num_missing <= 3))
  {
    mo_data_sum <- mo_data_sum + num_missing * mean(as.numeric(mo_data),na.rm=TRUE)
  }
  
  # INDEX 1 (July 1 - January 31)
  # If between July and January, add to index 1
  #
  if (year < 2016)
  {
    if (length(intersect(month,c(1,7:12)))>0)
    {
      if (month==1) {ind_year<-year-1982}
      else {ind_year<-year-1981}
      total_missing1[sta_count,ind_year] <- total_missing1[sta_count,ind_year]-num_data
      indexes1[sta_count, ind_year] <- sum(c(indexes1[sta_count, ind_year], mo_data_sum),na.rm=TRUE)
    }
    
    # INDEX 2 (February 1 - August 31)
    # If between February and August, add to index 2
    if (length(intersect(month,c(2:8)))>0)
    {
      ind_year <- year-1982
      total_missing2[sta_count,ind_year] <- total_missing2[sta_count,ind_year]-num_data
      if (rawdata[i,2]%%4==0 & month==2 & num_data==29) {total_missing2[sta_count,ind_year] <- total_missing2[sta_count,ind_year]+1}
      indexes2[sta_count, ind_year] <- sum(c(indexes2[sta_count, ind_year], mo_data_sum),na.rm=TRUE)
    }
  }
  
  # For debugging, can comment out
  print(paste(i, month, year, ind_year, mo_data_sum, indexes1[sta_count, ind_year], indexes2[sta_count, ind_year]))
  
}

indexes1[which(indexes1>100000)] <- NA
indexes2[which(indexes2>100000)] <- NA

output <- cbind(unique(rawdata[,1]),indexes)

write.csv(output,"/Users/mtnorton/Coffee_Ins_Heat_Index/data/HeatIndex.csv")

# For index investigation

total_missing_perc_1 <- total_missing1/215
total_missing_perc_2 <- total_missing2/212
screened_indexes1 <- indexes1
screened_indexes2 <- indexes2
screened_indexes1[which(total_missing_perc_1>0.05)] <- NA
screened_indexes2[which(total_missing_perc_2>0.05)] <- NA

#OLD PLOT FOR ELEVATION

plot(rep(1,32),indexes[1,],col='white',ylim=c(0000,7000),xlim=c(700,2300),xlab="ELEVATION",ylab="Heat Index")

for (i in 1:57)
{
  points(rep(cens_sta_output[i],31),screened_indexes[i,]) #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
  #  sta_output[i,1:4] <- cbind(stations[which(stations[,3]==unique(rawdata[,2])[i]/10),c(2,7:9)])
  
  #  sta_names_govt[i] <- stations[which(stations[,3]==unique(rawdata[,2])[i]/10),4]
  
  # Mostly ignore this trendline for now. Moved to Plot_indexes.R.
  
  # Detrend for elevation with simple line
  # Not mathematical b/c of all the outliers
  # Basic formula: y = -0.5 x + 6700
  # Elevations below 3350/2 = 1675, subtract thermal units
  # Elevations above 3350/2 = 1675, subtract thermal units
  
  #elev_trend[i] <- (as.numeric(sta_output[i,2])-1675)*2
  #index_detrended[i,] <- indexes[i,]+elev_trend[i]
  
}


######################
# PLOT detrended data by latitude
######################

#plot(rep(1,32),indexes[1,],col='white',ylim=c(0000,7000),xlim=c(0,9),xlab="LATITUDE",ylab="Heat Index")
#for (i in 1:59)
#{
#  points(rep(as.numeric(sta_output[i,3]),32),index_detrended[i,])
#}
  
######################
# GEOSTATISTICS
######################

# This can be ignored as well.

plot(c(1,1),col='white',xlim=c(0,300000),ylim=c(0,1200000))
models <- matrix(NA, 300, 32)
models[,1] <- (1:300)*1000

for (i in 1:32)
{
  heat <- as.vector(index_detrended[,i])
  if (length(which(is.na(heat))) > 0) {
    coo <- sta_coords[-which(is.na(heat)),]
    heat <- heat[-which(is.na(heat))]    
  } else {coo <- coords}
  coo <- project(coo, "+proj=utm +zone=15 ellps=WGS84")
  heatdf <- data.frame(heat)
  coordinates(heatdf) <- coo
  lzn = variogram(heat~1, heatdf)
  
  lzn.fit = fit.variogram(lzn, model = vgm(psill=max(lzn$gamma), "Sph", range=300000,nugget=0))
  
  #plot(lzn, lzn.fit)
  
  if (lzn.fit$psill[1]/lzn.fit$psill[2]<0.8)
  {
    models[,i+1] <- (1.5*models[,1]/lzn.fit$range[2]-0.5*(models[,1]/lzn.fit$range[2])^3)*lzn.fit$psill[2]+lzn.fit$psill[1]
    models[which(models[,1]>lzn.fit$range[2]),i+1] <- lzn.fit$psill[1] + lzn.fit$psill[2]
    if (i>1) {lines(models[1:300,cbind(1,i)])}
  }
  
}


mean_sd <- matrix(NA, 300, 4)
mean_sd [,1] <- rowMeans(models[,-1],na.rm=TRUE)
for (i in 1:300)
{
  mean_sd[i,2] <- sd(models[i,-1],na.rm=TRUE)
}

mean_sd[,3] <- mean_sd[,1] + 1.96 * mean_sd[,2]
mean_sd[,4] <- mean_sd[,1] - 1.96 * mean_sd[,2]

#Again, blank plot for lines
plot(3343969,1683021,col='white',xlim=c(0,300000),ylim=c(0,1200000),xlab="Distance (m)",ylab="Semivariance")
lines(models[,1],mean_sd[,1])
lines(models[,1],mean_sd[,3],lty=22)
lines(models[,1],mean_sd[,4],lty=22)

