# HeatIndexCenicafe.R
# (CC means cenicafe in variable names below)
# Will create heat index for cenicafe stations (only)
# Will also provide station coordinates for cenicafe stations (only)

# Gov't and CC data is in different format, so code is different
# Here: get a year of data as a column starting 8/1 of each year, take sum

rawdata_cc <- read.csv("/Users/mtnorton/Dropbox/temp/HeatIndex/cenicafe2.csv")

######################
# Station Coordinates
######################
stations_cc <- as.matrix(read.csv("/Users/mtnorton/Dropbox/temp/CCData/CCstations_wo_names.csv",header=FALSE))

sta_coords_cc <- stations_cc[,10:9]

######################
# Get start timing by department
# NOT USED ANYMORE
# Now we calculate 2 different indexes on each station
######################
dept_bound <- shapefile("/Users/mtnorton/Coffee_Ins_Heat_Index/COL_adm/COL_adm1.shp")
dept_bound$timing <- c(0,2,0,0,0,3,2,0,0,1,3,0,0,1,0,0,1,3,3,0,1,4,0,4,2,0,5,0,1,4,0,0)

SCC <- SpatialPoints(sta_coords_cc)
crs(SCC) <- CRS("+init=epsg:4326")

SCC <- spTransform(SCC,crs(dept_bound))

station_timing_cc <- over(SCC,dept_bound)$timing
dept_start_month_cc <- c(7,2,2,7,2)
start_month_cc <- dept_start_month_cc[station_timing_cc]
# Insert placeholders of 1 for stations w/o coordinates
# Missing stations: 1,10,14,16,20,48
ph <- 1
start_month_cc <- c(ph,start_month_cc[1:8],ph,start_month_cc[9:11],ph,start_month_cc[12],ph,start_month_cc[13:15],ph,start_month_cc[16:42],ph,start_month_cc[43:44])

######################
# Read data from CSV
######################
indexes_cc <- matrix(NA, 50, 26)
total_missing_cc <- matrix(NA, 50, 26)

sta_names <- matrix(NA, 50, 3)

#Initialize
sta1 <- rawdata_cc[1,1]
sta2 <- rawdata_cc[1,2]
sta3 <- rawdata_cc[1,3]

sta_names[1,1:3] <- as.matrix(rawdata_cc[1,1:3])


sta_count <- 1
# too_many_missing <- FALSE
# init <- TRUE

# Expected missing per month
month_length <- c(31,28,31,30,31,30,31,31,30,31,30,31)

for (i in (1:nrow(rawdata_cc)))
{
  year <- rawdata_cc[i,4]
  month <- rawdata_cc[i,5]
  
  if (!((rawdata_cc[i,1]==sta1) & (rawdata_cc[i,2]==sta2) & (rawdata_cc[i,3]==sta3)))
  {
    sta_count <- sta_count+1
    sta1 <- rawdata_cc[i,1]
    sta2 <- rawdata_cc[i,2]
    sta3 <- rawdata_cc[i,3]
    sta_names[sta_count,1:3] <- as.matrix(rawdata_cc[i,1:3])
    #init <- TRUE
  }
  
  if (month > start_month_cc[sta_count]) 
  {ind_year <- year-1988}
  else 
  {ind_year <- year-1989}
    
  if ((month==(start_month_cc[sta_count])) & (rawdata_cc[i,6]==1))
  {  
    if (month==7) {numdays <- 215}
    else {numdays <- 211}
    if (year%%4==3) {numdays <- numdays+1}
    # Make it as own object
    mo_data <- rawdata_cc[i:(i+numdays),7]-10
    mo_data <- sum(mo_data,na.rm=TRUE)
    
    # Count missing
    total_missing_cc[sta_count, ind_year] <- length(which(is.na(rawdata_cc[i:(i+numdays),7])))
    
    indexes_cc[sta_count, ind_year] <- mo_data
      
    #i <- i + 365
    #if (rawdata_cc[i,3]%%4==0 & month==2) {i <- i + 1}
    
  }
  print(paste(i, month, year, ind_year, mo_data))
        
}

# delete last year of data
indexes_cc <- indexes_cc[,-26]
total_missing_cc <- total_missing_cc[,-26]
# delete rows 1, 10, 14, 16, 20, 48
# Not in coordinates file
indexes_cc <- indexes_cc[-c(1,10,14,16,20,48),]
total_missing_cc <- total_missing_cc[-c(1,10,14,16,20,48),]

sta_output_cc <- matrix(NA, 44, 3)

total_missing365_cc[which(start_month_cc==7),] <- total_missing_cc[which(start_month_cc==7),]/215
total_missing365_cc[which(start_month_cc==2),] <- total_missing_cc[which(start_month_cc==2),]/212
screened_indexes_cc <- indexes_cc
screened_indexes_cc[which(total_missing365_cc>0.05)] <- NA



plot(rep(1,25),indexes_cc[1,],col='white',ylim=c(-2000,7000),xlim=c(700,2300),xlab="ELEVATION",ylab="Heat Index")

for (i in 1:44)
{
  sta_output_cc[i,1:3] <- as.matrix(stations_cc[i,c(2,9:10)])
  points(rep(sta_output_cc[i,1],25),screened_indexes_cc[i,],col='red') #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
}

output_cc <- cbind(unique(rawdata_cc[,2]),indexes_cc,sta_output_cc)
write.csv(output_cc,"/Users/mtnorton/Dropbox/temp/HeatIndex/HeatIndexCC.csv")

