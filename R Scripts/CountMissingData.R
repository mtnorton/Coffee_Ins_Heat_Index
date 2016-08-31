# CountMissingData.R
# 

rawdata <- read.csv("/Users/mtnorton/Dropbox/temp/HeatIndex/medtemp.csv")

# up to 22 years of data at 59 stations, matrix of 59x22

total_missing <- matrix(NA, 59, 32)

#Initialize
sta <- rawdata[1,2]
sta_count <- 1
#too_many_missing <- FALSE
init <- TRUE

# Expected missing per month
mo_exp_missing <- c(0,3,0,1,0,1,0,0,1,0,1,0)

for (i in (1:nrow(rawdata)))
{
  year <- rawdata[i,3]
  month <- rawdata[i,4]
  
  if (rawdata[i,2]!=sta) 
  {
    sta_count <- sta_count+1
    sta <- rawdata[i,2]
    init <- TRUE
  }
  
  if (month > 7) 
  {ind_year <- year-1982}
  else 
  {ind_year <- year-1983}
  
  if (init==TRUE) 
  {if (rawdata[i,4]==7) {init = FALSE}}
  else
  {
    # Make it as own object
    mo_data <- rawdata[i,5:35]
    
    # Count missing
    num_missing <- length(which(mo_data==-99))
    # Look up # expected to be missing by month
    exp_missing <- mo_exp_missing[month]
    if (rawdata[i,3]%%4==0 & month==2) {exp_missing <- exp_missing-1} 
    
    print(paste(i,num_missing,exp_missing))
    
    num_missing <- num_missing - exp_missing
    
    #as matrix
    total_missing[sta_count, ind_year] <- sum(c(total_missing[sta_count, ind_year], num_missing),na.rm=TRUE)
    
  }
  #print(paste(i,num_missing))
}

#as vector
total_days_missing <- as.vector(total_missing)
plot(ecdf(total_days_missing),xlab='Days of missing data',main='CDF of missing days of data',pch=1,ylab="Cumulative Density of Missing Days")

############## 
# FIGURE OUT WHAT PERCENTAGE OF YEARS ARE COMPLETE

# Using output and total_missing variable from HeatIndex.R
# Using total_missing_cc from HeatIndexCenicafe.R
# Using ecoto_merge from plot_dept.R

total_missing365 <- (total_missing/365)
total_missing365[which(is.na(output[,2:33]))] <- NA

# Set NAs by hand for station records which are not started yet
total_missing365[1,1:7] <- NA
total_missing365[14,1] <- NA
total_missing365[36,1] <- NA
total_missing365[41,1:3] <- NA
total_missing365[54,1:30] <- NA
total_missing365[,32] <- NA

ecoto_reg_merge <- over(sta_temp,ecotopos)$Ecotopo

regs <- output[,34]
regs[which(is.na(ecoto_reg_merge))] <- NA

length(which(total_missing365[which(regs=="1A"),]<0.05))/length(total_missing365[which(regs=="1A"),])
length(which(total_missing365[which(regs=="1A"),]>0.15))/length(total_missing365[which(regs=="1A"),])

length(which(total_missing365[which(regs=="1B"),]<0.05))/length(total_missing365[which(regs=="1B"),])
length(which(total_missing365[which(regs=="1B"),]>0.15))/length(total_missing365[which(regs=="1B"),])

length(which(total_missing365[which(regs=="2A"),]<0.05))/length(total_missing365[which(regs=="2A"),])
length(which(total_missing365[which(regs=="2A"),]>0.15))/length(total_missing365[which(regs=="2A"),])

length(which(total_missing365[which(regs=="2B"),]<0.05))/length(total_missing365[which(regs=="2B"),])
length(which(total_missing365[which(regs=="2B"),]>0.15))/length(total_missing365[which(regs=="2B"),])

length(which(total_missing365[which(regs=="3A"),]<0.05))/length(total_missing365[which(regs=="3A"),])
length(which(total_missing365[which(regs=="3A"),]>0.15))/length(total_missing365[which(regs=="3A"),])

length(which(total_missing365[which(regs=="3B"),]<0.05))/length(total_missing365[which(regs=="3B"),])
length(which(total_missing365[which(regs=="3B"),]>0.15))/length(total_missing365[which(regs=="3B"),])

length(which(total_missing365[which(regs=="4"),]<0.05))/length(total_missing365[which(regs=="4"),])
length(which(total_missing365[which(regs=="4"),]>0.15))/length(total_missing365[which(regs=="4"),])

total_missing365 <- (total_missing_cc/365)
# Rerun ecoto_merge from plot_dept.R here
ecoto_reg_merge <- over(sta_coords,ecotopos)$Ecotopo
ecoto_reg_merge <- paste0(substr(ecoto_reg_merge,1,1),substr(ecoto_reg_merge,4,4))
ecoto_reg_merge[which(ecoto_reg_merge=="NANA")] = NA
regs <- ecoto_reg_merge

length(which(total_missing365[which(regs=="1A"),]<0.05))/length(total_missing365[which(regs=="1A"),])
length(which(total_missing365[which(regs=="1A"),]>0.15))/length(total_missing365[which(regs=="1A"),])

length(which(total_missing365[which(regs=="1B"),]<0.05))/length(total_missing365[which(regs=="1B"),])
length(which(total_missing365[which(regs=="1B"),]>0.15))/length(total_missing365[which(regs=="1B"),])

length(which(total_missing365[which(regs=="2A"),]<0.05))/length(total_missing365[which(regs=="2A"),])
length(which(total_missing365[which(regs=="2A"),]>0.15))/length(total_missing365[which(regs=="2A"),])

length(which(total_missing365[which(regs=="2B"),]<0.05))/length(total_missing365[which(regs=="2B"),])
length(which(total_missing365[which(regs=="2B"),]>0.15))/length(total_missing365[which(regs=="2B"),])

length(which(total_missing365[which(regs=="3A"),]<0.05))/length(total_missing365[which(regs=="3A"),])
length(which(total_missing365[which(regs=="3A"),]>0.15))/length(total_missing365[which(regs=="3A"),])

length(which(total_missing365[which(regs=="3B"),]<0.05))/length(total_missing365[which(regs=="3B"),])
length(which(total_missing365[which(regs=="3B"),]>0.15))/length(total_missing365[which(regs=="3B"),])

length(which(total_missing365[which(regs=="4"),]<0.05))/length(total_missing365[which(regs=="4"),])
length(which(total_missing365[which(regs=="4"),]>0.15))/length(total_missing365[which(regs=="4"),])