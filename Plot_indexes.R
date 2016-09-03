# Plot_indexes.R
# Used to plot index strikepoints, etc.
# Uses screened_indexes variable from HeatIndex.R 
# And screened_indexes_cc from HeatIndexCenicafe.R 

#############
# Set up regression for trendline
#############

# Need 57 * 31 for Govt. stations
# Need 44 * 25 for CC
# Total of 2867

all_indexes <- matrix(NA, 2924, 2) # index in 1st column, elevation in 2nd

for (i in 1:57)
{
  all_indexes[(((i-1)*31)+1):(i*31),1] <- rep(cens_sta_output[i],31)
  all_indexes[(((i-1)*31)+1):(i*31),2] <- screened_indexes[i,]
}

for (i in 1:44)
{
  # Dropping the one station that's at crazy altitude. Need to fix this later at an earlier point.
  if (i!=4)
  {
    all_indexes[(((i-1)*25)+1768):(i*25+1767),1] <- rep(sta_output_cc[i,1],25)
    all_indexes[(((i-1)*25)+1768):(i*25+1767),2] <- screened_indexes_cc[i,]
  }
}

#############
# Run regression
#############

reg_results <- summary(lm(as.numeric(all_indexes[,2]) ~ as.numeric(all_indexes[,1])))

#############
# PLOT
#############

plot(rep(1,32),indexes[1,],col='white',ylim=c(0000,7000),xlim=c(700,2300),xlab="ELEVATION",ylab="Heat Index")

for (i in 1:57)
{
  points(rep(cens_sta_output[i],31),screened_indexes[i,])
}

for (i in 1:44)
{
  points(rep(sta_output_cc[i,1],25),screened_indexes_cc[i,],col='red') #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
}

lines(c(0,2500),c(reg_results$coefficients[1],reg_results$coefficients[1]+reg_results$coefficients[2]*2500))
text(1500, 6700, labels = paste0("y = ",round(reg_results$coefficients[2],3),"x + ",round(reg_results$coefficients[1],3)))
text(1500, 6000, labels = paste0("R^2 = ",round(reg_results$r.squared,3)))

#############
# Count payouts for (strike = 3000) by year
#############

perc_payouts <- vector("numeric",31)
strike <- 3000

for (i in 1:31)
{
  perc_payouts[i] <- length(which(screened_indexes[,i]<strike))
  num_indexes <- (57-length(which(is.na(screened_indexes[,i]))))
  if (i > 6)
  {
    perc_payouts[i] <- perc_payouts[i] + length(which(screened_indexes_cc[,i-6]<strike))
    num_indexes <- num_indexes + (45-length(which(is.na(screened_indexes_cc[,i-6]))))
  }
  perc_payouts[i] <- perc_payouts[i]/num_indexes
}

plot(1989:2013,perc_payouts[7:31],ylab="% Payouts",xlab="Year")
title(main=paste0("Strikepoint = ",strike))

strikes = 1:40*100+2000 #c(2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500)
perc_payouts <- vector("numeric",5)

for (i in 1:length(strikes))
{
  perc_payouts[i] <- length(which(screened_indexes<strikes[i]))
  num_indexes <- length(which(!is.na(screened_indexes)))
  perc_payouts[i] <- perc_payouts[i] + length(which(screened_indexes_cc<strikes[i]))
  num_indexes <- num_indexes + length(which(!is.na(screened_indexes_cc)))
  perc_payouts[i] <- perc_payouts[i]/num_indexes
}

plot(strikes,perc_payouts[1:length(strikes)],ylab="% Payouts",xlab="Strikepoint")
title(main="Percentage of payouts for different index values")

