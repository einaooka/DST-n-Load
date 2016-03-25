###############################################
##
##  Daylight Saving Time & Energy Consumption
##
##  author: Eina Ooka
##  created: Mar 2016
##  
###############################################

## =========================================
## Daily Data 
## =========================================

library(tea.datetime)
library(tea.color)
library(tea.eo.plots)
library(reshape)

# Get and clean data
load.df <- read.csv("C:/Users/murphyeo/Desktop/Load.csv")
load.df$Date <- as.Date1(load.df$Date)
load.df$Daily.Load <- as.numeric(gsub(",", "", load.df$Daily.Load))
load.df <- na.omit(load.df)

# Key dates
forward.dates <- as.Date(c("2010-3-14", "2011-3-13", "2012-3-11", "2013-3-10", "2014-3-9", "2015-3-8", "2016-3-13"))
backward.dates <- as.Date(c("2010-11-7", "2011-11-6", "2012-11-4", "2013-11-3", "2014-11-2", "2015-11-1", "2016-11-6"))

# Define a binary "Daylight Saving Time"
load.df$DST <- TRUE # Data starts on "2010-9-17"
load.df$DST[load.df$Date %in% forward.dates] <- NA
load.df$DST[load.df$Date %in% backward.dates] <- NA
isSummer <- 0
for (i in 1:nrow(load.df)){
  if(is.na(load.df$DST[i])) {isSummer <- isSummer +1
  } else if(isSummer%%2 ==1) load.df$DST[i] <- FALSE
}

# Define time variables
load.df$Year <- YearFromDate(load.df$Date)
load.df$Month <- MonthFromDate(load.df$Date)
load.df$DOW <- as.factor(DOW(load.df$Date))
load.df$Weekend <- load.df$DOW %in% c("Saturday", "Sunday")

## Day length
DayOfYear <- function(date) as.numeric(format(date,"%j"))
load.df$DayOfYear <- DayOfYear(load.df$Date)
load.df$DayLength <- cos((load.df$DayOfYear-DayOfYear(as.Date("2010-6-21")))*2*pi/365)

## =========================================
## Adjust Load for temperatures
## =========================================

load.df$temp.adj <- NA
for (i in 1:2){
  
  # Nov or Mar
  if (i ==1){ key.date <- round(mean(DayOfYear(forward.dates)), digits = 0)
  } else { key.date <- round(mean(DayOfYear(backward.dates)), digits = 0) }
  
  # 30 days around the key dates 
  filtr <- DayOfYear(load.df$Date) %in% seq(key.date-30, key.date+30,1)
  
  # Smooth spline fit for high and low temperatures
  par(mfrow=c(1,2), mar=c(4,4,4,1) + 0.1, oma=c(0,0,0,0))
  plot(load.df$Temp.Low[filtr], load.df$Daily.Load[filtr], xlab="Low Temperature", ylab="Daily Load", col=gray(0.5))
  fit1 <- smooth.spline(load.df$Temp.Low[filtr], load.df$Daily.Load[filtr])
  lines(fit1, col=blue.f(1), lwd=3)
  
  plot(load.df$Temp.High[filtr], load.df$Daily.Load[filtr], xlab="High Temperature")
  fit2 <- smooth.spline(load.df$Temp.High[filtr], load.df$Daily.Load[filtr])
  lines(fit2, col=blue.f(1), lwd=3)
  
  readline("Press <return to continue") 
  
  # Take the avarage of the 2 spline fits
  temp <- predict(fit1, load.df$Temp.Low[filtr])
  temp <- (temp$y + predict(fit2, load.df$Temp.High[filtr])$y)/2
  load.df$temp.adj[filtr] <- load.df$Daily.Load[filtr] - temp + predict(fit2, mean(load.df$Temp.High[filtr]))$y
  
  # Visualize adjusted Data
  Layout0()
  temp.df <- cast(load.df, DayOfYear ~ Year, value="temp.adj", subset=filtr)
  PlotMatStochastic(temp.df$DayOfYear, temp.df[,2:6], blue.f, main=ifelse(i==1,"March", "November"))
  matlines(temp.df$DayOfYear, temp.df[,-1], lty=1, col=gray.f(0.7))
  
  readline("Press <return to continue") 
  
}

## =========================================
##  Select a week before and after key dates
## =========================================

idx <-  c(1:nrow(load.df))[is.na(load.df$DST)]
temp <- rep(-7:7, each=length(idx))
idx <- rep(idx,15) + temp
load.filtrd.df <- load.df[idx,]
load.filtrd.df$DaysFromKeyDates <- temp
load.filtrd.df <- load.filtrd.df[order(load.filtrd.df$Date),]
load.filtrd.df  <- load.filtrd.df[!is.na(load.filtrd.df$Date),]

# Separate for March and November
load.filterd.3.df <- load.filtrd.df[load.filtrd.df$Month %in% c(3),]
load.filterd.11.df <- load.filtrd.df[load.filtrd.df$Month %in% c(10,11),]

# Visualize adjusted Data
par(mfrow=c(1,2), mar=c(4,4,4,1) + 0.1, oma=c(0,0,0,0))
temp.df <- cast(load.filterd.3.df, DaysFromKeyDates ~ Year, value="Daily.Load")
PlotMatStochastic(temp.df$DaysFromKeyDates, temp.df[,2:6], blue.f, main="March", xlab="Days +/- Key Date", ylab="Daily Load")
matlines(temp.df$DaysFromKeyDates, temp.df[,-1], lty=1, col=gray.f(0.7))
temp.df <- cast(load.filterd.11.df, DaysFromKeyDates ~ Year, value="Daily.Load")
PlotMatStochastic(temp.df$DaysFromKeyDates, temp.df[,2:6], blue.f, main="November", xlab="Days +/- Key Date", ylab="Daily Load")
matlines(temp.df$DaysFromKeyDates, temp.df[,-1], lty=1, col=gray.f(0.7))

# Unadjusted Load
par(mfrow=c(1,2), mar=c(4,3,4,1) + 0.1, oma=c(0,0,0,0))
boxplot(Daily.Load ~ DST, data=load.filterd.11.df, main="Nov 1st Sunday")
boxplot(Daily.Load ~ DST, data=load.filterd.3.df, main="March 2nd Sunday")

# Temperature adjusted
boxplot(temp.adj ~ DST, data=load.filterd.11.df, main="Nov 1st Sunday")
boxplot(temp.adj ~ DST, data=load.filterd.3.df, main="March 2nd Sunday")

#Kolmogorov-Smirnov test
ks.test(load.filterd.11.df$Daily.Load[load.filterd.11.df$DST], load.filterd.11.df$Daily.Load[!load.filterd.11.df$DST])
ks.test(load.filterd.11.df$temp.adj[load.filterd.11.df$DST], load.filterd.11.df$temp.adj[!load.filterd.11.df$DST])

ks.test(load.filterd.3.df$Daily.Load[load.filterd.11.df$DST], load.filterd.3.df$Daily.Load[!load.filterd.11.df$DST])
ks.test(load.filterd.3.df$temp.adj[load.filterd.11.df$DST], load.filterd.3.df$temp.adj[!load.filterd.11.df$DST])

## =========================================
## Random Forest
## =========================================

library(caret)
ctrl <- trainControl(method= "boot", number=100)

# Choose November or March
data.df=load.filterd.3.df
data.df=load.filterd.11.df

# First fit
fit <- train(Daily.Load ~ Temp.Low + Temp.High + Year + Weekend +DayLength + DST
             , data=data.df, method="rf", trControl = ctrl)
fit.importance <- importance(fit$finalModel)
importance.order <- rownames(fit.importance)[order(fit.importance, decreasing = TRUE)]
importance.order <- gsub("TRUE", "", importance.order)
par(mfrow=c(1,2), mar=c(4,4,4,1) + 0.1, oma=c(0,0,0,0))
varImpPlot(fit$finalModel, main=paste("RF:", MonthName(data.df$Month[8])))

# re-run the model with different # of predictors 
nvars <- length(importance.order)
rmse.compare <- rep(NA, nvars)
for (i in 1:nvars){
  temp.df <- data.df[,c("Daily.Load",importance.order[1:i])]
  fit <- train(Daily.Load~. , data=temp.df, method="rf", trControl = ctrl)
  importance(fit$finalModel)
  rmse.compare[i] <- min(fit$results$RMSE)
}

# Naive model
fit <- lm(Daily.Load ~ x, data=data.frame(data.df, x=1))
rmse.compare <- c(RMSE(predict(fit), data.df$Daily.Load, na.rm = FALSE), rmse.compare)

# Result RMSE
plot(0:(nvars+1), c(rmse.compare,NA), "b", axes=FALSE, xlab="Predictors", ylab="RMSE")
axis(1, at=0:nvars, labels = c("None", importance.order))
axis(2)
text(0:nvars, rmse.compare, paste0(round(rmse.compare*100/rmse.compare[1]), "%"), pos=4)
box(col=gray(0.5))

## =========================================
## Hourly Data
## =========================================

load_hrly.df <- read.csv("C:/Users/murphyeo/Desktop/HourlyLoad.csv")
load_hrly.df$Date <- as.Date0(load_hrly.df$Date)
load_hrly.df$Load <- as.numeric(gsub(",", "", load_hrly.df$Load))
load_hrly.df$DayOfYear <- DayOfYear(load_hrly.df$Date)
load_hrly.df$Year <- YearFromDate(load_hrly.df$Date)

# Choose November or March
data.df <- load.filterd.3.df
data.df <- load.filterd.11.df

dates <- data.df$Date
load_hrly.filtrd.df <- load_hrly.df[load_hrly.df$Date %in% dates,]
load_hrly.filtrd.df$DaysFromKeyDates <- data.df$DaysFromKeyDates[match(load_hrly.filtrd.df$Date, data.df$Date)]
temp.df <- cast(load_hrly.filtrd.df, Year + DaysFromKeyDates ~ Hour, value="Load", mean) 
for (i in 3:26) temp.df[,i] <- na.approx(temp.df[,i], na.rm=FALSE)

par(mfrow=c(1,2), mar=c(4,4,4,1) + 0.1, oma=c(0,0,0,0))
PlotMatStochastic(1:24, t(temp.df[temp.df$DaysFromKeyDates < 0,3:26]), blue.f, main=MonthName(data.df$Month[8]), ylab="Hourly Load")
PlotMatStochastic(1:24, t(temp.df[temp.df$DaysFromKeyDates > 0,3:26]), orange.f, Add=TRUE)

# Adjust for temperature
if (FALSE){
Layout12()
for(h in seq(0,22,2)){
  filtr <- load_hrly.filtrd.df$Hour %in% c(h,h+1)
  plot(load_hrly.filtrd.df$Temperature[filtr], load_hrly.filtrd.df$Load[filtr], xlim=c(30,70), ylim=c(4000,8000))
  title(main=h, line=-3)
}
}
load_hrly.filtrd.df$temp.adj <- NA
for(h in 0:23){
 
  filtr <- load_hrly.filtrd.df$Hour %in% (c(h-1, h, h+1)%%24) & !is.na(load_hrly.filtrd.df$Temperature) & !is.na(load_hrly.filtrd.df$Load)
  filtr <- filtr & !is.na(load_hrly.filtrd.df$Temperature) & !is.na(load_hrly.filtrd.df$Load)
  temp.df <- load_hrly.filtrd.df[filtr, ]
  fit <- smooth.spline(temp.df$Temperature, temp.df$Load)
  
  filtr <- load_hrly.filtrd.df$Hour == h & !is.na(load_hrly.filtrd.df$Temperature)
  load_hrly.filtrd.df$temp.adj[filtr] <- predict(fit,load_hrly.filtrd.df$Temperature[filtr])$y
  load_hrly.filtrd.df$temp.adj[filtr] <- load_hrly.filtrd.df$Load[filtr] - load_hrly.filtrd.df$temp.adj[filtr]
  load_hrly.filtrd.df$temp.adj[filtr] <- load_hrly.filtrd.df$temp.adj[filtr] + predict(fit,mean(load_hrly.filtrd.df$Temperature[filtr]))$y
}
  
temp.df <- cast(load_hrly.filtrd.df, Year + DaysFromKeyDates ~ Hour, value="temp.adj", mean) 
for (i in 3:26) temp.df[,i] <- na.approx(temp.df[,i], na.rm=FALSE)
PlotMatStochastic(1:24, t(temp.df[temp.df$DaysFromKeyDates < 0,3:26]), blue.f, main=MonthName(data.df$Month[8])
                  , ylab="Temperature Adjusted Hourly Load")
PlotMatStochastic(1:24, t(temp.df[temp.df$DaysFromKeyDates > 0,3:26]), orange.f, Add=TRUE)


# March sunrise & sunset
sunrise <- c(6 + 27/60, 7+25/60)
sunset <- c(18+12/60, 19+13/60)

# November sunrise & sunset
sunrise <- c(7 + 54/60, 6+55/60)
sunset <- c(17+52/60, 16+50/60)

abline(v=sunrise, col=c(blue.f(0.8), orange.f(0.8)), lwd=2)
abline(v=sunset, col=c(blue.f(0.8), orange.f(0.8)), lwd=2)

legend("bottomright"
       , inset=.05
       , c("before key date", "after key date")
       , col=c(blue.f(0.5), orange.f(0.5))
       , lty=1
       , lwd=2
       , bg="white")








