#1. Installing and Reading in Packages

#Installing Packages
install.packages("forecast")
install.packages("tidyverse")
install.packages("readxl")
install.packages("fma")
install.packages("fpp2")
install.packages("expsmooth")
install.packages("gridExtra")

#Reading in Packages
library(tidyverse)
suppressMessages(library(forecast))
library(expsmooth)
library(readxl)
library(fma)
library(fpp2)
library(gridExtra)



#2.Reading in and Preparing Data Sets

#Reading in Data Sets
GMT <- read_csv("/Users/maherp/Desktop/Forecasting Global Mean Temperature/GLB.Ts.csv", skip = 1, na = "***")
GMT_dSST <- read_csv("/Users/maherp/Desktop/Forecasting Global Mean Temperature/GLB.Ts+dSST.csv", skip = 1, na = "***")
#The first row, containing the name of the data set ist ignored and the "***" enteries are identified as missing data points

#Selecting Data Sets out of the complete Data Sets
S_GMT <- select(GMT, 'J-D')
S_GMT_dSST <- select(GMT_dSST, 'J-D')
#New data sets "S_GMT" and S_GMT_dSST are created. They contain only the Column "J-D" from the original data sets.
#The "J-D" column contains the yearly global mean temperature index.
#It is calculated by averaging out the means from january till december.
#By doing so the column specifying the year is also dropped.

#Preparation of the in-sample Data Sets
F_S_GMT <-S_GMT[-c(121:140), ]+1
F_S_GMT_dSST <-S_GMT_dSST[-c(121:140), ]+1
#The Rows 121 till 140 are taken out, they correspond to the years 2001 till 2019.
#Therefor the data before 2001 is in sample where as the data in 2001 and after is out of sample.

#Preparation of original Data Sets
F_S_GMT_original <-S_GMT[-c(140), ]+1
F_S_GMT_dSST_original <-S_GMT_dSST[-c(140), ]+1
#The Row 140, which corresponds to the year 2019 is filtered out because of missing values.

#3.Time Series Definition and Ploting of the Data Sets

#Time Series Definition of the in sample Data Sets
ts_GMT <- ts(F_S_GMT, frequency = 1,start = c(1880,1))
ts_GMT_dSST <- ts(F_S_GMT_dSST, frequency = 1,start = c(1880,1))
#Time Series Definition of the out of sample Data Sets
ts_GMT_original <- ts(F_S_GMT_original, frequency = 1,start = c(1880,1))
ts_GMT_dSST_original <- ts(F_S_GMT_dSST_original, frequency = 1,start = c(1880,1))
#The data sets are turned into time series to allow for an analysis with the fpp2 package.
#The starting year 1880 is set since the year column was edited out in the Ddta preperation.

#Time Series Plot of the GMT Data. (LOTI)
autoplot(ts_GMT[,"J-D"]) +
  ggtitle("GMT") +
  xlab("Year") +
  ylab("GMT Index")
#Time Series Plot of the GMT_dSST Data. (dSt)
autoplot(ts_GMT_dSST[,"J-D"]) +
  ggtitle("GMT_dSST") +
  xlab("Year") +
  ylab("GMT_dSST Index")


#4.The Models


#Linear Trend Model 1


#GMT Data Set

#Defining the Time Variable
time=c(1:120)

#Defining the Exponential Trend Model 1
m1=tslm(ts_GMT~time)
summary(m1)

#Plotting Residuals
autoplot(m1$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m1$residuals,lag=20)
ggPacf(m1$residuals, lag=20)
auto.arima(m1$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 1 including the ARMA Component
m1_arma=Arima(ts_GMT, order = c(1,0,0), xreg = cbind(time))
#Diagnostics of Model 1 with ARMA
tsdiag(m1_arma, gof=20)


#Forecast of Model 1 with ARMA
fm1_arma=forecast(m1_arma, xreg = cbind(time=length(time)+1:19))

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 1 with ARMA
Plot_m1 <- autoplot(ts_GMT_original[,"J-D"], series="Data") +
  autolayer(forecast(fm1_arma), series="Forecast") + 
  autolayer(ts_GMT_original[,"J-D"], series="Data") + 
  autolayer(fitted(m1_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 1") +
  guides(colour=guide_legend(title=" "))
Plot_m1

#GMT_dSST Data Set

#Defining the Exponential Trend Model 1
m1_dSST=tslm(ts_GMT_dSST~time)
summary(m1_dSST)

#Plotting Residuals
autoplot(m1_dSST$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m1_dSST$residuals,lag=20)
ggPacf(m1_dSST$residuals, lag=20)
auto.arima(m1_dSST$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 1 including the ARMA Component
m1_dSST_arma=Arima(ts_GMT_dSST, order = c(1,0,0), xreg = cbind(time))
#Diagnostics of Model 1 with ARMA
tsdiag(m1_dSST_arma, gof=20)

#Forecast of Model 1 with ARMA
fm1_dSST_arma=forecast(m1_dSST_arma, xreg = cbind(time=length(time)+1:19))

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 1 with ARMA
Plot_m1_dSST <- autoplot(ts_GMT_dSST_original[,"J-D"], series="Data") +
  autolayer(forecast(fm1_dSST_arma), series="Forecast") + 
  autolayer(ts_GMT_dSST_original[,"J-D"], series="Data") + 
  autolayer(fitted(m1_dSST_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 2") +
  guides(colour=guide_legend(title=" "))
Plot_m1_dSST


#Linear Trend Shift Model 2


#GMT Data Set

#Optimal Trend Shift Date 

#AIC Function depending on the Trend Shift Date
AIC=function(z){auto.arima(ts_GMT, xreg = cbind(time,c(rep(0,z), time((z+1):120))))$aic}
#The function uses the the auto arima command (Hyndman) to determine the optimal order.

#Vektor Transformation of the AIC Function
l <- c() 
for (j in 2:116) { l[j-1]  <- AIC(j) }
#Depicting and Plotting the AIC Function
l
AIC_Plot <- plot(l)
#Note that the AIC Function has 2 lokal minima. I therefore chose to restrict the j vektor to (60:116).
#Othrewise the Trend Shift Date would be earlier around 30. Which is better for the fit but worse for the forecasting power.

#Minimizing the AIC Function to optain the Optimal Trend Shift Date 
z_optimal <- which.min(l)  
#Depicting The Optimal Trend Shift Date 
z_optimal

#Defining the Second Time Index according to the Optimal Trend Shift Date 
time1=c(rep(0,z_optimal-1), time(z_optimal:120))

#Defining the Trend Shift Model 2
model=cbind(time,time1)
m2 <- tslm(ts_GMT~model)
summary(m2)

#Plotting Residuals
autoplot(m2$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m2$residuals,lag=20)
ggPacf(m2$residuals, lag=20)
auto.arima(m2$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 2 including the ARMA Component
m2_arma=Arima(ts_GMT, order = c(1,0,0), xreg = cbind(time,time1))
#Diagnostics of Model 2 with ARMA
tsdiag(m2_arma, gof=20)

#Forecast of Model 2 with ARMA
fm2_arma=forecast(m2_arma, xreg = cbind(time=length(time) + 1:19, time1=length(time)-z_optimal+1 + 1:19)) 

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 2 with ARMA
Plot_m2 <- autoplot(ts_GMT_original[,"J-D"], series="Data") +
  autolayer(forecast(fm2_arma), series="Forecast") + 
  autolayer(ts_GMT_original[,"J-D"], series="Data") + 
  autolayer(fitted(m2_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 2") +
  guides(colour=guide_legend(title=" "))
Plot_m2

#GMT_dSST Data Set

#Optimal Trend Shift Date 

#AIC Function depending on the Trend Shift Date
AIC_dSST=function(z){auto.arima(ts_GMT_dSST, xreg = cbind(time,c(rep(0,z), time((z+1):120))))$aic}

#Vektor Transformation of the AIC Function
l_dSST <- c() 
for (j in 2:116) { l_dSST[j-1]  <- AIC_dSST(j) }
#Depicting and Plotting the AIC Function
l_dSST
AIC_Plot_dSST <- plot(l_dSST)
#Note that the AIC Function has 2 lokal minima. I therefore chose to restrict the j vektor to (60:116).
#Othrewise the Trend Shift Date would be earlier around 30. Which is better for the fit but worse for the forecasting power.

#Minimizing the AIC Function to optain the Optimal Trend Shift Date 
z_optimal_dSST <- which.min(l_dSST)  
#Depicting The Optimal Trend Shift Date 
z_optimal_dSST

#Defining the Second Time Index according to the Optimal Trend Shift Date 
time1_dSST=c(rep(0,z_optimal_dSST-1), time(z_optimal_dSST:120))

#Defining the Trend Shift Model 2
model_dSST=cbind(time,time1_dSST)
m2_dSST <- tslm(ts_GMT_dSST~model_dSST)
summary(m2_dSST)

#Plotting Residuals
autoplot(m2_dSST$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m2_dSST$residuals,lag=20)
ggPacf(m2_dSST$residuals, lag=20)
auto.arima(m2_dSST$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 2 including the ARMA Component
m2_dSST_arma=Arima(ts_GMT_dSST, order = c(1,0,0), xreg = cbind(time,time1_dSST))
#Diagnostics of Model 2 with ARMA
tsdiag(m2_dSST_arma, gof=20)

#Forecast of Model 2 with ARMA
fm2_dSST_arma=forecast(m2_dSST_arma, xreg = cbind(time=length(time) + 1:19, time1_dSST=length(time)-z_optimal_dSST+1 + 1:19))

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 2 with ARMA
Plot_m2_dSST <- autoplot(ts_GMT_dSST_original[,"J-D"], series="Data") +
  autolayer(forecast(fm2_dSST_arma), series="Forecast") + 
  autolayer(ts_GMT_dSST_original[,"J-D"], series="Data") + 
  autolayer(fitted(m2_dSST_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 2") +
  guides(colour=guide_legend(title=" "))
Plot_m2_dSST


#Quadratic-Linear Trend Model 3


#GMTData Set

#Defining the Quadratic-Linear Trend Model 3
regressors <- cbind(trend=time,quad=time^2)
m3<- tslm(ts_GMT ~regressors)
summary(m3)

#Plotting Residuals
autoplot(m3$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m3$residuals,lag=20)
ggPacf(m3$residuals, lag=20)
auto.arima(m3$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 3 including the ARMA Component
m3_arma=Arima(ts_GMT, order = c(1,0,0), xreg = cbind(trend=time,quad=time^2))
#Diagnostics of Model 3 with ARMA
tsdiag(m3_arma, gof=20)

#Forecast of Model 3 with ARMA
fm3_arma=forecast(m3_arma, xreg = cbind(trend=length(time) + 1:19, quad=(length(time) + 1:19)^2))

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 4 with ARMA
Plot_m3 <- autoplot(ts_GMT_original[,"J-D"], series="Data") +
  autolayer(forecast(fm3_arma), series="Forecast") + 
  autolayer(ts_GMT_original[,"J-D"], series="Data") + 
  autolayer(fitted(m3_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 3") +
  guides(colour=guide_legend(title=" "))
Plot_m3

#GMT_dSST Data Set

#Defining the Linear-Quadratic Trend Model 3
regressors_dSST <- cbind(trend=time,quad=time^2)
m3_dSST<- tslm(ts_GMT_dSST ~regressors_dSST)
summary(m3_dSST)

#Plotting Residuals
autoplot(m3_dSST$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m3_dSST$residuals,lag=20)
ggPacf(m3_dSST$residuals, lag=20)
auto.arima(m3_dSST$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 3 including the ARMA Component
m3_dSST_arma=Arima(ts_GMT_dSST, order = c(1,0,0), xreg = cbind(trend=time,quad=time^2))
#Diagnostics of Model 3 with ARMA
tsdiag(m3_dSST_arma, gof=20)

#Forecast of Model 3 with ARMA
fm3_dSST_arma=forecast(m3_dSST_arma, xreg = cbind(trend=length(time) + 1:19, quad=(length(time) + 1:19)^2))

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 3 with ARMA
Plot_m3_dSST <- autoplot(ts_GMT_dSST_original[,"J-D"], series="Data") +
  autolayer(forecast(fm3_dSST_arma), series="Forecast") + 
  autolayer(ts_GMT_dSST_original[,"J-D"], series="Data") + 
  autolayer(fitted(m3_dSST_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 3") +
  guides(colour=guide_legend(title=" "))
Plot_m3_dSST



#Exponetial Trend Model 4


#GMT Data Set

#Defining the Exponential Trend Model 4
m4 <- tslm(ts_GMT ~ trend, lambda = 0) 
#"lambda=0" indicates a log transformation of the data"
summary(m4)

#Plotting Residuals
autoplot(m4$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m4$residuals,lag=20)
ggPacf(m4$residuals, lag=20)
auto.arima(m4$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Defining the Model 4 including the ARMA Component
m4_arma=Arima(ts_GMT, order = c(1,0,0), xreg = time, lambda = 0)
#Diagnostics of Model 4 with ARMA
tsdiag(m4_arma, gof=20)

#Forecast of Model 4 with ARMA
fm4_arma <- forecast(m4_arma, xreg = length(time) + 1:19)

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 4 with ARMA
Plot_m4 <- autoplot(ts_GMT_original[,"J-D"], series="Data") +
  autolayer(forecast(fm4_arma), series="Forecast") + 
  autolayer(ts_GMT_original[,"J-D"], series="Data") + 
  autolayer(fitted(m4_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 4") +
  guides(colour=guide_legend(title=" "))
Plot_m4

#GMT_dSST Data Set

#Defining the Exponential Trend Model 4
m4_dSST <- tslm(ts_GMT_dSST ~ trend, lambda = 0) 
#"lambda=0" indicates a log transformation of the data"
summary(m4_dSST)

#Plotting Residuals
autoplot(m4$residuals)

#Plotting ACF and PACF of Residuals and computing the Auto-ARIMA of Residuals
ggAcf(m4_dSST$residuals,lag=20)
ggPacf(m4_dSST$residuals, lag=20)
auto.arima(m4_dSST$residuals)
#The Patterns indicate an ARIMA(1,0,0)

#Forecast of Model 4 with ARMA
m4_dSST_arma=Arima(ts_GMT_dSST, order = c(1,0,0), xreg = time, lambda = 0)
#Diagnostics of Model 4 with ARMA
tsdiag(m4_dSST_arma, gof=20)

#Forecast of Model 4 with ARMA
fm4_dSST_arma <- forecast(m4_dSST_arma, xreg = length(time) + 1:18)

#Plotting the complete Data Set and the Fitted and Forecasted values of Model 4 with ARMA
Plot_m4_dSST <- autoplot(ts_GMT_dSST_original[,"J-D"], series="Data") +
  autolayer(forecast(fm4_dSST_arma), series="Forecast") + 
  autolayer(ts_GMT_dSST_original[,"J-D"], series="Data") + 
  autolayer(fitted(m4_dSST_arma), series="Fitted") + 
  xlab("Year") + ylab("GMT Index") +
  ggtitle("Global-Mean-Temperature Index Model 4") +
  guides(colour=guide_legend(title=" "))
Plot_m4_dSST


#1-step ahead forecasts with the backstep method for all 4 Models in the GMT Data Set


#This is done to compute the RMSFE and MAFE from the out-of-sample forecast
#The following backtest function is from Tsay, R. (2013) An Introduction to Analysis of Financial Data with R. Wiley
backtest <- function(m_used,rt,orig,h,xre=NULL,fixed=NULL,inc.mean=TRUE){
 
  regor=c(m_used$arma[1],m_used$arma[6],m_used$arma[2])
  seaor=list(order=c(m_used$arma[3],m_used$arma[7],m_used$arma[4]),period=m_used$arma[5])
  T=length(rt)
  if(!is.null(xre) && !is.matrix(xre))xre=as.matrix(xre)
  ncx=ncol(xre)
  if(orig > T)orig=T
  if(h < 1) h=1
  rmse=rep(0,h)
  mabso=rep(0,h)
  nori=T-orig
  err=matrix(0,nori,h)
  jlast=T-1
  for (n in orig:jlast){
    jcnt=n-orig+1
    x=rt[1:n]
    if (!is.null(xre)){
      pretor=xre[1:n,]
      mm=arima(x,order=regor,seasonal=seaor,xreg=pretor,fixed=fixed,include.mean=inc.mean)
      nx=xre[(n+1):(n+h),]
      if(h==1)nx=matrix(nx,1,ncx)
      fore=predict(mm,h,newxreg=nx)
    }
    else {
      mm=arima(x,order=regor,seasonal=seaor,xreg=NULL,fixed=fixed,include.mean=inc.mean)
      fore=predict(mm,h,newxreg=NULL)
    }
    kk=min(T,(n+h))
    
    nof=kk-n
    pred=fore$pred[1:nof]
    obsd=rt[(n+1):kk]
    err[jcnt,1:nof]=obsd-pred
  }
  
  for (i in 1:h){
    iend=nori-i+1
    tmp=err[1:iend,i]
    mabso[i]=sum(abs(tmp))/iend
    rmse[i]=sqrt(sum(tmp^2)/iend)
  }
  print("RMSE of out-of-sample forecasts")
  print(rmse)
  print("Mean absolute error of out-of-sample forecasts")
  print(mabso)
  backtest <- list(origin=orig,error=err,rmse=rmse,mabso=mabso)
}

#Time Variables for the Forecasts
time3=c(1:139)
time4=c(rep(0,z_optimal-1), time(z_optimal:139))

#Backtest function to the 4 models
m1_backtest <- backtest(m1_arma,ts_GMT_original,120,1,xre = as.matrix(time3))
m2_backtest <- backtest(m2_arma,ts_GMT_original,120,1,xre = cbind(time3,time4))
m3_backtest <- backtest(m3_arma,ts_GMT_original,120,1,xre = cbind(time3,time3^2))
m4_backtest <- backtest(m4_arma,ts_GMT_original,120,1,xre = as.matrix(time3))


#1-step ahead forecasts with the backstep method for all 4 Models in the GMT_dSST Data Set


#Backtest function to the 4 models
m1_backtest_dSST <- backtest(m1_dSST_arma,ts_GMT_dSST_original,120,1,xre = as.matrix(time3))
m2_backtest_dSST <- backtest(m2_dSST_arma,ts_GMT_dSST_original,120,1,xre = cbind(time3,time4))
m3_backtest_dSST <- backtest(m3_dSST_arma,ts_GMT_dSST_original,120,1,xre = cbind(time3,time3^2))
m4_backtest_dSST <- backtest(m4_dSST_arma,ts_GMT_dSST_original,120,1,xre = as.matrix(time3))



#Data Preparation for the Seminar Paper


#Content Tables


#Table 1 containing the Estimation Results
ta1 <- matrix(c(m1_arma$coef[1],m1_arma$coef[3],0,0,0,m1_arma$coef[2],
                m2_arma$coef[2],m2_arma$coef[3],m2_arma$coef[4],0,0,m2_arma$coef[1],
                m3_arma$coef[2],m3_arma$coef[3],0,m3_arma$coef[4],0,m3_arma$coef[1],
                m4_arma$coef[2],0,0,0,m4_arma$coef[3],m4_arma$coef[1])
              ,ncol=6,byrow=TRUE)
colnames(ta1) <- c("Intercept","Linear Trend","Trend Shift","Quadratic Trend","Exponential Trend","AR1")
rownames(ta1) <- c("m1","m2","m3","m4")
ta1 <- as.table(ta1)

jpeg("table1.jpeg", height=100, width=1150, quality=100)
p1<-tableGrob(ta1)
grid.arrange(p1)
dev.off()

#Table 2 containing the AIC, BIC, RMSE and MAFE Results
ta2_dSST <- matrix(c(m1_arma$aic,m1_arma$bic,m1_backtest_dSST$rmse,m1_backtest_dSST$mabso,
                m2_arma$aic,m2_arma$bic,m2_backtest_dSST$rmse,m2_backtest_dSST$mabso,
                m3_arma$aic,m3_arma$bic,m3_backtest_dSST$rmse,m3_backtest_dSST$mabso,
                m4_arma$aic,m4_arma$bic,m4_backtest_dSST$rmse,m4_backtest_dSST$mabso)
              ,ncol=4,byrow=TRUE)
colnames(ta2_dSST) <- c("AIC","BIC","RMSE","MAFE")
rownames(ta2_dSST) <- c("m1","m2","m3","m4")
ta2_dSST <- as.table(ta2_dSST)

jpeg("table2.jpeg", height=100, width=1150, quality=100)
p2_dSST<-tableGrob(ta2_dSST)
grid.arrange(p2_dSST)
dev.off()


#Content Tables for the Appendix


#Table 1 containing the Estimation Results
ta1_dSST <- matrix(c(m1_dSST_arma$coef[1],m1_dSST_arma$coef[3],0,0,0,m1_dSST_arma$coef[2],
                     m2_dSST_arma$coef[2],m2_dSST_arma$coef[3],m2_dSST_arma$coef[4],0,0,m2_dSST_arma$coef[1],
                     m3_dSST_arma$coef[2],m3_dSST_arma$coef[3],0,m3_dSST_arma$coef[4],0,m3_dSST_arma$coef[1],
                     m4_dSST_arma$coef[2],0,0,0, m4_dSST_arma[3],m4_dSST_arma$coef[1])
              ,ncol=6,byrow=TRUE)
colnames(ta1_dSST) <- c("Intercept","Linear Trend","Trend Shift","Quadratic Trend","Exponential Trend","AR1")
rownames(ta1_dSST) <- c("m1","m2","m3","m4")
ta1_dSST <- as.table(ta1_dSST)

jpeg("table1_dSST.jpeg", height=100, width=1150, quality=100)
p1<-tableGrob(ta1_dSST)
grid.arrange(p1)
dev.off()

#Table 2 containing the AIC, BIC, RMSE and MAFE Results
ta2_dSST <- matrix(c(m1_dSST_arma$aic,m1_arma$bic,m1_backtest_dSST$rmse,m1_backtest$mabso,
                m2_dSST_arma$aic,m2_dSST_arma$bic,m2_backtest_dSST$rmse,m2_backtest$mabso,
                m3_dSST_arma$aic,m3_dSST_arma$bic,m3_backtest_dSST$rmse,m3_backtest$mabso,
                m4_dSST_arma$aic,m4_dSST_arma$bic,m4_backtest_dSST$rmse,m4_backtest$mabso)
              ,ncol=4,byrow=TRUE)
colnames(ta2_dSST) <- c("AIC","BIC","RMSE","MAFE")
rownames(ta2_dSST) <- c("m1","m2","m3","m4")
ta2_dSST <- as.table(ta2_dSST)

jpeg("table2_dSST.jpeg", height=100, width=1150, quality=100)
p2<-tableGrob(ta2_dSST)
grid.arrange(p2)
dev.off()


#Graphs


#Time Series Plot of both Data Sets
Plot_GMT_GMT_dSST <- autoplot(ts_GMT[,"J-D"], series = "LOTI") +
  autolayer(ts_GMT_dSST[,"J-D"], series = "dTs") +
  ggtitle("Global Mean Temperature Indexes") +
  xlab("Year") +
  ylab("Global Mean Temperature Index in Degrees Celsius")
Plot_GMT_GMT_dSST

#Export Time Series Plot of both Data Sets
png(filename="Plot1.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_GMT_GMT_dSST
dev.off()

#Export Time Series Plot of Model 1
png(filename="Plot2.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m1
dev.off()

#Export Time Series Plot of Model 2
png(filename="Plot3.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m2
dev.off()

#Export Time Series Plot of Model 3
png(filename="Plot4.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m3
dev.off()

#Export Time Series Plot of Model 4
png(filename="Plot5.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m4
dev.off()

#Graphs for the Appendix

#Export Time Series Plot of Model 1 
png(filename="Plot2_dSST.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m1_dSST
dev.off()

#Export Time Series Plot of Model 2
png(filename="Plot3_dSST.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m2_dSST
dev.off()

#Export Time Series Plot of Model 3
png(filename="Plot4_dSST.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m3_dSST
dev.off()

#Export Time Series Plot of Model 4
png(filename="Plot5_dSST.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
Plot_m4_dSST
dev.off()

#Export AIC Plot of GMT Data
png(filename="AIC_Plot.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
plot(l)
dev.off()

#Export AIC Plot of GMT Data
png(filename="AIC_Plot_dSST.png", 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=100)
plot(l_dSST)
dev.off()


#Computations


#Point Forecasts for the Seminar Paper


#Pointforecasts 2040 in the GMT Data Set
forecast(m1_arma, xreg = cbind(time=length(time)+40))
forecast(m2_arma, xreg = cbind(time=length(time) + 40, time1=length(time)-z_optimal+1 + 40))
forecast(m3_arma, xreg = cbind(trend=length(time) + 40, quad=(length(time) + 40)^2))
forecast(m4_arma, xreg = length(time) + 40)

#Relative RMSE drop between models 2 and 3 in the GMT Data Set
RMSE_2_3_rel = (m3_backtest$rmse-m2_backtest$rmse)/m2_backtest$rmse
RMSE_2_3_rel

#Relative MAFE drop between models 2 and 3 in the GMT Data Set
MAFE_2_3_rel = (m3_backtest$mabso-m2_backtest$mabso)/m2_backtest$mabso
MAFE_2_3_rel


###Thank You for trying these Estimations##

ta2 <- matrix(c(m1_arma$aic,m1_arma$bic,
                     m2_arma$aic,m2_arma$bic,
                     m3_arma$aic,m3_arma$bic,
                     m4_arma$aic,m4_arma$bic)
                   ,ncol=2,byrow=TRUE)
colnames(ta2) <- c("AIC","BIC")
rownames(ta2) <- c("m1","m2","m3","m4")
ta2_dSST <- as.table(ta2)

jpeg("table2.0.jpeg", height=100, width=1150, quality=100)
p2<-tableGrob(ta2)
grid.arrange(p2)
dev.off()


ta2 <- matrix(c(m1_backtest_dSST$rmse,m1_backtest_dSST$mabso,
                m2_backtest_dSST$rmse,m2_backtest_dSST$mabso,
                m3_backtest_dSST$rmse,m3_backtest_dSST$mabso,
                m4_backtest_dSST$rmse,m4_backtest_dSST$mabso)
              ,ncol=2,byrow=TRUE)
colnames(ta2) <- c("RMSE","MAFE")
rownames(ta2) <- c("m1","m2","m3","m4")
ta2_dSST <- as.table(ta2)

jpeg("table2.1.jpeg", height=100, width=1150, quality=100)
p2<-tableGrob(ta2)
grid.arrange(p2)
dev.off()


ta2_dSST <- matrix(c(m1_backtest_dSST$rmse,m1_backtest$mabso,
                     m2_backtest_dSST$rmse,m2_backtest$mabso,
                     m3_backtest_dSST$rmse,m3_backtest$mabso,
                     m4_backtest_dSST$rmse,m4_backtest$mabso)
                   ,ncol=2,byrow=TRUE)
colnames(ta2_dSST) <- c("RMSE","MAFE")
rownames(ta2_dSST) <- c("m1","m2","m3","m4")
ta2_dSST <- as.table(ta2_dSST)

jpeg("table2.1_dSST.jpeg", height=100, width=1150, quality=100)
p2_dSST<-tableGrob(ta2_dSST)
grid.arrange(p2_dSST)
dev.off()


ta2_dSST <- matrix(c(m1_dSST_arma$aic,m1_dSST_arma$bic,
                     m2_dSST_arma$aic,m2_dSST_arma$bic,
                     m3_dSST_arma$aic,m3_dSST_arma$bic,
                     m4_dSST_arma$aic,m4_dSST_arma$bic)
                   ,ncol=2,byrow=TRUE)
colnames(ta2_dSST) <- c("AIC","BIC")
rownames(ta2_dSST) <- c("m1","m2","m3","m4")
ta2_dSST <- as.table(ta2_dSST)

jpeg("table2.0_dSST.jpeg", height=100, width=1150, quality=100)
p2_dSST<-tableGrob(ta2_dSST)
grid.arrange(p2_dSST)
dev.off()


z_optimal
z_optimal_dSST