library(Quandl)
library(dplyr)
library(quantmod)
library(mgcv)
library(ggplot2)
library(tseries)
#quandlapi <- readr::read_file('C:/pjt/QuantFin-Volatility/nogit/quandlapi.txt')
#Quandl.api_key(quandlapi)
#data <- Quandl("XNAS/AAPL")

####Read init Data ####
getSymbols("AAPL",src='yahoo')
candleChart(AAPL,theme='white')

#1 level difirintiation
aaplrt <- diff(log(Cl(AAPL)))
aaplrt <- aaplrt[2:nrow(aaplrt),]
par(mfrow=c(2,1))

#Plot log return with squeard autocor
plot(log(Cl(AAPL)),main='Abs Return')
plot(aaplrt,main='Log Return')
acf(as.vector(aaplrt),main='ACF')
acf(as.vector(aaplrt^2),main='ACF^2')
pacf(as.vector(aaplrt),main='PACF')


####Fit ARIMA model####
final.aic = Inf
final.order = c(0,0,0)
for (p in 1:6) for (d in 0:1) for (q in 1:6) {
  tryCatch(
  current.aic = AIC(arima(aaplrt, order=c(p, d, q)))
  , error = function(e){ cat("ERROR :",conditionMessage(e), "\n")})
  if (current.aic < final.aic) {
    final.aic = current.aic
    final.order = c(p, d, q)
    
    
  }
}
final.arima = arima(aaplrt, order=final.order)

####Residuals analysis####
resids = resid(final.arima)[-1]
acf(resids,main="Residuals of ARIMA Fit")
acf(resids^2,main="Squared Residuals of ARIMA Fit")
# for serial correlation
Box.test((resids)^2,lag=12,type='Ljung',fitdf=11)
# for arch effect
Box.test(resids,lag=12,type='Ljung',fitdf=11)


#### Estimate the variance using nonparametric regression ####

zt.sq.log <- log(resids^2)
n <- length(resids)
time.aapl <- c(1:(n+100))
time.aapl <- (time.aapl-min(time.aapl))/(max(time.aapl)-min(time.aapl))

time.aapl.train <-time.aapl[1:n]
dat.train <- data.frame(time=time.aapl.train ,zt = zt.sq.log)
gam.var <- gam(zt~s(time),dat=dat.train)
aaplrt.obs <- aaplrt[-1]
aaplrt.var.fit <- sqrt(exp(fitted(gam.var)))

plot(dat.resid$vola,main="")

dat.predict <- data.frame(time=time.aapl[n:(n+100)])
#predict variance using gam
plot(predict(gam.var,dat.predict),type='l')


######## Apply ARCH Model ###################################################
pacf(resids^2,main="Squared Residuals")
arch.fit = garch(resids, order = c(0, 7),trace=F)
summary(arch.fit)
resids.fgarch = residuals(arch.fit)[-c(1:7)]
acf(resids.fgarch,main="ACF of ARCH Residuals")
acf(resids.fgarch^2,main="ACF of Squared ARCH Residuals")                    
Box.test(resids.fgarch,lag=10,type='Ljung')
Box.test(resids.fgarch^2,lag=10,type='Ljung')
hist(resids.fgarch,"Histrogram of the Residuals")
qqnorm(resids.fgarch)


