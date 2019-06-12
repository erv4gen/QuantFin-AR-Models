library(data.table)
library(xts)


#Load data
	#USD/CNY data
		data=read.csv("~/drive1/csv/playground/garch/4 CNY-USD Currency Exchange Data.csv",header=TRUE)
		data$Date=as.POSIXct(data$Date,format='%m/%d/%Y')
		data=xts(data[,2],data[,1])
		colnames(data)="rate"


#Exploratory analysis
	#Plot original exchange rates
		plot(data$rate,type='l',main='USD/CNY Exchange Rate',ylab="Exchange rate")

	#Differencing the series
		diff.rate=diff(data$rate); diff.rate <- diff.rate[!is.na(diff.rate)]

	#Plot differenced series
		plot(diff.rate,type='l',main='USD/CNY Exchange Rate Daily Changes',ylab="Difference")
		#Change scale
		plot(diff.rate,type='l',main='USD/CNY Exchange Rate Daily Changes',ylab="Difference",ylim=c(-0.3,0.3))		
		hist(diff.rate)

	#ACF & PACF plots on original series
		par(mfcol=c(2,1))
		acf(data$rate,main='ACF USD/CNY Exchange Rate')
		pacf(data$rate,main='PACF USD/CNY Exchange Rate')

	#ACF & PACF plots on differenced series
		par(mfcol=c(2,1))
		acf(diff.rate,main='ACF USD/CNY Exchange Rate Daily Changes')
		pacf(diff.rate,main='PACF Difference USD/CNY Exchange Rate Daily Changes')


#Fit ARIMA on differenced series
	final.aic=Inf
	final.order=c(0,0,0)
	for (p in 1:10) for (d in 0:1) for (q in 1:10) 
	{
		current.aic=AIC(arima(diff.rate,order=c(p, d, q)))
		if(current.aic<final.aic) 
		{
			final.aic=current.aic
			final.order=c(p,d,q)
			final.arima=arima(diff.rate, order=final.order)
		}
	}

	# What is the selected order?
	final.order
	#9 0 1


#Residual Analysis
	resids = resid(final.arima)[-1]
	squared.resids=resids^2

	par(mfcol=c(2,1))
	plot(resids,main='Residuals of USD/CNY ARIMA Fit')
	plot(squared.resids,main='Squared Residuals of USD/CNY ARIMA Fit')

	par(mfcol=c(2,1))
	acf(resids,main='ACF Residuals of USD/CNY ARIMA Fit')
	acf(squared.resids,main='ACF Squared Residuals of USD/CNY ARIMA Fit')


	#test for serial correlation
		Box.test(resids,lag=11,type='Ljung',fitdf=10)
	#test for arch effect
		Box.test((resids)^2,lag=11,type='Ljung',fitdf=10)


#ARCH Fit
	library(tseries)
	#garch.fit = garch(resids, order = c(0,1),trace=F)
	# What order?
	#	pacf(resids^2, ,main="Squared Residuals")
	#summary(garch.fit)

	#Evaluate goodness of fit
	#	resids.fgarch = residuals(garch.fit)[-c(1:7)]
	#	resids.fgarch=resids.fgarch[!is.na(resids.fgarch)]
	#	par(mfcol=c(2,1))
	#	acf(resids.fgarch,main="ACF of ARCH Residuals (USD/CNY)")
	#	acf(resids.fgarch^2,main="ACF of Squared ARCH Residuals (USD/CNY)")
	#	Box.test(resids.fgarch,lag=10,type='Ljung')
	#	Box.test(resids.fgarch^2,lag=10,type='Ljung')


#GARCH Model
	#Divide data into training and testing
	#Predict July and August
		data.test=diff.rate[7855:nrow(diff.rate),]
		data.train=diff.rate[-c(7855:nrow(diff.rate)),]

	#GARCH Order Selection
		library(rugarch)
		#Select model with smallest BIC (if prediction is the objective)
			final.bic = Inf
			final.order = c(0,0)
			for (p in 0:3) for (q in 0:3)
			{
				spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
				mean.model=list(armaOrder=c(9, 1), include.mean=T),
				distribution.model="std")    
				fit = ugarchfit(spec, data.train, solver = 'hybrid')
				current.bic = infocriteria(fit)[2] 
				if (current.bic < final.bic) 
				{
					final.bic = current.bic
					final.order = c(p, q)
				}
			}
		final.order
		#[1] 3 3

	#Refine the ARMA order
		final.bic = Inf
		final.order.arma = c(0,0)
		for (p in 0:6) for (q in 0:6)
		{
			spec = ugarchspec(variance.model=list(garchOrder=c(3,3)),
			mean.model=list(armaOrder=c(p, q), include.mean=T),
			distribution.model="std")    
			fit = ugarchfit(spec, data.train, solver = 'hybrid')
			current.bic = infocriteria(fit)[2] 
			if (current.bic < final.bic) 
			{
				final.bic = current.bic
				final.order.arma = c(p, q)
			}
		} 
		final.order.arma
		#[1] 1 0

	#Refine the GARCH order
		final.bic = Inf
		final.order.garch = c(0,0)
		for (p in 0:3) for (q in 0:3)
		{
			spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
			mean.model=list(armaOrder=c(final.order.arma[1], final.order.arma[2]), 
               	include.mean=T), distribution.model="std")    
               	fit = ugarchfit(spec, data.train, solver = 'hybrid')
               	current.bic = infocriteria(fit)[2] 
               	if (current.bic < final.bic) 
			{
				final.bic = current.bic
				final.order.garch = c(p, q)
               	}
               } 
		final.order.garch
		#[1] 3 3


	#Goodness of Fit 
		spec.1 = ugarchspec(variance.model=list(garchOrder=c(3,3)),
			mean.model=list(armaOrder=c(9,1), 
			include.mean=T), distribution.model="std")    
		final.model.1 = ugarchfit(spec.1, data.train, solver = 'hybrid')

		spec.2 = ugarchspec(variance.model=list(garchOrder=c(3,3)),
			mean.model=list(armaOrder=c(1,0), 
			include.mean=T), distribution.model="std")    
		final.model.2 = ugarchfit(spec.2, data.train, solver = 'hybrid')

		#Compare Information Criteria
		infocriteria(final.model.1)
		infocriteria(final.model.2)

	#Residual Analysis 
		resids.final.model = residuals(final.model.2)
		par(mfcol=c(2,1))
		acf(resids.final.model,main="ACF of GARCH Residuals (USD/CNY)")
		acf(resids.final.model^2,main="ACF of Squared GARCH Residuals (USD/CNY)")                    
		Box.test(resids.final.model,lag=10,type='Ljung')
		Box.test(resids.final.model^2,lag=10,type='Ljung')
		qqnorm(resids.final.model)

	#Prediction of the return time series and the volatility sigma
		nfore = length(data.test)
		fore.series.1 = NULL
		fore.sigma.1 = NULL
		fore.series.2 = NULL
		fore.sigma.2 = NULL
		for(f in 1: nfore)
		{
			#Fit models
			data = data.train
			if(f>2)
			data = c(data.train,data.test[1:(f-1)])  
			final.model.1 = ugarchfit(spec.1, data, solver = 'hybrid')    
			final.model.2 = ugarchfit(spec.2, data, solver = 'hybrid')
			#Forecast
			fore = ugarchforecast(final.model.1, n.ahead=1)
			fore.series.1 = c(fore.series.1, fore@forecast$seriesFor)
			fore.sigma.1 = c(fore.sigma.1, fore@forecast$sigmaFor)
			fore = ugarchforecast(final.model.2, n.ahead=1)
			fore.series.2 = c(fore.series.2, fore@forecast$seriesFor)
			fore.sigma.2 = c(fore.sigma.2, fore@forecast$sigmaFor)
		}
 
	#Compute Accuracy Measures 
		#Mean Squared Prediction Error (MSPE)
			mean((fore.series.1 - data.test)^2)
			mean((fore.series.2 - data.test)^2)
		#Mean Absolute Prediction Error (MAE)
			mean(abs(fore.series.1 - data.test))
			mean(abs(fore.series.2 - data.test))
		#Mean Absolute Percentage Error (MAPE)
			mean(abs(fore.series.1 - data.test)/(data.test+0.000001))
			mean(abs(fore.series.2 - data.test)/(data.test+0.000001))
		#Precision Measure (PM)
			sum((fore.series.1 - data.test)^2)/sum((data.test-mean(data.test))^2)
			sum((fore.series.2 - data.test)^2)/sum((data.test-mean(data.test))^2)

	#Mean Prediction Comparison Plot
		n=length(data)
		ymin = min(na.omit(c(as.vector(data.test),fore.series.1,fore.series.2)))
		ymax = max(na.omit(c(as.vector(data.test),fore.series.1,fore.series.2)))
		data.plot = data.test
		names(data.plot)="Fore"
		plot(data.test,type="l", ylim=c(ymin,ymax), xlab=" ", 
		ylab="USD/CNY Exchange Rate",main="Mean Prediction Comparison (USD/CNY)")
		data.plot$Fore=fore.series.1
		points(data.plot,lwd= 2, col="blue")
		data.plot$Fore=fore.series.2
		points(data.plot,lwd= 2, col="brown")


	#Compare squared observed time series with variance forecasts
		ymin = min(c(as.vector(data.test^2),fore.sigma.1^2,fore.sigma.2^2))
		ymax = max(c(as.vector(data.test^2),fore.sigma.1^2,fore.sigma.2^2))
		plot(data.test^2,type="l", ylim=c(ymin,ymax), xlab=" ", ylab="USD/CNY Exchange Rate",
			main="Variance Prediction Comparison (USD/CNY)")
		data.plot$Fore=fore.sigma.1^2
		points(data.plot,lwd= 2, col="blue")
		data.plot$Fore=fore.sigma.2^2
		points(data.plot,lwd= 2, col="brown")




















