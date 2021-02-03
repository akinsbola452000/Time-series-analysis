
########## NAME - AKINAINI BOLARINWA ##########

########## STUDENT NO - 422932 ##########


########## DEGREE PROG - MDP CBDA ##########








#QUESTION1
library(TSA)

vent <- scan("http://www.uta.fi/sis/mtt/mttts18/ventilation.txt")
venti = ts(vent)
plot(venti,ylab="Ventilation (L/min)",xlab="Observation time",type="o") # the data not stationary, clear case of increasing linear trend
acf(venti,main="Sample ACF") # An obvious case of a zero mean white noise note



plot(diff(venti),ylab="Taking first order differencing",xlab="Time",type="o")#using the first order differencing
acf(diff(venti),main="Sample ACF: 1st differences")# autocorrelation after the first order differencing to obtain a stationary residual
#the above shows that taking the first order difference, we were able to get rid of the linear trend.




#QUESTION2
arma.sim<-arima.sim(model=list(ar=c(-.6),ma=c(.8)),n=200) #ARMa(1,1) simulation with 200 observations
arma.sim 
ts.plot(arma.sim) #time series plot

arma.acf<-acf(arma.sim,type="correlation",plot=T) # Autocorrelation plot
arma.acf

arma.pacf<-acf(arma.sim,type="partial",plot=T) # partial-autocorrelation plot
arma.pacf  

order.arma.sim=armasubsets(y=arma.sim,nar=6,nma=6,y.name='(e.qu.)',ar.method='ols')
plot(order.arma.sim)  # BIC shows MA(1) or ARMA(0,1)




#QUESTION3
ibm <- scan("http://www.uta.fi/sis/mtt/mttts18/ibm.txt")
ibm = ts(ibm)
ts.plot(ibm) # time series plot
acf(ibm,main="Sample ACF") #autocorrelation plot before first differecing
adf.test(ibm, alternative = "stationary", k=1) # Augmented Dickey and Fuller test

ibm1 = diff(ibm) # first difference
ts.plot(ibm1)

arma.acf<-acf(ibm1,type="correlation",plot=T) # Autocorrelation plot after first defferencing
arma.acf

arma.pacf<-acf(ibm1,type="partial",plot=T) # partial-autocorrelation plot
arma.pacf
pacf(ibm1,main="Sample PACF") # trying second plot to be double sure

order.ibm1 = armasubsets(y=ibm1,nar=6,nma=6,y.name='ibm1',ar.method='ols') # armasubsets function to detect best model
plot(order.ibm1) # 

arima(ibm1,order=c(6,1,0),method='ML') # maximum likelihood  # AIC test for ARIMA(6,1,0)  # aic = 2537.86

arima(ibm1,order=c(1,1,0),method='ML') # maximum likelihood  # AIC test for ARIMA(1,1,0) # aic = 2635.41

#Residual analysis of ibm data

ibm1diag<- arima(ibm1,order=c(6,1,0),method='ML') # diagnostic of ARima(6,1,0)

par(mfrow=c(2,2))  # four plots in one page

plot(ibm1,ylab="IBM",xlab="ibmdata",type="o")

plot(rstandard(ibm1diag),xlab="Time",ylab="Standardised residuals",type='o')
abline(h=0)

hist(rstandard(ibm1diag),xlab="Standardised residuals",main="")

qqnorm(rstandard(ibm1diag),main="")
qqline(rstandard(ibm1diag))

Box.test(residuals(ibm1diag),lag =10, type="Ljung-Box",fitdf=6)  #Box-Ljung test  with fitdf=p+q
#data:  residuals(ibm1diag)

Box.test(residuals(ibm1diag), 4)  # Box-Pierce test


ibm1diag.resid = resid(ibm1diag)
plot(y=ibm1diag.resid , x=zlag(lh.resid,1),ylab=expression(X[t]),xlab=expression(X[t-1]),type='p',main="Lag 6 scatterplot") # scatter plot of residuals
abline(h=0

ibm1diag.resid.acf<-acf(ibm1diag.resid,type="correlation",plot=T) # Autocorrelation plot of residuals

ibm1diag.resid.acf

# Box-Cox transformation of the ibm data
ibm.bc = BoxCox.ar(ibm)
ibm.bc$mle #[1] 2        #### Then, we choose lambda= 0.5 : Square root transformation



ibm2 = sqrt(ibm)
plot(ibm2,ylab="IBM DATA (Square-root scale)",xlab="Year",type="o")
ibm3 = diff(ibm2)

arma.acf<-acf(ibm3,type="correlation",plot=T) # Autocorrelation plot after first defferencing
arma.acf

arma.pacf<-acf(ibm3,type="partial",plot=T) # partial-autocorrelation plot
arma.pacf
pacf(ibm3,main="Sample PACF") # trying second plot to be double sure

order.ibm3 = armasubsets(y=ibm3,nar=6,nma=6,y.name='ibm3(transf and diff)',ar.method='ols') # armasubsets function to detect best model
plot(order.ibm3) 


# Trying 5 by 5 armasubsets
order.ibm3 = armasubsets(y=ibm3,nar=5,nma=5,y.name='ibm3(transf and diff)',ar.method='ols') # armasubsets function to detect best model
plot(order.ibm3)
arima(ibm3,order=c(4,1,0),method='ML') # AR(4) Aic= -144.39
arima(ibm3,order=c(3,1,4),method='ML') # ARMA(3,4) Aic= --219.2



#Dignostic of the transformed and differeced AR(6)/ARIMA(6,1,0)

#Residual analysis of ibm data

ibm3diag<- arima(ibm3,order=c(6,1,0),method='ML') # diagnostic of ARima(6,1,0)

par(mfrow=c(2,2))  # four plots in one page

plot(ibm3diag,ylab="IBM",xlab="ibmdata",type="o")

plot(rstandard(ibm3diag),xlab="Time",ylab="Standardised residuals",type='o')
abline(h=0)

hist(rstandard(ibm3diag),xlab="Standardised residuals",main="")

qqnorm(rstandard(ibm3diag),main="")
qqline(rstandard(ibm3diag))


#Dignostic of the transformed and differeced AR(4)/ARIMA(4,1,0)

#Residual analysis of ibm data

ibm3diag2<- arima(ibm3,order=c(4,1,0),method='ML') # diagnostic of ARima(4,1,0)

par(mfrow=c(2,2))  # four plots in one page

plot(ibm3diag2,ylab="IBM",xlab="ibmdata",type="o")

plot(rstandard(ibm3diag2),xlab="Time",ylab="Standardised residuals",type='o')
abline(h=0)

hist(rstandard(ibm3diag2),xlab="Standardised residuals",main="")

qqnorm(rstandard(ibm3diag2),main="")
qqline(rstandard(ibm3diag2))

