library(fpp2)
#Problem 9-1
autoplot(advert, facets = T)
tslm.advert <- tslm(sales~advert, data=advert)
autoplot(advert[,'sales'], series="Data") +
  autolayer(fitted(tslm.advert), series="Fitted")
checkresiduals(tslm.advert)
ggPacf(advert[,'sales']-fitted(tslm.advert))
dynamic.reg.sales<-Arima(advert[,"sales"], xreg=advert[,"advert"], order=c(0,0,2))
checkresiduals(dynamic.reg.sales)
checkresiduals(auto.arima(advert[,"sales"], xreg=advert[,"advert"]))
fcast <- forecast(dynamic.reg.sales, xreg=rep(10,6))
autoplot(fcast) + xlab("Time") +
  ylab("Sales")
#Problem 9-2
autoplot(huron)
t <- time(huron)
t.break1 <- 1920
tb1 <- ts(pmax(0, t - t.break1), start = 1875)
fit.pw <- auto.arima(huron, xreg=t + tb1)
autoplot(huron, series = "data") + autolayer(fitted(fit.pw), series = "fitted piecewise model")
checkresiduals(fit.pw)
h=30
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h) 
autoplot(forecast(fit.pw, xreg=t.new+tb1.new))

#Problem 9-3
autoplot(motel)
mean(motel[,"Takings"]/motel[,"Roomnights"]*1000)
autoplot(log(motel[,"CPI"]))+autolayer(log(cpimel))
xreg <- cbind(Takings = motel[, "Takings"],
              CPI = motel[, "CPI"])
fit <- auto.arima(motel[, "Roomnights"], xreg = xreg)
checkresiduals(fit)
fit.Taking <- auto.arima(motel[, "Takings"])
fit.cpi <- auto.arima(motel[, "CPI"])
fcast <- forecast(fit, xreg = cbind(Takings=forecast(fit.Taking, h=12)$mean, CPI=forecast(fit.cpi, h=12)$mean), h=12)
autoplot(fcast)

#Problem 9-5
autoplot(gasoline)
t <- time(gasoline)
t.break1 <- 2007
t.break2 <- 2013
tb  <- ts(pmax(0, t), start = 1991.1)
tb1 <- ts(pmax(0, t - t.break1), start = 1991.1)
tb2 <- ts(pmax(0, t - t.break2), start = 1991.1)

fit.pw <- tslm(gasoline ~ tb + tb1 + tb2+fourier(gasoline, K=12))
AIC(fit.pw)
autoplot(gasoline)+autolayer(fitted(fit.pw))
data.new <- cbind(tb=tb, tb1=tb1, tb2=tb2, fourier(gasoline, K=12))

dynamic.reg <- auto.arima(gasoline, xreg=data.new)


#Problm 9-6
retaildata <- readxl::read_excel("C:/Users/vahid/Documents/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))
(lambda <- BoxCox.lambda(myts))
autoplot(BoxCox(myts,lambda))
fit <- auto.arima(BoxCox(myts,lambda), xreg = fourier(BoxCox(myts,lambda), K=6))
AIC(fit)
checkresiduals(fit)

