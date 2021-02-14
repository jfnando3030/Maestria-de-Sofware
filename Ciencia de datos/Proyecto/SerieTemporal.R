
#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("ggfortify")
#install.packages("readr")

library(ggplot2)
library(forecast)
library(tseries)
library(readr)
library(ggfortify)
library(urca)

#carga de datos
datosc <- read_delim("D:/Maestria de Sofware/Maestria-de-Sofware/Ciencia de datos/Proyecto/ventas.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)

# convertimos la data a series temporales
ventas<-ts(datosc$ventas_mensuales, start = c(2019,6), end=c(2022, 12), frequency = 12)
#graficamos nuestra serie temporal

diff.serie1.12<-diff(ventas, lag = 0)
adf<-adf.test(diff.serie1.12, alternative = "stationary")
adf$p.value

#determina el numero de diferencias necesarias para la serie de tiempo, esto para hacerla estacionaria
ndiffs(ventas) # da resultado 0 


serie1<-ts(datosc$ganancias, start = c(2019,6), end=c(2030, 12), frequency = 12)
ts.plot(serie1)
print(serie1)

diferencias <- ndiffs(serie1)
diferencias[1]

seriedif2= diff(serie1, differences = 2)

adf.test(seriedif2, alternative = "stationary")
plot(seriedif2)
#CONVERTIR LA SERIE TEMPORAL EN ESTACIONARIA
# Prueba ADF Augmented Dickey-Fuller (ADF)
diff.serie1.12<-diff(serie1, lag = 12)
adf<-adf.test(diff.serie1.12, alternative = "stationary")
adf$p.value


#Prueba de (KPSS Kwiatkowski-Phillips-Schmidt-Shin)
k_test= ur.kpss(serie1, type="tau", lags="short")
summary(k_test)
dk_test= ur.kpss(diff(serie1), type="tau", lags="short")
summary(dk_test)

#Trazamos la serie de tiempo co2ts
#tendencia ascendente, no es estacionaria en la media.
autoplot(serie1, ts.colour = "blue", ts.linetype = "dashed")
# determina el numero de diferencias necesarias para la serie de tiempo, esto para hacerla estacionaria
ndiffs(serie1)
# Los respectivos cálculos nos muestran que la serie necesitar ser diferenciación regular (tendencia regular con la media) y otra estacional(estructura que se repite en un periodo).
diff.serie1<-autoplot(diff(serie1), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff.serie1
# Se elimina el componente de tendencia, se grafica la función de autocorrelación
autoplot(acf(diff(serie1), plot = FALSE))
# La serie ya no es estacionaria, se traza una subseries estacionales de una serie de tiempo.
monthplot(diff(serie1), col = "midnightblue")
# la diferencia se presenta en caja
diff2<-diff(serie1)
boxplot(diff2~cycle(diff2))

# se elimina el componente estacional
diff.serie1.12<-diff(serie1, lag = 12)
autoplot(diff.serie1.12, ts.colour = "darkorange4", ts.linetype = "dashed")
# Prueba ADF Augmented Dickey-Fuller (ADF) t-test 
adf<-adf.test(diff.serie1.12, alternative = "stationary")
plot(adf)

adf$p.value
# Prueba kpss
kpss<-kpss.test(diff.serie1.12)
kpss$p.value
#El valor 0.01 del test indica que se puede rechazar la hipótesis nula H0
# autocovarianza 
autoplot(acf(diff.serie1.12, plot = FALSE))
# autcorrelacion parcial de la muestra 
autoplot(pacf(diff.serie1.12, plot = FALSE))


arima1<- Arima(serie1, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima1

#diferecias
deSerie1 <- decompose(serie1, type="multiplicative")
plot(deSerie1)




#prediccion de ventas Chevrolet 
#modelo STLM
fitc1 <- stlm(serie1, modelfunction=ar)
fcc1<- forecast(fitc1, h=36)
plot(fcc1)
fcc1
accuracy(fcc1)
#STLF suavisacion exponencial
fitc2 <- stlf(serie1, lambda=0)
fcc2<- forecast(fitc2,36)
plot(fcc2)
accuracy(fcc2)

#prueba Augmented Dickey-Fuller (ADF) t-test 
adf.test(diff(log(serie1)), alternative="stationary", k=0)
#Kwiatkowski-Philips-Schmidt-Shin (KPSS) 
kpss.test(serie1)

#HoltWinters
#fitc3 <- HoltWinters(serie1, beta=TRUE, gamma=TRUE) 
fitc3 <- HoltWinters(serie1) 
fcc3 <- forecast(fitc3, 36)
plot(fcc3)
accuracy(fcc3)
#Variacion Modelo STLM arima 3,1,6
fitc4 <- stlm(serie1, modelfunction=Arima, order=c(3,1,6))
fcc4 <- forecast(fitc4, 36)
plot(fcc4)
accuracy(fcc4)
#EST suavizacion exponencial
fitc5 <- ets(serie1)
fcc5<- forecast(fitc5, h=36)
plot(fcc5)
fcc5
accuracy(fcc5)

#Auto.Arima
fitc6 <- auto.arima(serie1, lambda=0, biasadj=TRUE)
fcc6<- forecast(fitc6, h=36)
plot(fcc6)
fcc6
accuracy(fcc6)

#modelo nnetar(investigar)
fitc7 <- nnetar(serie1)
fcc7<- forecast(fitc7, h=36)
plot(fcc7)
fcc7
accuracy(fcc7)

#modelo tbats(investigar)
fitc8 <- tbats(serie1, biasadj=TRUE)
fcc8<- forecast(fitc8, h=36)
plot(fcc8)
fcc8
accuracy(fcc8)







