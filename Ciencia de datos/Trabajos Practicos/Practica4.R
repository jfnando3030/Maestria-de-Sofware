#Leer datos de archivo csv
#librería para leer datos csv
library(readr)

#cargar datos
datos <- read_csv("C:/Users/ASUS/Downloads/PracticaDataMiningR/data_plantas.csv")
#mostrar datos
View(datos)

#Aplicamos la data de plantas, volver a cargar si es necesario
# Elegir Variables CurvedHeight   y  MaximumHeight

Variables      <-c(7,8)              
Entrenamiento  <-datos[ , c(7,8)]
Entrenamiento

# MaximumHeight es la variable independiente y CurvedHeight la variable dependiente
#modelo <- lm(data = Entrenamiento, formula = CurvedHeight ~ MaximumHeight)
modelo <- lm(CurvedHeight ~ MaximumHeight, data = Entrenamiento)
confint(modelo, level = 0.95)
#Ejemplo de predicción, asignar un valor a la variable independiente, MaximumHeight = 100
predict(object = modelo, newdata = data.frame (MaximumHeight = c(100)),
        interval = "confidence", level = 0.95)
#Ejemplo de predicción para varios datos
Test <- data.frame(MaximumHeight = seq(100, 150, by=5))
p<-predict(modelo, Test, interval = "prediction", level = 0.95)
p<-data.frame (Test,p)
p
#Ejemplo de gráfico 1
plot(x = Entrenamiento$CurvedHeight, y = Entrenamiento$MaximumHeight, main = " CurvedHeight vs MaximumHeight", xlab = "CurvedHeight", ylab = "MaximumHeight", pch = 20, col = "grey30")
abline(modelo, lwd = 3, col = "red")


#Series Temporales
f <- decompose(AirPassengers)
# seasonal figures
f$figure

plot(f$figure, type="b", xaxt="n", xlab="")
# get names of 12 months in English words
monthNames <- months(ISOdate(2011,1:12,1))
# label x-axis with month names
# las is set to 2 for vertical label orientation

axis(1, at=1:12, labels=monthNames, las=2)
plot(f)

#Predicción
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"),  col = c(1,2,4), lty = c(1,1,2))

