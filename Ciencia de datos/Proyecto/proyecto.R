#install.packages("devtools")
#devtools::install_github("rstudio/shinydashboard")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(forecast)
library(tseries)
library(readr)
library(ggfortify)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = tags$a(href='http://mycompanyishere.com',
                                 tags$img(src='https://pagaresfacil.com/images/PagarBlanco.png', width = "100px"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Pre-Procesamiento Datos", tabName = "pre_procesamiento", icon = icon("th")),
      menuItem("Series temporales", tabName = "time_series", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "inicio",
              fluidRow(
                column(4,uiOutput("ganancias")),
                
                column(4,uiOutput("ventas_si")),
                column(4,uiOutput("ventas_no")),
                )
            
      ),
      
      # Second tab content
      tabItem(tabName = "pre_procesamiento",
              h2("Listado de Datos"),
              fluidRow(
                column(12, DT::dataTableOutput("mytable"))
              )),
    tabItem(
         tabName = "time_series",
            h2("Series temporales"),
            fluidRow(
              box(width=12,title = "Datos convertidos en series temporales",column(12, plotOutput("plotSerieTemporal1")))
            ),
            fluidRow(
              box(width=3,title = "Prueba ADF Augmented Dickey-Fuller (ADF)",column(12,textOutput("adfTest"))),
              box(width=9,title = "Diagrama estacionario",column(12,plotOutput("diagramEstacionario")))
            ),
           fluidRow(
             box(width=6,title = "Función de autorrelación (ACF)",column(12,plotOutput("plotACF"))),
             box(width=6,title = "Función de autorrelación Parcial (PACF)",column(12,plotOutput("plotPACF")))
            
             ),
         fluidRow(
           column(4,box(width=12,title = "ARIMA",column(12,textOutput("arima_result"))), column(12,box(width=12,title = "Test de LJung-Box",column(12,textOutput("jung_box"))))),
          
           column(8,box(width=12,title = "Grafico Arima",column(12,plotOutput("plotArima"))))
           
           
         ),
         fluidRow(
           box(width=6,title = "Valores de pronóstico con intervalos de confianza superior e inferior del 80%",column(12,DT::dataTableOutput("pronosticoTable80"))),
           box(width=6,title = "Valores de pronóstico con intervalos de confianza superior e inferior del 95%",column(12,DT::dataTableOutput("pronosticoTable95"))),
           
           
         ),
         fluidRow(
           box(width=12,title = "Gráfico ARIMA",column(12, plotOutput("plotPronostico")))
         ),
         fluidRow(
           box(width=12,title = "Métricas ARIMA",column(12,DT::dataTableOutput("metricas"))),
           
           
           
         )
            
      
         )
  
         
      )
    
    
    )
  )


server <- function(input, output){
  
  #carga de datos
  datosc <- read_delim("D:/Maestria de Sofware/Maestria-de-Sofware/Ciencia de datos/Proyecto/ventas.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
  
   # convertimos la data a series temporales
  ventas<-ts(datosc$ventas_mensuales, start = c(2019,6), end=c(2025, 12), frequency = 12)
  #graficamos nuestra serie temporal
  
  

  
  output$plotSerieTemporal1 <- renderPlot({
    
    g1 <- ts.plot(ventas)
    print(g1)
    
  })
  
  #CONVERTIR LA SERIE TEMPORAL EN ESTACIONARIA
  # Prueba ADF Augmented Dickey-Fuller (ADF) t-test 
  estacionario= diff(ventas, differences = 2)

  
  respuesta <- adf.test(estacionario, alternative = "stationary")
  
  
  
  output$adfTest <- renderPrint({ 
    respuesta
  })
  

  
  
  output$diagramEstacionario <- renderPlot({
    
    g2 <- plot(estacionario, type="o", lty="dashed", col="red", main="Serie de tiempo")
    print(g2)
    
  })
  
  #La función de auto correlación y autocorrelación parcial
  #sirven para conocer cuantos medias moviles y cuantos auto regresivos utilizaremos en nuestra modelo ARIMA
  
  # autocovarianza 
  acf <- autoplot(acf(ts(estacionario,frequency = 1)))
 
  # autcorrelacion parcial de la muestra 
  plotPACF <- autoplot(pacf(ts(estacionario,frequency = 1)))
  
  output$plotACF <- renderPlot({
    
    
    print(acf)
    
  })
  
  output$plotPACF <- renderPlot({
    
    
    print(plotPACF)
    
  })
  
  modelo1 <- Arima(ventas, order=c(1,2,1))
  
  
   #modelo1<- Arima(serie1, order=c(1,2,1), seasonal=list(order=c(1,2,1),period=12))
  
  
  output$arima_result <- renderPrint({ 
    modelo1
  })
  
  output$plotArima <- renderPlot({
    
     tsdiag(modelo1)
    
  })
  
  # También se puede probar que una serie es un ruido blanco por medio de prueba de hipótesis.
  #prueba de autocorrelacion de los residuos
  jung_box <- Box.test(residuals(modelo1), type="Ljung-Box")
  
  output$jung_box <- renderPrint({ 
    jung_box
  })
  

  pronostico <- forecast::forecast(modelo1, h=12)
  pronostico
  
  
  output$plotPronostico <- renderPlot({
    
    pronos <- plot(pronostico)
    print(pronos)
    
  })
  
  
  
  
  output$metricas <- renderDataTable({
    
    myAccuracy <- accuracy(pronostico)
    
    myAccuracy <- as.data.frame(as.table(myAccuracy))
    
  })
  
  
  #pronostico con el 95%
  pronostico95 <- with(pronostico,
                                  data.frame(Point_forecast=pronostico$mean,
                                             Higher=pronostico$upper[,2],
                                             Lower=pronostico$lower[,2]))

  #pronostico con el 80%
  pronostico80 <- with(pronostico,
                                  data.frame(Point_forecast=pronostico$mean,
                                             Higher=pronostico$upper[,1],
                                             Lower=pronostico$lower[,1]))
  
  output$pronosticoTable80 = DT::renderDataTable({
    pronostico80 <- pronostico80 %>% 
      select(Point_forecast, Higher, Lower) %>%
      mutate(Point_forecast = round(Point_forecast, 2),
             Higher = round(Higher, 2),
             Lower = round(Lower, 2))
  }, extensions = 'Responsive')
  
  output$pronosticoTable95 = DT::renderDataTable({
    pronostico95 <- pronostico95 %>% 
      select(Point_forecast, Higher, Lower) %>%
      mutate(Point_forecast = round(Point_forecast, 2),
             Higher = round(Higher, 2),
             Lower = round(Lower, 2))
  }, extensions = 'Responsive')
  
  
  datosc <- read_delim("D:/Maestria de Sofware/Maestria-de-Sofware/Ciencia de datos/Proyecto/ReporteVentas_PagarEsFacil_2020-02-07.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
  

  ganancias_totales <- sum(datosc$ganancias)
  ventas_concretadas <- sum(datosc$venta_concretada == "Si")
  ventas_concretadas_no <- sum(datosc$venta_concretada == "No")
  
  output$mytable = DT::renderDataTable({
    datosc
  }, extensions = 'Responsive')
  
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Inicio", tabName="home", icon =icon("dashboard")),
      menuItem("Series Temporales", tabName="time_series", icon =icon("th"))
    )
  })
  
  output$ganancias <- renderUI({
    infoBox("Ganancias Totales", width = '100%',color = "green", ganancias_totales,icon = icon("dollar-sign"), fill = TRUE)
  })
  
  output$ventas_si <- renderUI({
    infoBox("Ventas Concretadas",width = '100%',color = "purple", ventas_concretadas,icon = icon("chart-line"), fill = TRUE)
  })
  
  output$ventas_no <- renderUI({
    infoBox("Ventas No Concretadas",width = '100%',color = "blue", ventas_concretadas_no,icon = icon("chart-line"), fill = TRUE)
  })
  
}

shinyApp(ui, server)
