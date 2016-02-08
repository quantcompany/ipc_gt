

shinyServer(function(input, output) {
# This is the series that will be analyzed
   serie <- reactive({
       principal[[input$serie]][,as.integer(input$region)]
   })
   
   decomp <- reactive({
       stl(serie(), t.window=12, s.window=7)
   })
   
   fst <- reactive(forecast(decomp(), level = 90))
  
   nombre_serie <- reactive({input$serie})
   nombre_reg <- reactive({input$region})

#### Text for the first chart
   output$text1 <- renderText({
       sr <- serie()
       if(sd(sr) < 0.01) return("No hay suficiente variación en la serie")
       n <- round(100*(sr[57]/sr[1] - 1 ), 1)
       n <- ifelse(is.nan(n),0,n)
       n15 <- round(100*(sr[57]/sr[45] - 1 ), 1)
       n15 <- ifelse(is.nan(n15),0,n15)
       w1 <- ifelse(n<0,"decreció","creció")
       w2 <- ifelse(n15<0,"decreció en ","creció en ")
       w3 <- adj_perc(abs(n15))
       w4 <- ifelse(n15<0," a la baja", " al alza")
       reg <- names(regiones)[as.integer(nombre_reg())]
       part0 <- paste0("El índice de ", tolower(nombre_serie()), " para la ", reg)
       part1 <- paste0(w1," en ",sprintf("%1.1f", abs(n)),"% durante el periodo de Abril 2011 a Diciembre 2015.")
       part2 <- paste0("Este índice presentó una ", w3," tendencia ", w4, " durante el 2015, año en que el ", w2, sprintf("%1.1f", abs(n15)),"%.")
       out <- paste(part0, part1, part2)
       return(out)
       })
 
### Text for the seasonal chart
   output$text2 <- renderText({
       sr <- serie()
       if(sd(sr) < 0.01) return("No hay suficiente variación en la serie")
       est <- decomp()$time.series[,1]
       maxi <- max(est); mini <- abs(min(est)); mean_sr <- mean(sr,na.rm = TRUE)
       value_est <- 50*(maxi+mini)/mean_sr
       
       if(value_est >= 4){
           est_med <- sort(tapply(est,mesesf,mean, na.rm = TRUE))
           meses_baj <- names(est_med)[1:2]
           meses_alt <- names(est_med)[11:12]
           out <- paste0("En cuanto al análisis de estacionalidad, el índice analizado muestra una
                    estacionalidad muy alta, es decir, se caracteriza por ser fuertemente
                    afectado por factores estacionales, mostrando por lo general, valores bajos en los meses
                    de ", month_table2[[meses_baj[1]]], " y ", month_table2[[meses_baj[2]]], ", y valores altos en el mes de ",
                    month_table2[[meses_alt[1]]], " y sobre todo en ", month_table2[[meses_alt[2]]],".")
       }else if(value_est>=1.5 && value_est<4){
           est_med <- sort(tapply(est,mesesf,mean, na.rm = TRUE))
           meses_baj <- names(est_med)[1:2]
           meses_alt <- names(est_med)[11:12]
           out <- paste0("En cuanto al análisis de estacionalidad, el índice analizado muestra una
                    estacionalidad relativamente alta, es decir, se caracteriza por ser afectado 
                    moderadamente por factores estacionales, mostrando por lo general, valores bajos en los meses
                        de ", month_table2[[meses_baj[1]]], " y ", month_table2[[meses_baj[2]]], ", y valores altos en los meses de ",
                        month_table2[[meses_alt[1]]], " y ", month_table2[[meses_alt[2]]],".")
       }else if(value_est>=0.8){
           out <- "En cuanto al análisis de estacionalidad, el índice analizado muestra una
           estacionalidad relativamente baja, es decir, aunque existen ciertas variaciones de carácter
           estacional, éstas no afectan al índice de manera importante."
       }else{
           out <- "En cuanto al análisis de estacionalidad, el índice analizado muestra una
           estacionalidad muy baja, es decir, las variaciones de carácter
           estacional son prácticamente irrelevantes."
       }
       return(out)
   })
   
### Text for the irregular chart
   output$text3 <- renderText({
       sr <- serie()
       if(sd(sr) < 0.01) return("No hay suficiente variación en la serie")
       irr <- decomp()$time.series[,3]
       sd_irr <- sd(irr, na.rm = TRUE)
       
       if(sd_irr >= 1.5){
           out <- "El índice analízado muestra movimientos irregulares
                   relativamente grandes, es decir, los precios de los productos/servicios que componen el índice tienden a ser muy volátiles
                   y suceptibles a múltiples factores que afectan la oferta y/o la demanda."
       }else if(sd_irr>=0.4 && sd_irr<1.5){
           out <- "El índice analízado muestra movimientos irregulares de nivel moderado, es decir, 
                    los precios de los productos/servicios que componen el índice muestran una volatilidad moderada."
           
       }else{
           out <- "El índice analízado muestra movimientos irregulares muy pequeños, es decir,
                    los precios de los productos/servicios que componen el índice son relativamente estables respecto de la tendencia general."
       }

       return(out)
       })
   
### Text for the forecast chart
   output$text4 <- renderText({
       sr <- as.vector(serie())
       if(sd(sr) < 0.01) return("No hay suficiente variación en la serie")
       fst <- fst()
       pronostico <- fst$mean[12]
       pronosticoL <- fst$lower[12]
       pronosticoH <- fst$upper[12]
       
       crec_dic <- round(100*(pronostico/sr[57] - 1), 1)
       crec_dicL <- round(100*(pronosticoL/sr[57] - 1), 1)
       crec_dicH <- round(100*(pronosticoH/sr[57] - 1), 1)
       
       amplitud <- 100*(pronosticoH-pronosticoL)/pronostico
       
       if(amplitud > 15){
           out <- paste0("Según el modelo cuantitativo aplicado, se pronostica un cambio en el índice seleccionado de ",
                         sprintf("%1.1f",crec_dic),"% para diciembre de 2016. Aunque debido al comportamiento histórico y volatilidad del índice,
                         el márgen de amplitud para la predicción es bastante grande, siendo el límite inferior ",
                         sprintf("%1.1f",pronosticoL)," y el superior ", sprintf("%1.1f",pronosticoH),",
                         lo que equivale a un cambio porcentual de entre ", sprintf("%1.1f",crec_dicL), "% y ", sprintf("%1.1f",crec_dicH),"% 
                         para diciembre de 2016."
                         )
       }else if(amplitud<=15 && amplitud > 4.5){
           out <- paste0("Según el modelo cuantitativo aplicado, se pronostica un cambio en el índice seleccionado de ",
                         sprintf("%1.1f",crec_dic),"% para diciembre de 2016. Sin embargo, debido al comportamiento histórico del índice, el márgen de amplitud para las
                         predicciones es relativamente amplio, de entre ", sprintf("%1.1f",pronosticoL)," y ", 
                         sprintf("%1.1f",pronosticoH),", lo que equivale a un cambio porcentual de entre ", 
                         sprintf("%1.1f",crec_dicL), "% y ", sprintf("%1.1f",crec_dicH),"% para diciembre de 2016."
           )
       }else{
       out <- paste0("Según el modelo cuantitativo aplicado, se pronostica un cambio en el índice seleccionado de ",
                     sprintf("%1.1f",crec_dic),"% para diciembre de 2016. Debido al comportamiento histórico del índice, es posible tener un
                     intervalo relativamente cerrado para las predicciones de entre ", sprintf("%1.1f",pronosticoL)," y ", 
                     sprintf("%1.1f",pronosticoH),", lo que equivale a un cambio porcentual de entre ", 
                     sprintf("%1.1f",crec_dicL), "% y ", sprintf("%1.1f",crec_dicH),"% para diciembre de 2016.")
                         
           
       }
       return(out)
   })
   
   # Report Title
  output$titulo <- renderText({
       reg <- toupper(names(regiones)[as.integer(nombre_reg())])
       paste0(nombre_serie(),", ", reg)
       })
  
  #### Gráfica de tendencia
  output$ind_tend <- renderChart2({
      dat <- data.frame("Fecha" = fechas, 
                        "Indice" = as.vector(serie()),
                        "Tendencia" = round(as.vector(decomp()$time.series[,2]),2)
      )
      h1 <- Highcharts$new()
      h1$title(text = "Índice y Tendencia")
      h1$chart(type = "line", width = 630, height = 410, zoomType = 'x')
      h1$xAxis(categories = dat[["Fecha"]], labels = list(rotation = -45, step = 4))
      h1$yAxis(title = list(text = "Indice"))
      h1$series(data = dat[["Indice"]], name = "Indice")
      h1$series(data = dat[["Tendencia"]], name = "Tendencia", marker = list(radius=0), color = "#E95D22")
      h1
  })

#### Gráfica estacional
  output$estacion <- renderChart2({
      dat <- data.frame("Fecha" = fechas, 
                        "Estacionalidad" = round(as.vector(decomp()$time.series[,1]),2)
      )
      h1 <- Highcharts$new()
      h1$title(text = "Estacionalidad")
      h1$chart(type = "line", width = 630, height = 410, zoomType = 'x')
      h1$xAxis(categories = dat[["Fecha"]], labels = list(rotation = -45, step = 4))
      #h1$yAxis(title = list(text = "Puntos"))
      h1$series(data = dat[["Estacionalidad"]], name = "Estacionalidad", list(enabled = "false"))
      h1
  })

#### Gráfica irregular
  output$shock <- renderChart2({
      dat <- data.frame("Fecha" = fechas, 
                        "Shock" = round(as.vector(decomp()$time.series[,3]),2)
      )
      h1 <- Highcharts$new()
      h1$title(text = "Movimientos irregulares")
      h1$chart(type = "column", width = 630, height = 410, zoomType = 'x')
      h1$xAxis(categories = dat[["Fecha"]], labels = list(rotation = -45, step = 4))
      h1$yAxis(title = list(text = "Shocks"))
      h1$series(data = dat[["Shock"]], name = "Shocks")
      h1
  })

#### Gráfica proyeccion 
  output$forecast <- renderChart2({
      fechas <- c(fechas[40:57],paste0(meses,"-2016"))
      fst <- fst()
      sr <- as.vector(serie()[40:57])
      pronostico <- c(rep(NA,17),sr[18],round(fst$mean[1:12],2))
      pronosticoL <- c(rep(NA,17),sr[18],round(fst$lower[1:12],2))
      pronosticoH <- c(rep(NA,17),sr[18],round(fst$upper[1:12],2))
      dat <- data.frame("Fecha" = fechas, 
                        "Indice" = c(sr, rep(NA,12)),
                        "Pronostico" = pronostico,
                        "PronosticoL" = pronosticoL,
                        "PronosticoH" = pronosticoH) 

      
      h1 <- Highcharts$new()
      h1$title(text = "Pronóstico para 2016")
      h1$subtitle(text = "Límites al 90% de confianza")
      h1$chart(type = "line", width = 630, height = 410, zoomType = 'x')
      h1$xAxis(categories = dat[["Fecha"]], labels = list(rotation = -45, step = 2))
      h1$yAxis(title = list(text = "Indice"))
      h1$series(data = dat[["Indice"]], name = "Indice")
      h1$series(data = dat[["Pronostico"]], name = "Pronóstico Medio", marker = list(radius=0), color = "#E95D22")
      h1$series(data = dat[["PronosticoL"]], name = "Límite Inferior", marker = list(radius=0), color = "black")
      h1$series(data = dat[["PronosticoH"]], name = "Límite Superior", marker = list(radius=0), color = "black")
      h1
  })  
})
