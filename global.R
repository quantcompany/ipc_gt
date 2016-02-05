library(shiny)
library(dplyr)
library(rCharts)
#library(ggplot2)

datos <- read.csv(file = "data/IPCGuate2015-11.csv", header = TRUE)

### Reshaping the data_table so we can work with the time series data
datos <- tbl_df(datos)

#' Creating the last day of the month
#' @param month_name a String, name of the month in spanish, all lower case
#' @return a string, 28 for Feb., 30 for all the other months 
last_day <- function(month_name){
    
    if(month_name == "febrero"){
        return("28")
    }else{
        return("30")
    }
}

# month_table <- list("enero" = "01", 
#                     "febrero" = "February",
#                     "marzo" = "March",
#                     "abril" = "Abril",
#                     "mayo" = "May",
#                     "junio" = "June",
#                     "julio" = "July",
#                     "agosto" = "August",
#                     "septiembre" = "September",
#                     "octubre" = "Octuber",
#                     "noviembre" = "November",
#                     "diciembre" = "December")

month_table <- list("enero" = "01", 
                    "febrero" = "02",
                    "marzo" = "03",
                    "abril" = "04",
                    "mayo" = "05",
                    "junio" = "06",
                    "julio" = "07",
                    "agosto" = "08",
                    "septiembre" = "09",
                    "octubre" = "10",
                    "noviembre" = "11",
                    "diciembre" = "12")

month_table2 <- list("Ene" = "enero", 
                    "Feb" = "febrero",
                    "Mar" = "marzo",
                    "Abr" ="abril",
                    "May" = "mayo",
                    "Jun" = "junio",
                    "Jul" = "julio",
                    "Ago" = "agosto",
                    "Sep" = "septiembre",
                    "Oct" = "octubre",
                    "Nov" = "noviembre",
                    "Dic" = "diciembre")

#This function creates a date for posterior use in time series
create_date <- function(anio, mes){
    year <- toString(anio)
    month <- month_table[[tolower(mes)]]
    day <- last_day(tolower(mes))
    as.Date(paste0(year,"-",month,"-",day))
}

datos$Date <- NA

for(i in 1:dim(datos)[1]){
    datos$Date[i] <- create_date(datos$Anio[i],datos$Mes[i])
}

# nombre_series <- unique(
#     tolower((as.character(unique(datos$Descripcion)))
#     )
    
principal <- list()
    
for(i in 1:292){ 
    x <- filter(datos, Codigo == codigos[i]) %>% arrange(Date)
    nombre <- as.character(x$Descripcion[1])
    nombre <- stringi::stri_trim_both(nombre)
    principal[[i]] <- ts(
        select(x, ipc:reg_8), 
        start = c(2011,4), end = c(2015,12),
        frequency=12)
    names(principal)[i] <- nombre
}

regiones <- list("Región Nacional"=1,"Región.I"=2,"Región.II"=3,"Región.III"=4,
     "Región.IV"=5, "Región.V"=6,"Región.VI"=7,"Región.VII"=8,"Región.VII"=9)

names(principal)[1] <- "IPC"
nombre_series <- names(principal)

meses <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
fechas <- c(paste0(meses,"-2011"),paste0(meses,"-2012"),paste0(meses,"-2013"),
            paste0(meses,"-2014"),paste0(meses,"-2015"))
fechas <- fechas[-(1:3)]
mesesf <- factor(substr(fechas,1,3))

adj_perc <- function(num){
    if(num >=0 & num < 2){
        out <- "leve"
    }else if(num < 4.5){
        out <- "moderada"
    }else{out <- "fuerte"}
    out
}
                    
intro <- "Figure 6.11 shows naïve forecasts of the seasonally adjusted electrical equipment orders data. These are then “reseasonalized” by adding in the seasonal naïve forecasts of the seasonal component. The resulting forecasts of the original data are shown in Figure 6.12. The prediction intervals shown in this graph are constructed in the same way as the point forecasts. That is, the upper and lower limits of the prediction intervals on the seasonally adjusted data are “reseasonalized” by adding in the forecasts of the seasonal component. "    