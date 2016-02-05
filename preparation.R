library(dplyr)
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

names(principal)[1] <- "IPC"

save(principal,file = "data/datos.RData")