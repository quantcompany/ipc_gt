
shinyUI(fluidPage(
    
  fluidRow(
      column(12,
      tags$h1("Análisis de las series del IPC de Guatemala",
              style = "font-family: 'Palatino Linotype'; font-size: 3em; color:#008CFF")
      )
  ),
  fluidRow(
      column(12, tags$p(intro, style = "font-family: 'Lato', sans-serif; font-size: 15pt"))
  ),
  
  fluidRow(
      column(2),
      column(4, wellPanel(
            h3("Elija Una Serie"),
            selectInput(inputId = "serie",
                        label = "",
                        choices = nombre_series)
        )),
      column(4, wellPanel(
          h3("Elija Una Región"),
          selectInput(inputId = "region",
                      label = "",
                      choices = regiones)
          
      )),
      column(2)
  ),
  
  fluidRow(
      column(12, h3(textOutput("titulo"), align = "center"))
  ),
  
  fluidRow(
      column(6, showOutput("ind_tend",lib = "highcharts")),
      column(6, showOutput("estacion",lib = "highcharts"))
  ),
  fluidRow(
      column(6, h4(textOutput("text1"))),
      column(6, h4(textOutput("text2")))
  ),
  fluidRow(
      column(6, showOutput("shock",lib = "highcharts")),
      column(6)
  )

))
