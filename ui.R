
shinyUI(fluidPage(
    tags$head(HTML(FB_metas), HTML(fuente)), 
  fluidRow(
      column(12,
      tags$h1("Análisis de las series del IPC de Guatemala",
              style = "font-family: 'Palatino Linotype'; font-size: 3em; color:#434348"),
      tags$i(tags$h3("Por: Alvaro Fuentes", style = "color:#017890"))
      )
  ),
  fluidRow(column(1,HTML(twitter)),column(1,HTML(facebook_button)),column(10)),
  fluidRow(
       column(12, tags$div(HTML(intro), style = "font-family: 'Lato', sans-serif; font-size: 14.5pt"))
  ),
  tags$hr(),
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
  
  tags$hr(),
  
  fluidRow(
      column(6, showOutput("shock",lib = "highcharts")),
      column(6, showOutput("forecast",lib = "highcharts"))
  ),
  fluidRow(
      column(6, h4(textOutput("text3"))),
      column(6, h4(textOutput("text4")))
  ),
  
    tags$hr(),
    HTML(creditos)
    

))
