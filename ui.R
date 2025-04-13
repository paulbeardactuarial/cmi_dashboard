library(shiny)
library(ggplot2)
library(ggiraph)
library(cmi)


fluidPage(

  titlePanel("CMI 2022 Test"),

  sidebarPanel(

    sliderInput('Sa',
                'Sa (smoothing parameter)',
                min=6,
                max=12,
                value=7,
                step=0.1,
                ticks =F,
                round=0)

  ,

  sliderInput('Sb',
              'Sb (smoothing parameter)',
              min=6,
              max=12,
              value=7,
              step=0.1,
              ticks =F,
              round=0)

,

sliderInput('Sk',
            'Sk (smoothing parameter)',
            min=6,
            max=12,
            value=7,
            step=0.1,
            ticks =F,
            round=0)

,

sliderInput('Sy',
            'Sy (smoothing parameter)',
            min=6,
            max=12,
            value=7,
            step=0.1,
            ticks =F,
            round=0),

htmlOutput("selected_point_info")
)
,


  mainPanel(
    girafeOutput('heatmap')
  ),

fluidRow(
  column(width = 12, girafeOutput("plot_by_year")
  )

)

)
















