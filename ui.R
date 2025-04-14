library(shiny)
library(ggplot2)
library(ggiraph)
library(cmi)
library(shinyWidgets)

# fluidPage(
#   chooseSliderSkin("Flat", color = "#008CBA"),
#
#   # Static banner at the top with specified color and text
#   tags$div(
#     style = "background-color: #008CBA; width: 100%; padding: 18px; margin-bottom: 18px;",
#     tags$a(
#       href = "https://paulbeardactuarial.github.io/",
#       style = "color: white; text-decoration: none; font-weight: none; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 2rem;",
#       "paulbeardactuarial.github.io"
#     )
#   ),
#
#   # Main content
#   fluidRow(
#     # Left column: APCI parameters
#     column(
#       width = 6,
#       wellPanel(
#         style = "padding: 15px;",
#         tags$h4("APCI Parameters", style = "margin-top: 0;"),
#
#         # Smoothing parameters with reduced size
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             sliderInput('Sa',
#                         'Sa (smoothing parameter)',
#                         min = 6,
#                         max = 12,
#                         value = 7,
#                         step = 0.1,
#                         ticks = FALSE,
#                         round = 0)
#         ),
#
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             sliderInput('Sb',
#                         'Sb (smoothing parameter)',
#                         min = 6,
#                         max = 12,
#                         value = 7,
#                         step = 0.1,
#                         ticks = FALSE,
#                         round = 0)
#         ),
#
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             sliderInput('Sk',
#                         'Sk (smoothing parameter)',
#                         min = 6,
#                         max = 12,
#                         value = 7,
#                         step = 0.1,
#                         ticks = FALSE,
#                         round = 0)
#         ),
#
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             sliderInput('Sy',
#                         'Sy (smoothing parameter)',
#                         min = 6,
#                         max = 12,
#                         value = 7,
#                         step = 0.1,
#                         ticks = FALSE,
#                         round = 0)
#         ),
#
#         # Age and cohort range sliders
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             sliderInput("ageRange",
#                         "Age Range",
#                         min = 20,
#                         max = 100,
#                         value = c(20, 100),
#                         step = 1,
#                         ticks = FALSE)
#         ),
#
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             uiOutput("cohortRangeInput") # Dynamic UI for cohort range based on min age
#         )
#       )
#     ),
#
#     # Right column: Projection parameters
#     column(
#       width = 6,
#       wellPanel(
#         style = "padding: 15px;",
#         tags$h4("Projection Parameters", style = "margin-top: 0;"),
#
#         # LTR slider - FIXED: Changed 'label' to 'labels'
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             sliderInput("ltr",
#                         "LTR",
#                         min = -5,
#                         max = 10,
#                         value = 1,
#                         step = 0.5,
#                         ticks = FALSE,
#                         post = "%"
#             )
#         ),
#
#         # Taper Age slider (dynamic UI based on max age)
#         div(style = "transform: scale(0.9); transform-origin: left top;",
#             uiOutput("taperAgeInput")
#         ),
#
#         # Dropdown selection
#         selectInput("viewType",
#                     "View Type",
#                     choices = c("age", "cohort", "year"),
#                     selected = "age")
#       )
#     )
#   ),
#
#   # Heatmap output below both columns
#   fluidRow(
#     column(
#       width = 12,
#       style = "margin-top: 15px;",
#       girafeOutput('heatmap', height = "600px")
#     )
#   )
# )





fluidPage(
  chooseSliderSkin("Flat", color = "#008CBA"),

  # Static banner at the top with specified color and text
  tags$div(
    style = "background-color: #008CBA; width: 100%; padding: 18px; margin-bottom: 18px;",
    tags$a(
      href = "https://paulbeardactuarial.github.io/",
      style = "color: white; text-decoration: none; font-weight: none; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 2rem;",
      "paulbeardactuarial.github.io"
    )
  ),

  # Main content
  fluidRow(
    # Combined smoothing parameters
    column(
      width = 4,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("Smoothing Parameters", style = "margin-top: 0;"),

        # Age and cohort range sliders
        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("smoothAlpha",
                        "Sα",
                        min = 6,
                        max = 12,
                        value = 7,
                        step = 0.1,
                        ticks = FALSE)
        ),

        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("smoothBeta",
                        "Sβ",
                        min = 6,
                        max = 12,
                        value = 7,
                        step = 0.1,
                        ticks = FALSE)
        ),

        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("smoothGamma",
                        "Sγ",
                        min = 6,
                        max = 12,
                        value = 7,
                        step = 0.1,
                        ticks = FALSE)
        ),

        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("smoothKappa",
                        "Sκ",
                        min = 6,
                        max = 12,
                        value = 7,
                        step = 0.1,
                        ticks = FALSE)
        ),
      )
      ),

    column(
      width = 4,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("APCI Parameters", style = "margin-top: 0;"),

        # Age and cohort range sliders
        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("ageRange",
                        "Age Range",
                        min = 20,
                        max = 100,
                        value = c(20, 100),
                        step = 1,
                        ticks = FALSE)
        ),

        # Age and cohort range sliders
        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("yearRange",
                        "Year Range",
                        min = 1982,
                        max = 2022,
                        value = c(1982, 2022),
                        step = 1,
                        ticks = FALSE)
        ),

        div(style = "transform: scale(0.9); transform-origin: left top;",
            uiOutput("cohortRangeInput") # Dynamic UI for cohort range based on min age
        )
      )
    ),

    # Right column: Projection parameters
    column(
      width = 4,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("Projection Parameters", style = "margin-top: 0;"),

        # LTR slider
        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("ltr",
                        "LTR",
                        min = -5,
                        max = 10,
                        value = 0,
                        step = 0.5,
                        ticks = FALSE,
                        post = "%")
        ),

        # Taper Age slider (dynamic UI based on max age)
        div(style = "transform: scale(0.9); transform-origin: left top;",
            uiOutput("taperAgeInput")
        ),

        # additionalImprove slider
        div(style = "transform: scale(0.9); transform-origin: left top;",
            sliderInput("additionalImprove",
                        "Additional Improvement",
                        min = -2,
                        max = 4,
                        value = 0,
                        step = 0.1,
                        ticks = FALSE,
                        post = "%")
        ),

      )
    )
  ),

  # Heatmap output below both columns
  fluidRow(
    column(
      width = 1,
      # Dropdown selection
      prettyRadioButtons(
        inputId = "viewType",
        label = "View Type",
        thick = TRUE,
        choices = c("age", "cohort", "year"),
        animation = "pulse",
        status = "info"
      )
    ),
    column(
      width = 11,
      style = "margin-top: 15px;",
      girafeOutput('heatmap', height = "600px")
    )
  )
)
