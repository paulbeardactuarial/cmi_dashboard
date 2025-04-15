library(shiny)
library(ggplot2)
library(ggiraph)
library(cmi)
library(shinyWidgets)




fluidPage(
  chooseSliderSkin("Shiny", color = "#008CBA"),

  # Static banner at the top with specified color and text
  tags$div(
    style = "background-color: #008CBA; width: 100%; padding: 18px; margin-bottom: 18px;",
    tags$a(
      href = "https://paulbeardactuarial.github.io/",
      style = "color: white; text-decoration: none; font-weight: none; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 2rem;",
      "paulbeardactuarial.github.io"
    )
  ),
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }

    "
  )),

  # Main content
  fluidRow(
    # Combined smoothing parameters
    column(
      width = 3,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("Smoothing Parameters", style = "margin-top: 0;"),

        # Age and cohort range sliders
        div(
          class = "label-left",
          sliderInput("smoothAlpha",
            "Sα",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$alpha,
            step = 0.1,
            ticks = FALSE
          )
        ),
        div(
          class = "label-left",
          sliderInput("smoothBeta",
            "Sβ",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$beta,
            step = 0.1,
            ticks = FALSE
          )
        ),
        div(
          class = "label-left",
          sliderInput("smoothGamma",
            "Sγ",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$gamma,
            step = 0.1,
            ticks = FALSE
          )
        ),
        div(
          class = "label-left",
          sliderInput("smoothKappa",
            "Sκ",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$kappa,
            step = 0.1,
            ticks = FALSE
          )
        ),
      )
    ),
    column(
      width = 4,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("APCI Parameters", style = "margin-top: 0;"),

        # Age and cohort range sliders
        div(
          style = "transform-origin: left top;",
          sliderInput("ageRange",
            "Age Range",
            min = 20,
            max = 100,
            value = c(cmi::rp$age$min, cmi::rp$age$max),
            step = 1,
            ticks = FALSE
          )
        ),

        # Age and cohort range sliders
        div(
          style = "transform-origin: left top;",
          sliderInput("yearRange",
            "Year Range",
            min = 1982,
            max = 2022,
            value = c(cmi::rp$year$min, cmi::rp$year$max),
            step = 1,
            ticks = FALSE,
            sep = ""
          )
        ),
        div(
          style = "transform-origin: left top;",
          uiOutput("cohortRangeInput") # Dynamic UI for cohort range based on min age
        ),
        actionButton(
          "click",
          "Solve APCI",
          icon = icon("calculator"),
          style = "background-color: #008CBA; color: white;"
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
        div(
          style = "transform-origin: left top;",
          sliderInput("ltr",
            "LTR",
            min = -2,
            max = 4,
            value = cmi::projection_params$ltr * 100,
            step = 0.1,
            ticks = FALSE,
            post = "%"
          )
        ),

        # Taper Age slider (dynamic UI based on max age)
        div(
          style = "transform-origin: left top;",
          uiOutput("taperAgeInput")
        ),

        # additionalImprove slider
        div(
          style = "transform-origin: left top;",
          sliderInput("additionalImprove",
            "Additional Improvement",
            min = -3,
            max = 3,
            value = cmi::projection_params$additional_improve * 100,
            step = 0.1,
            ticks = FALSE,
            post = "%"
          )
        ),
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      tags$div(
        style = "color: red; font-weight: bold; font-size: 16px;",
        textOutput("alignmentMessage")
      )
    )
  ),

  # Dropdown menu to pick the graph type shown (i.e. age, cohort or year)
  fluidRow(
    column(
      width = 4,
      offset = 1,
      # Dropdown selection
      prettyRadioButtons(
        inputId = "viewType",
        label = "View Type",
        thick = TRUE,
        inline = TRUE,
        choices = c("age", "cohort", "year"),
        animation = "pulse",
        status = "info"
      )
    )
  ),

  # the money
  fluidRow(
    column(
      width = 12,
      style = "margin-top: 15px;",
      girafeOutput("heatmap", height = "600px")
    )
  )
)
