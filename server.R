library(shiny)
library(ggplot2)
library(ggiraph)
library(remotes)
library(withr)
library(scales)
library(patchwork)

if (!require("cmi")) {
  remotes::install_github("paulbeardactuarial/cmi", force = T)
}
library(cmi)

max_iteration <- 500 ## <-- keeping max_iteration low as Shiny server could become overloaded if keep at 10,000. Will cause APCI to give up sometimes

withr::with_seed(1,

                 {randomised_male_data <-
                   cmi::cmi_2022_dth_exp$male |>
                   dplyr::arrange(age, year) |>
                   dplyr::mutate(
                     age_reweight = rnorm(1, 0, 0.05),
                     .by = age
                   ) |>
                   dplyr::mutate(
                     age_reweight_minus_1 = dplyr::lag(age_reweight, 1, default = 0),
                     .by = year
                   ) |>
                   dplyr::mutate(
                     year_reweight = rnorm(1, 0, 0.05),
                     .by = year
                   ) |>
                   dplyr::mutate(
                     year_reweight_minus_1 = dplyr::lag(year_reweight, 1, default = 0),
                     .by = age
                   ) |>
                   dplyr::mutate(
                     exposure =
                       exposure *
                       (1 + age_reweight) * (1 + year_reweight) *
                       (1 + 0.5 * age_reweight_minus_1) * (1 + 0.5 * year_reweight_minus_1)
                   ) |>
                   dplyr::select(age, year, deaths, exposure)}

)

extract_slider_vars_rp <-
  function(list) {
    list(
      list$smoothing_params$alpha,
      list$smoothing_params$beta,
      list$smoothing_params$kappa,
      list$smoothing_params$gamma,
      list$age$min,
      list$age$max,
      list$age$cohort_low,
      list$age$cohort_high,
      list$year$min,
      list$year$max
    )
  }


function(input, output, session) {


  # Dynamic Cohort Range Input that depends on minimum age
  # cohortRange <- reactiveVal(c(cmi::rp$age$cohort_low, cmi::rp$age$cohort_high))
  # output$cohortRangeInput <- renderUI({
  #   minCohort <- input$ageRange[1] # Min cohort must be >= min age
  #   cohortRangeSetting <- cohortRange()
  #   cohortRangeSetting[1] <- max(minCohort, cohortRangeSetting[1])
  #   sliderInput("cohortRange",
  #               "Cohort Constraint Range",
  #               min = minCohort,
  #               max = 140,
  #               value = c(cmi::rp$age$cohort_low, cmi::rp$age$cohort_high),
  #               step = 1,
  #               ticks = FALSE)
  # })

  # Dynamic Taper Age Input that depends on maximum age
  taperAge <- reactiveVal(cmi::projection_params$age_taper_zero)  # Initial value storage

  output$taperAgeInput <- renderUI({
    maxAge <- input$ageRange[2]

    sliderInput("taperAge",
                "Tapered to Zero Age",
                min = maxAge + 1,
                max = 120,
                value = cmi::projection_params$age_taper_zero,
                step = 1,
                ticks = FALSE)
  })

  # observeEvent(input$taperAge, {
  #   if(!is.null(input$taperAge)) {
  #     taperAge(input$taperAge)  # Update stored value
  #   }
  # })


  # run parameters

  runParametersReactive <- reactiveVal({cmi::rp})

  observe({
      rp <- runParametersReactive()
    rp$smoothing_params$alpha <- input$smoothAlpha
    rp$smoothing_params$beta <- input$smoothBeta
    rp$smoothing_params$kappa <- input$smoothKappa
    rp$smoothing_params$gamma <- input$smoothGamma
    rp$age$min <- input$ageRange[1]
    rp$age$max <- input$ageRange[2]
    rp$year$min <- input$yearRange[1]
    rp$year$max <- input$yearRange[2]
    rp$age$cohort_low <- input$cohortRange[1]
    rp$age$cohort_high <- input$cohortRange[2]
    runParametersReactive(rp)
    }
  )

  output$alignmentMessage <- renderUI({
    slider_rp <- runParametersReactive()
    model <- cmi_proj_model()
    model_rp <- model$rp
    if (
      identical(
        extract_slider_vars_rp(slider_rp),
        extract_slider_vars_rp(model_rp)
      )
    ) {
      ""
    } else {
      tagList(
        icon("exclamation-triangle", class = "text-warning"),
        HTML(" Parameter settings are not aligned to solved values. Click `Solve APCI` button to re-calculate."),
        icon("exclamation-triangle", class = "text-warning"),
      )
    }
  })

  output$taperAgeMessage <-
    renderText({
      taper_age <- input$taperAge
      max_age <- input$ageRange[2]
      if(taper_age > max_age) {
        ""
      } else {
        "Condition not met: Age Taper to Zero > Age Range Max."
      }
    })

  output$cohortAgeMessage <-
    renderText({
      min_cohort_age <- input$cohortRange[1]
      min_age <- input$ageRange[1]
      if(min_cohort_age >= min_age) {
        ""
      } else {
        "Condition not met: Cohort Constraint Range Min. >= Age Range Min."
      }
    })

  output$convergeFailMessage <-
    renderText({
      model <- cmi_proj_model()
      if(model$iteration_no < max_iteration) {
        ""
      } else {
        glue::glue("APCI Failed to solve after {max_iteration} iterations. Max. allowed iterations has been restricted due to limited server capacity.")
      }
    })



    # projection parameters

  projParametersReactive <- reactiveVal({cmi::projection_params})

  observe({
    pp <- projParametersReactive()
    pp$additional_improve <- input$additionalImprove/100
    pp$ltr <- input$ltr/100
    pp$age_taper_zero <- input$taperAge
    projParametersReactive(pp)
  }) |> debounce(250)

  cmi_proj_model <- eventReactive(

    input$click,{

      rp <- runParametersReactive()

    model <- cmi::CMI2022_model$new(
      gender = "male",
      dth_exp = randomised_male_data,
      rp = rp
    )
    model$solve_apci(max_iteration = max_iteration)
    return(model)
  },
  ignoreNULL = FALSE)

  dataset <- reactive({

    # the dynamic parts of the code...
    model <- cmi_proj_model()
    pp <- projParametersReactive()

    # return nothing in cases that have gone wrong
    if(
      input$cohortRange[1] < input$ageRange[1] |
      input$taperAge <= input$ageRange[2] |
      model$iteration_no >= max_iteration
    ) {
      return()
    }

    # project mortality from the solved apci model
    model$projection_params <- pp
    model$project_mortality_improvements()
    projected_mi <-
      model$mortality_improvements_projected |>
      dplyr::filter(age <= 120 & year <= 2072) |>
      dplyr::mutate(cohort = year - age) |>
      dplyr::mutate(row_number = dplyr::row_number())
    return(projected_mi)
  })

  output$heatmap <- renderGirafe({

    if(
      input$cohortRange[1] < input$ageRange[1] | input$taperAge <= input$ageRange[2]
    ) {
      return()
    }


    variable_used_for_p2 <- input$viewType

    p1 <-
      dataset() |>
      ggplot(aes(x = year, y = age, fill = mi)) +
      geom_tile() +
      geom_tile_interactive(
        aes(
          data_id = get(variable_used_for_p2),
          tooltip = paste0(variable_used_for_p2, ": ", get(variable_used_for_p2))
        )
      ) +
      theme_classic() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_gradientn(
        colours = c(
          "#7f3b08",
          "#b35806",
          "#e08214",
          "#fdb863",
          "#fee0b6",
          "#f7f7f7",
          "#d8daeb",
          "#b2abd2",
          "#8073ac",
          "#542788",
          "#2d004b"
          ),
        values = scales::rescale(seq(from = 0.06, to = -0.06, length.out = 11)),
        limits = c(-0.06, 0.06),
        labels = scales::percent_format()
      ) +
      theme(
        legend.position = "left"
      )


    p2_x_var <- ifelse(variable_used_for_p2 == "year", "age", "year")
    p2 <-
      dataset() |>
      ggplot(
        aes(
          x = get(p2_x_var),
          y = mi,
          group = get(variable_used_for_p2)
        )
      ) +
      geom_line_interactive(
        aes(
          data_id = get(variable_used_for_p2),
          tooltip = paste0(variable_used_for_p2, ": ", get(variable_used_for_p2))
        ),
        color = "grey",
        linewidth = 0.5) +
      scale_y_continuous(
        name = "q imp (%)",
        labels = scales::percent_format()) +
      scale_x_continuous(name = p2_x_var) +
      theme_classic()

    p <- p1 + p2

    ip <-
      girafe(ggobj = p,
             width_svg  = 14,
             height_svg  = 7,
             options =
               list(
                 opts_hover(css = "stroke:black;color:black;line-width:20px"),
                 opts_hover_inv(css = "opacity:0.2;"),
                 opts_tooltip(css = "background-color:#008CBA; color:white; padding:5px; border-radius:4px;")
               )
      )

    return(ip)
  })


}

