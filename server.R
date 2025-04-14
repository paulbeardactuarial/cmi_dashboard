library(shiny)
library(ggplot2)
library(ggiraph)
library(remotes)
library(withr)
library(scales)
library(patchwork)

if (!require("cmi")) {
  remotes::install_github("paulbeardactuarial/cmi")
}
library(cmi)

withr::with_seed(1,

                 {randomised_male_data <-
                   cmi::cmi_2022_dth_exp$male |>
                   dplyr::arrange(age, year) |>
                   dplyr::mutate(
                     age_reweight = rnorm(1, 0, 0.03),
                     .by = age
                   ) |>
                   dplyr::mutate(
                     age_reweight_minus_1 = dplyr::lag(age_reweight, 1, default = 0),
                     .by = year
                   ) |>
                   dplyr::mutate(
                     year_reweight = rnorm(1, 0, 0.03),
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




function(input, output, session) {

    # Dynamic Cohort Range Input that depends on minimum age
    output$cohortRangeInput <- renderUI({
      minCohort <- input$ageRange[1] # Min cohort must be >= min age

      sliderInput("cohortRange",
                  "Cohort Constraint Range",
                  min = minCohort,
                  max = 140,
                  value = c(minCohort, 110),
                  step = 1,
                  ticks = FALSE)
    })

    # Dynamic Taper Age Input that depends on maximum age
    taperAge <- reactiveVal(110)  # Initial value storage

    output$taperAgeInput <- renderUI({
      maxAge <- input$ageRange[2]

      sliderInput("taperAge",
                  "Tapered to Zero Age",
                  min = maxAge,
                  max = 120,
                  value = taperAge(),
                  step = 1,
                  ticks = FALSE)
    })

    observeEvent(input$taperAge, {
      if(!is.null(input$taperAge)) {
        taperAge(input$taperAge)  # Update stored value
      }
    })


  apci_solved <- reactive({
    rp <- cmi::rp
    rp$smoothing_params$kappa <- input$Sk
    cmi_proj_model <-
      cmi::CMI2022_model$new(
        gender = "male",
        dth_exp = randomised_male_data,
        rp = cmi::rp,
        projection_params = cmi::projection_params
      )
    cmi_proj_model$run()
    projected_mi <-
      cmi_proj_model$mortality_improvements_projected |>
      dplyr::filter(age <= 120 & year <= 2072) |>
      dplyr::mutate(cohort = year - age) |>
      dplyr::mutate(row_number = dplyr::row_number())
    return(projected_mi)
  })

  dataset <- reactive({
    rp <- cmi::rp
    rp$smoothing_params$kappa <- input$Sk
    cmi_proj_model <-
      cmi::CMI2022_model$new(
        gender = "male",
        dth_exp = randomised_male_data,
        rp = cmi::rp,
        projection_params = cmi::projection_params
      )
    cmi_proj_model$run()
    projected_mi <-
      cmi_proj_model$mortality_improvements_projected |>
      dplyr::filter(age <= 120 & year <= 2072) |>
      dplyr::mutate(cohort = year - age) |>
      dplyr::mutate(row_number = dplyr::row_number())
    return(projected_mi)
  })

  output$heatmap <- renderGirafe({

    p1 <-
      dataset() |>
      ggplot(aes(x = year, y = age, fill = mi)) +
      geom_tile() +
      geom_tile_interactive(aes(data_id = cohort)) +
      theme_classic() +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = c(-0.06, 0.06),
        labels = scales::percent_format()
      )


    p2 <-
      dataset() |>
      ggplot(aes(x = year, y = mi, group = cohort)) +
      geom_line_interactive(aes(data_id = cohort), color = "grey", linewidth = 0.5) +
      scale_y_continuous(
        name = "q imp (%)",
        labels = scales::percent_format()) +
      theme_classic()

    p <- p1 + p2

    ip <-
    girafe(ggobj = p,
           width_svg  = 12,
           height_svg  = 6,
           options =
             list(
               opts_hover(css = "stroke:black;color:black;line-width:20px"),
               opts_hover_inv(css = "opacity:0.25;")
             )
    )

    return(ip)
  })


}





#
# rp <- cmi::rp
# rp$smoothing_params$kappa <- 7
# cmi_proj_model <-
#   cmi::CMI2022_model$new(
#     gender = "male",
#     dth_exp = randomised_male_data,
#     rp = cmi::rp,
#     projection_params = cmi::projection_params
#   )
# cmi_proj_model$run()
# projected_mi <-
#   cmi_proj_model$mortality_improvements_projected |>
#   dplyr::filter(age <= 120 & year <= 2072) |>
#   dplyr::mutate(cohort = year - age) |>
#   dplyr::mutate(row_number = dplyr::row_number())
#
# projected_mi |>
# ggplot(aes(x = year, y = mi, group = cohort)) +
#   geom_line_interactive(aes(data_id = cohort), color = "grey", linewidth = 0.3) +
#   theme_classic()
