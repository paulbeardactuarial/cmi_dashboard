library(shiny)
library(ggplot2)
library(ggiraph)
library(remotes)
library(withr)
if (!require("cmi")) {
  remotes::install_github("paulbeardactuarial/cmi", auth_token = Sys.getenv("GITHUB_PAT"))
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



function(input, output) {

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

  p <-
    dataset() |>
    ggplot(aes(x = year, y = age, fill = mi)) +
    geom_tile_interactive(aes(data_id = cohort)) +
    theme_classic() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-0.06, 0.06)
    )

  ip <-
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "fill:grey;stroke:black;stroke-width:1px;"),
        opts_selection(
          type = "single",
          only_shiny = FALSE,
          css = "fill:black;stroke:black;stroke-width:1px;")
      )
    )

  return(ip)
  })



}
