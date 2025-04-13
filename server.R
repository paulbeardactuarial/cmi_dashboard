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

  # Create a reactive value to store the selected row number
  selected_row <- reactiveVal(NULL)

  # Observer to update the selected row when user clicks on the heatmap
  observeEvent(input$heatmap_selected, {
    # Update the selected row value
    selected_row(input$heatmap_selected)
  })

  output$plot_by_year <- renderGirafe({
    row_num <- selected_row()
    if (is.null(row_num)) {
      return()
    } else {

    selected_data <- dataset() |> dplyr::filter(row_number == row_num)
    clicked_year <- selected_data[["year"]]
    clicked_age <- selected_data[["age"]]
    clicked_cohort <- selected_data[["cohort"]]


    age_filter_df <- dataset() |> dplyr::filter(age == clicked_age) |> dplyr::filter(!is.na(mi))
    cohort_filter_df <- dataset() |> dplyr::filter(cohort == clicked_cohort) |> dplyr::filter(!is.na(mi))
    mi_range <- range(c(age_filter_df$mi, cohort_filter_df$mi))
    year_range <- range(c(age_filter_df$year, cohort_filter_df$year))

    p1 <-
      age_filter_df |>
      ggplot(aes(x = year, y = mi)) +
      scale_y_continuous(labels = scales::percent_format(), limits = mi_range) +
      scale_x_continuous(limits = year_range) +
      geom_line(size = 1.5) +
      theme_classic() +
      labs(
        title = glue::glue("Mortality improvements for age {clicked_age}")
      )
    p2 <-
      cohort_filter_df |>
      ggplot(aes(x = year, y = mi)) +
      scale_y_continuous(labels = scales::percent_format(), limits = mi_range) +
      scale_x_continuous(limits = year_range) +
      geom_line(size = 1.5) +
      theme_classic() +
      labs(
        title = glue::glue("Mortality improvements when born in {clicked_cohort}")
      )
    p <- p1 + p2
    ip <-
      girafe(
        ggobj = p,
        width_svg = 12,  # Set a base width
        height_svg = 4,  # Half the width
        options = list(
          opts_hover(css = "fill:grey;stroke:black;stroke-width:1px;")
        )
      )
    return(ip)
    }
  })

  # Create output for displaying the selected point details
  output$selected_point_info <- renderUI({
    # Get the selected row number
    row_num <- selected_row()

    if (is.null(row_num)) {
      return(HTML("<p>Click on a point in the heatmap to see details.</p>"))
    } else {
      # Look up the row in dataset
      selected_data <- dataset() |>
        dplyr::filter(row_number == row_num)

      # Extract values
      if (nrow(selected_data) > 0) {
        age_val <- selected_data$age[1]
        year_val <- selected_data$year[1]
        cohort_val <- selected_data$cohort[1]
        mi_val <- selected_data$mi[1]

        # Format the output
        return(HTML(sprintf(
          "<p><strong>Selected point details:</strong><br>
          Age: %d<br>
          Year: %d<br>
          Cohort: %d<br>
          Mortality Improvement: %.4f%%</p>",
          age_val, year_val, cohort_val, mi_val * 100
        )))
      } else {
        return(HTML("<p>Selected point data not found.</p>"))
      }
    }
  })

  output$heatmap <- renderGirafe({

    row_num <- selected_row()

    if (is.null(row_num)) {
      plot_data <- dataset()
      mark_clicks_black <- NULL
    } else {
    selected_data <- dataset() |> dplyr::filter(row_number == row_num)
    clicked_year <- selected_data[["year"]]
    clicked_age <- selected_data[["age"]]
    clicked_cohort <- selected_data[["cohort"]]
    plot_data <- dataset() |> dplyr::mutate(highlight = age %in% clicked_age | cohort %in% clicked_cohort)
    mark_clicks_black <- list(
      geom_tile(aes(alpha = highlight), fill = "black"),
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0))
    )
    }

    p <-
      plot_data |>
      ggplot(aes(x = year, y = age, fill = mi)) +
      geom_tile_interactive(aes(data_id = row_number)) +
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

    ip <-
      girafe(
        ggobj = p,
        options = list(
          opts_hover(css = "fill:grey;stroke:black;stroke-width:0.1px;"),
          opts_selection(
            type = "single",
            only_shiny = FALSE,
            css = "fill:black;stroke:black;stroke-width:1px;")
        )
      )
    return(ip)
  })
}




#
#
#
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
#
# p <-
# projected_mi |>
#   ggplot(aes(x = year, y = mi, group = age)) +
#   geom_line_interactive(aes(data_id = age), color = "grey") +
#   theme_classic()
#
# girafe(ggobj = p,
#        options =
#          list(
#            opts_hover(css = "stroke:black;"),
#            opts_hover_inv(css = "opacity:0.5;")
#          )
#        )
