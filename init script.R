library(cmi)

remotes::install_git("https://github.com/paulbeardactuarial/cmi")


new_run <- cmi::CMI2022_model$new(gender = "male")


new_run$run()

library(rsconnect)
rsconnect::deployApp('path/to/your/app')

new_male_data <-
cmi::cmi_2022_dth_exp$male |>
  dplyr::mutate(
    age_reweight = rnorm(1, 0, 0.05),
    .by = age
  ) |>
  dplyr::mutate(
    year_reweight = rnorm(1, 0, 0.05),
    .by = year
  ) |>
  dplyr::mutate(
    exposure = exposure * (1 + age_reweight) * (1 + year_reweight)
  ) |>
  dplyr::select(-c(age_reweight, year_reweight))


cmi::cmi_2022_dth_exp$male |>
  dplyr::left_join(new_male_data, by = c("age", "year", "deaths"), suffix = c("_og", "_nu")) |>
  dplyr::mutate(change = exposure_nu / exposure_og - 1) |>
  ggplot(aes(x = year, y = change, color = age)) +
  geom_point()
