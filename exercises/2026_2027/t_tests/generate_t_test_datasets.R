source_url <- stringr::str_c(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
  "educ_uoe_enra21?format=TSV&compressed=false",
  collapse = ""
)

eu_country_codes <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
  "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL",
  "PT", "RO", "SE", "SI", "SK"
)
non_eu_country_codes <- c("CH", "IS", "LI", "ME", "MK", "NO", "RS", "TR")
analysis_country_codes <- c(eu_country_codes, non_eu_country_codes)

script_file <- commandArgs(trailingOnly = FALSE) |>
  stringr::str_subset(pattern = "^--file=") |>
  stringr::str_remove(pattern = "^--file=")
script_directory <- dirname(
  path = normalizePath(
    path = script_file,
    mustWork = TRUE
  )
)

source_connection <- curl::curl(url = source_url)
on.exit(
  expr = close(con = source_connection),
  add = TRUE
)

source_data <- readr::read_tsv(
  file = source_connection,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
)

participation_rates <- source_data |>
  tidyr::separate_wider_delim(
    cols = "freq,sex,unit,geo\\TIME_PERIOD",
    delim = ",",
    names = c("frequency", "sex", "unit", "country")
  ) |>
  tidyr::pivot_longer(
    cols = tidyselect::matches(match = "^20[0-9]{2}$"),
    names_to = "year",
    values_to = "value_with_flag"
  ) |>
  dplyr::mutate(
    preschool_participation_pct = readr::parse_double(
      x = stringr::str_extract(
        string = value_with_flag,
        pattern = "^[0-9]+(?:\\.[0-9]+)?"
      )
    ),
    country_group = dplyr::if_else(
      condition = country %in% eu_country_codes,
      true = "EU member state",
      false = "Non-EU European state"
    )
  ) |>
  dplyr::filter(
    frequency == "A",
    sex == "T",
    unit == "PC",
    country %in% analysis_country_codes
  ) |>
  dplyr::select(
    country,
    country_group,
    year,
    preschool_participation_pct
  )

rates_2019 <- participation_rates |>
  dplyr::filter(
    year == "2019",
    !is.na(preschool_participation_pct)
  )

z_test <- rates_2019 |>
  dplyr::filter(country_group == "EU member state") |>
  dplyr::arrange(country) |>
  dplyr::select(preschool_participation_pct)

one_sample_t_test <- rates_2019 |>
  dplyr::arrange(country) |>
  dplyr::select(preschool_participation_pct)

independent_samples_t_test <- rates_2019 |>
  dplyr::arrange(country_group, country) |>
  dplyr::select(
    country_group,
    preschool_participation_pct
  )

dependent_samples_t_test <- participation_rates |>
  dplyr::filter(year %in% c("2018", "2019")) |>
  tidyr::pivot_wider(
    id_cols = country,
    names_from = year,
    values_from = preschool_participation_pct,
    names_prefix = "preschool_participation_"
  ) |>
  dplyr::filter(
    !is.na(preschool_participation_2018),
    !is.na(preschool_participation_2019)
  ) |>
  dplyr::arrange(country) |>
  dplyr::select(
    preschool_participation_2018,
    preschool_participation_2019
  )

historical_population_sd <- participation_rates |>
  dplyr::filter(
    country_group == "EU member state",
    year %in% c(
      "2013", "2014", "2015", "2016", "2017", "2018"
    ),
    !is.na(preschool_participation_pct)
  ) |>
  dplyr::pull(preschool_participation_pct) |>
  stats::sd(x = _)

z_test_parameters <- tibble::tibble(
  hypothesised_mean_pct = 90,
  historical_population_sd_pct = round(
    x = historical_population_sd,
    digits = 2
  )
)

if (nrow(z_test) < 2L || nrow(one_sample_t_test) < 2L ||
    nrow(independent_samples_t_test) < 4L ||
    nrow(dependent_samples_t_test) < 2L) {
  stop("Eurostat returned too few complete observations for the exercises.")
}

readr::write_csv(
  x = z_test,
  file = file.path(script_directory, "z_test.csv")
)
readr::write_csv(
  x = z_test_parameters,
  file = file.path(script_directory, "z_test_parameters.csv")
)
readr::write_csv(
  x = one_sample_t_test,
  file = file.path(script_directory, "one_sample_t_test.csv")
)
readr::write_csv(
  x = independent_samples_t_test,
  file = file.path(script_directory, "independent_samples_t_test.csv")
)
readr::write_csv(
  x = dependent_samples_t_test,
  file = file.path(script_directory, "dependent_samples_t_test.csv")
)
