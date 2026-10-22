# Preschool Participation T-Test Datasets

The CSV files are cleaned extracts from Eurostat dataset
[`educ_uoe_enra21`](https://doi.org/10.2908/EDUC_UOE_ENRA21), "Pupils aged 3
to the starting age of compulsory education at primary level by sex - % of
corresponding age population." The data come from the annual
UNESCO/OECD/Eurostat (UOE) collection. They measure participation in formal
education; national starting ages and programme definitions differ, so they
are suitable for teaching analysis mechanics rather than substantive
cross-country conclusions.

`generate_t_test_datasets.R` downloads the open Eurostat TSV endpoint,
selects total-sex participation rates, removes aggregates and missing values,
and generates every CSV in this directory. It is deterministic for a given
Eurostat release and uses the fixed reference years 2013--2019. Greece last
reported this indicator in 2019, so this is the latest reference year that
includes Greece; it also avoids pandemic-era disruption.

This directory is an isolated `renv` project, keeping its R 4.6-compatible
dependencies separate from the older lockfile at the repository root. Run the
script from this directory after restoring its locked packages:

```sh
cd exercises/2026_2027/t_tests
Rscript -e 'renv::restore(prompt = FALSE)'
Rscript generate_t_test_datasets.R
```

## Files

| File | jamovi analysis | Variables |
|---|---|---|
| `z_test.csv` | One-sample z test | `preschool_participation_pct` |
| `z_test_parameters.csv` | z-test settings | `hypothesised_mean_pct`, `historical_population_sd_pct` |
| `one_sample_t_test.csv` | One-sample t test | `preschool_participation_pct` |
| `independent_samples_t_test.csv` | Independent-samples t test | `country_group`, `preschool_participation_pct` |
| `dependent_samples_t_test.csv` | Paired-samples t test | `preschool_participation_2018`, `preschool_participation_2019` |

For the independent-samples exercise, each observation is a country and the
groups are EU-member and non-EU European states. Use Welch's t test rather
than assuming equal variances.

For the z-test exercise, use the parameters in `z_test_parameters.csv`.
The standard deviation is calculated from all non-missing 2013--2018 EU-country
rates as a historical reference value. It is supplied solely to demonstrate a z
test: it should not be described as a known population standard deviation in
substantive research.
