library(EdSurvey)

set.seed(19910930)

eclsk <- readECLS_K1998("data/ECLS_K/1998")

schools <- sample(1:3000, 100)

dplyr::tibble(
  school_id = eclsk$s1_id,
  child_id = eclsk$childid,
  child_gender = eclsk$gender,
  child_age = eclsk$r1_kage,
  child_reading_irt = eclsk$c1r4rscl,
  child_knows_letter_names = eclsk$t1letter,
  child_motor_skills = eclsk$c1fmotor,
  socioeconomic_status = eclsk$wksesl,
  parent_read_to_child = eclsk$p1readbo,
  mother_education_raw = eclsk$wkmomed,
  father_education_raw = eclsk$wkdaded,
  school_type = eclsk$s2kpupri,
  primary_non_parental = eclsk$p1primpk
) |>
  dplyr::filter(school_id %in% schools) |>
  tidyr::drop_na() |>
  dplyr::filter(
    !(child_knows_letter_names %in% c("NOT APPLICABLE", "NOT ASCERTAINED")),
    parent_read_to_child != "NOT ASCERTAINED",
    !(mother_education_raw %in% c("NOT APPLICABLE", "NOT ASCERTAINED")),
    !(father_education_raw %in% c("NOT APPLICABLE", "NOT ASCERTAINED"))
  ) |>
  dplyr::mutate(
    mother_education = dplyr::case_when(
      mother_education_raw %in% c("8TH GRADE OR BELOW", "9TH - 12TH GRADE") ~ "LOW",
      mother_education_raw %in% c("HIGH SCHOOL DIPLOMA/EQUIVALENT", "VOC/TECH PROGRAM", "SOME COLLEGE") ~ "MEDIUM",
      mother_education_raw %in% c("BACHELOR'S DEGREE", "GRADUATE/PROFESSIONAL SCHOOL-NO DEGREE", "MASTER'S DEGREE (MA, MS)", "DOCTORATE OR PROFESSIONAL DEGREE (PHD, MD, ETC.)") ~ "HIGH",
      TRUE ~ NA_character_
    ),
    father_education = dplyr::case_when(
      father_education_raw %in% c("8TH GRADE OR BELOW", "9TH - 12TH GRADE") ~ "LOW",
      father_education_raw %in% c("HIGH SCHOOL DIPLOMA/EQUIVALENT", "VOC/TECH PROGRAM", "SOME COLLEGE") ~ "MEDIUM",
      father_education_raw %in% c("BACHELOR'S DEGREE", "GRADUATE/PROFESSIONAL SCHOOL-NO DEGREE", "MASTER'S DEGREE (MA, MS)", "DOCTORATE OR PROFESSIONAL DEGREE (PHD, MD, ETC.)") ~ "HIGH",
      TRUE ~ NA_character_,
    ),
    parent_read_to_child = dplyr::case_when(
      parent_read_to_child %in% c("NOT AT ALL", "ONCE OR TWICE A WEEK") ~ "LESS THAN TWICE A WEEK",
      parent_read_to_child == "3 TO 6 TIMES A WEEK" ~ "3 TO 6 TIMES A WEEK",
      parent_read_to_child == "EVERYDAY" ~ "EVERYDAY"
    ),
    primary_non_parental = dplyr::case_when(
      primary_non_parental == "NO NON-PARENTAL CARE" ~ "NO NON-PARENTAL CARE",
      primary_non_parental %in% c("RELATIVE CARE, CHILD'S HOME", "RELATIVE CARE, OTHER'S HOME" ) ~ "RELATIVE CARE",
      primary_non_parental %in% c("NON-REL CARE, CHILD'S HOME", "NON-REL CARE, OTHER'S HOME" ) ~ "NON-RELATIVE CARE",
      primary_non_parental == "CENTER-BASED PROGRAM" ~ "CENTER-BASED PROGRAM",
      TRUE ~ NA
    )
  ) |>
  dplyr::mutate(
    child_knows_letter_names = dplyr::case_when(
      child_knows_letter_names %in% c("NOT YET", "BEGINNING") ~ 0,
      TRUE ~ 1
    )
  ) |>
  dplyr::select(-mother_education_raw, -father_education_raw) |>
  tidyr::drop_na() |>
  readr::write_csv("data/sample_child.csv")
