readr::read_csv("data/tools_of_the_mind_raw.csv") |>
  dplyr::select(
    c(
      "ID", "Condition", "ParentEd_R", "SchoolID",
      "ap_ws", "ap_ws.2", "AGE", "Sex"
    )
  ) |>
  dplyr::rename(
    c(
      "id" = "ID",
      "treatment" = "Condition",
      "parent_education" = "ParentEd_R",
      "school_id" = "SchoolID",
      "applied_prblems_w_score_fall" = "ap_ws",
      "applied_prblems_w_score_spring" = "ap_ws.2",
      "age" = "AGE",
      "sex" = "Sex"
    )
  ) |>
  dplyr::relocate("school_id", .after = "id") |>
  tidyr::drop_na() |>
  readr::write_csv("data/tools_of_the_mind.csv")
