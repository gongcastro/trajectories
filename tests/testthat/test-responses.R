test_responses <- function(responses) {
  test_that("responses variable names are right", {
    expect_true(all(
      colnames(responses) %in% c(
        "child_id",
        "response_id",
        "time",
        "age",
        "age_std",
        "te",
        "language",
        "meaning",
        "item",
        "response",
        "lv",
        "lv_std",
        "freq",
        "freq_std",
        "n_phon",
        "n_phon_std",
        "doe",
        "doe_std",
        "exposure",
        "exposure_std"
      )
    ))
  })

  test_that("responses variable classes are right", {
    expect_type(responses$child_id, "character")
    expect_type(responses$response_id, "character")
    expect_type(responses$time, "integer")
    expect_type(responses$age, "double")
    expect_type(responses$age_std, "double")
    expect_type(responses$te, "integer")
    expect_type(responses$language, "character")
    expect_type(responses$meaning, "character")
    expect_type(responses$item, "character")
    expect_type(responses$response, "integer")
    expect_type(responses$lv, "double")
    expect_type(responses$lv_std, "double")
    expect_type(responses$freq, "double")
    expect_type(responses$freq_std, "double")
    expect_type(responses$n_phon, "integer")
    expect_type(responses$n_phon_std, "double")
    expect_type(responses$doe, "double")
    expect_type(responses$doe_std, "double")
    expect_type(responses$exposure, "double")
    expect_type(responses$exposure_std, "double")
  })

  test_that("response take values 1, 2, or 3", {
    expect_true(all(unique(responses$response) %in% c(
      "No",
      "Understands",
      "Understands and Says"
    )))
  })

  test_that("no times are duplicated for the same ID", {
    expect_lte(max(count(responses, child_id, time, language, item)$n), 1)
  })
}
