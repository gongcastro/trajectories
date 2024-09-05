test_participants <- function(participants) {
  test_that("participants variable names are right", {
    expect_true(all(
      colnames(participants) %in% c(
        "child_id",
        "response_id",
        "time",
        "time_stamp",
        "list",
        "age",
        "sex",
        "lp",
        "doe_catalan",
        "doe_spanish",
        "edu_parent"
      )
    ))
  })

  test_that("participants variable classes are right", {
    expect_type(participants$child_id, "character")
    expect_type(participants$response_id, "character")
    expect_type(participants$time, "integer")
    expect_type(participants$time_stamp, "double")
    expect_type(participants$list, "character")
    expect_type(participants$age, "double")
    expect_type(participants$lp, "character")
    expect_type(participants$sex, "character")
    expect_type(participants$doe_catalan, "double")
    expect_type(participants$doe_spanish, "double")
    expect_type(participants$edu_parent, "integer")
  })

  test_that("same id does not repeat time", {
    expect_equal(max(count(participants, child_id, time)$n), 1)
  })

  test_that("list has appropriate values", {
    expect_true(all(
      participants$list %in% c(
        "bvq-short",
        "bvq-lockdown"
      )
    ))
  })

  test_that("age is positive and within the appropriate range", {
    expect_gt(min(participants$age), 12)
    expect_lte(max(participants$age), 34)
  })

  test_that("lp is 'Monolingual' or 'Bilingual'", {
    expect_true(all(participants$lp %in% c("Monolingual", "Bilingual")))
  })

  test_that("doe_catalan is between 0 and 1", {
    expect_gte(min(participants$doe_catalan), 0)
    expect_lte(max(participants$doe_catalan), 1)
  })

  test_that("doe_spanish is between 0 and 1", {
    expect_gte(min(participants$doe_spanish), 0)
    expect_lte(max(participants$doe_spanish), 1)
  })

  test_that("sum of doe_spanish and doe_catalan does not exceed 1", {
    expect_lte(
      max(participants$doe_catalan + participants$doe_spanish),
      1
    )
  })

  test_that("sum of doe_spanish and doe_catalan exceeds or is equal to 0.8", {
    expect_gte(
      min(participants$doe_catalan + participants$doe_spanish),
      0.8
    )
  })
}
