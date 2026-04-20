test_that("coerce_tac_column_name handles TAC/tac casing", {
  upper <- tibble::tibble(TAC = c(1, 2), frame_mid = c(10, 20))
  lower <- tibble::tibble(tac = c(1, 2), frame_mid = c(10, 20))

  expect_true("tac" %in% names(coerce_tac_column_name(upper, target_col = "tac")))
  expect_true("TAC" %in% names(coerce_tac_column_name(lower, target_col = "TAC", candidates = c("TAC", "tac"))))
})

test_that("coerce_tac_column_name errors when no TAC-like column exists", {
  no_tac <- tibble::tibble(frame_mid = c(10, 20), value = c(1, 2))

  expect_error(
    coerce_tac_column_name(no_tac, target_col = "tac"),
    "Could not find any TAC column to rename"
  )
})
