context("test-univariate.R")

test_that("Simple matching", {
  df <- tibble::data_frame(
    Group = c(rep("Treatment", 5), rep("Control", 5)) ,
    ID = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
    Age = c(10, 12, 11, 12, NA, 9, 12, 13, 14, 11)
  )

  set.seed(20180116)
  m1 <- suppressMessages({
    boot_match_univariate(df, Group, Age, caliper = 0, boot = 10, id = ID)
  })

  expect_true(all(c("j", "d", "c", "g") %in% m1$Matching_MatchID))

  expect_equal(
    m1$Matching,
    c("unmatchable", "unmatched", "matched", "matched", "missing-data",
      "unmatchable", "matched", "unmatchable", "unmatchable", "matched"))

  set.seed(20180116)
  m2 <- suppressMessages({
    boot_match_univariate(df, Group, Age, caliper = 1, boot = 10, id = ID)
  })

  expect_equal(sum(m2$Matching == "matched"), 8)
})
