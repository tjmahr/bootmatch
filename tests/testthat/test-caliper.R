context("test-caliper.R")



test_that("create pairs that satisfy caliper constraints", {
  grp <- c(rep("a", 9), rep("b", 10))
  x <- c(
 #  1,  2,  3,  4,  5,   6,   7,   8,   9,
    0,  1,  2,  4,  5,  10,  12,  13,  15,
 # 10, 11, 12, 13, 14,  15,  16,  17,  18, 19
    0,  0,  3,  4,  6,   9,  11,  13,  18, 25)

  test_data <- data.frame(grp, x, stringsAsFactors = FALSE)

  # item 1 (0) matches item 10 (0) exactly
  # item 1 (0) matches item 11 (0) exactly
  # item 4 (4) matches item 13 (4) exactly
  # item 8 (13) matches item 17 (13) exactly
  pairs_exactly <- find_caliper_pairs(test_data, grp, x, caliper = 0)
  exact1 <- c(1, 1, 4, 8)
  exact2 <- c(10, 11, 13, 17)
  expect_equal(sort(pairs_exactly$.rowid1), exact1)
  expect_equal(sort(pairs_exactly$.rowid2), exact2)

  # exact matches plus...
  # item 2 (1) matches item 10 (0) within 1
  # item 2 (1) matches item 11 (0) within 1
  # item 3 (2) matches item 12 (3) within 1
  # item 4 (4) matches item 12 (3) within 1
  # item 5 (5) matches item 13 (4) within 1
  # item 5 (5) matches item 14 (6) within 1
  # item 6 (10) matches item 15 (9) within 1
  # item 6 (10) matches item 16 (11) within 1
  # item 7 (12) matches item 16 (11) within 1
  # item 7 (12) matches item 17 (13) within 1

  within1_1 <- sort(c(exact1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7))
  within1_2 <- sort(c(exact2, 10, 11, 12, 12, 13, 14, 15, 16, 16, 17))

  pairs_within1 <- find_caliper_pairs(test_data, grp, x, caliper = 1)
  expect_equal(sort(pairs_within1$.rowid1), within1_1)
  expect_equal(sort(pairs_within1$.rowid2), within1_2)

  # within 1 matches plus...
  # item 2 (1) matches item 12 (3) within 2
  # item 3 (2) matches item 10 (0) within 2
  # item 3 (2) matches item 11 (0) within 2
  # item 3 (2) matches item 13 (4) within 2
  # item 4 (4) matches item 14 (6) within 2
  # item 5 (5) matches item 12 (3) within 2
  # item 8 (13) matches item 16 (11) within 2
  # item 9 (15) matches item 17 (13) within 2

  within2_1 <- sort(c(within1_1, 2, 3, 3, 3, 4, 5, 8, 9))
  within2_2 <- sort(c(within1_2, 12, 10, 11, 13, 14, 12, 16, 17))

  pairs_within2 <- find_caliper_pairs(test_data, grp, x, caliper = 2)
  expect_equal(sort(pairs_within2$.rowid1), within2_1)
  expect_equal(sort(pairs_within2$.rowid2), within2_2)

  pairs_all <- find_caliper_pairs(test_data, grp, x, caliper = 100)
  expect_equal(nrow(pairs_all), 90)
})
