context("test-boot.R")

grp2 <- c(rep("a", 30), rep("b", 60))
grp3 <- c(rep("a", 10), rep("b", 60), rep("c", 20))
x <- c(round(rnorm(30, 20, 4)), round(rnorm(60, 22, 3)))

test_data <- data.frame(
  group2 = grp2,
  group3 = grp3,
  group2_fct = as.factor(grp2),
  group3_fct = as.factor(grp3),
  x,
  stringsAsFactors = FALSE)

test_that("only two unique group values are accepted", {
  test_data %>% validate_grouping(group2) %>% expect_true()
  test_data %>% validate_grouping(group2_fct) %>% expect_true()

  expect_error(validate_grouping(test_data, group3),
               "Groupings should yield two values")

  expect_error(validate_grouping(test_data, group3_fct),
               "Groupings should yield two values")
})

test_that("logical statements can define groups", {
  test_data %>% validate_grouping(group2 == "a") %>% expect_true()
  test_data %>% validate_grouping(group3 == "a") %>% expect_true()
  test_data %>% validate_grouping(group2_fct == "a") %>% expect_true()
  test_data %>% validate_grouping(group3_fct == "a") %>% expect_true()
})

