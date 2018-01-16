#' @keywords internal
"_PACKAGE"

# y <- c(rep("a", 30), rep("b", 60))
# x <- c(round(rnorm(30, 20, 4)), round(rnorm(60, 22, 3)))
# data <- data.frame(y, x, stringsAsFactors = FALSE)
# y <- rlang::quo(y)
# x <- rlang::quo(x)

bootstrap_match <- function(matchable) {
  done <- dplyr::slice(matchable, 0L)
  while (nrow(matchable) > 0L) {
    next_match <- dplyr::sample_n(matchable, 1L)
    matchable <- matchable[matchable$control != next_match$control &
                           matchable$treated != next_match$treated, ]
    done <- dplyr::bind_rows(done, next_match)
  }
  done
}

prepare_data <- function(data, y, x) {
  y <- enquo(y)
  x <- enquo(x)

  # x_name <- data %>% dplyr::select(!! x) %>% names()
  x_name <- quo_name(x)


  if (validate_grouping(data, y)) {
    y_vals <- eval_tidy(y, data)
    # If y is a character vector,
    # treat it as a factor for the purposes of numbering groups
    if (is.character(y_vals)) {
      y <- quo(as.factor(!! y))
    }
  }

  data <- tibble::rowid_to_column(data, ".rowid")


  matching_df <- data %>%
    dplyr::mutate(.group = as.numeric(!! y)) %>%
    dplyr::select(.data$.rowid, .data$.group, !! x) %>%
    stats::na.omit()

  # Clean up some metadata attached by na.omit
  attr(matching_df, "na.action") <- NULL
  matching_df <- tibble::as_tibble(matching_df)

  # todo...

}

# Confirm that the y variable yields exactly 2 unique values
validate_grouping <- function(data, y) {
  y <- enquo(y)
  y_vals <- eval_tidy(y, data)

  if (length(unique(y_vals)) != 2) {
    values <- as.list(dplyr::distinct(data, !! y))
    comma_sep <- paste0(values[[1]], collapse = ", ")
    msg <- glue::glue(
      "Groupings should yield two values.\n",
      "{glue::backtick(names(values))} has values: {comma_sep}")
    stop(msg, call. = FALSE)
  }

  TRUE
}
