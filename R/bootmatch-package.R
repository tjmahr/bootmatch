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

prepare_data <- function(data, y, x, caliper = 0) {
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


  # Keep only participants within +- caliper values of each other

  boundaries <- matching_df %>%
    dplyr::group_by(.data$.group) %>%
    dplyr::summarise(GroupMin = min(!! x), GroupMax = max(!! x))

  lower_edge <- max(boundaries$GroupMin) - caliper
  upper_edge <- min(boundaries$GroupMax) + caliper

  trimmed_df <- matching_df %>%
    dplyr::filter(dplyr::between(!! x, lower_edge, upper_edge))


  # group_1 %>%
  #   dplyr::mutate(possible = rep(list(group_2), nrow(group_1)))
  #
  #
  # # Keep only pairs within a caliper distance
  # f <- rlang::new_formula(quote(.group), rlang::get_expr(x))
  # ccc <- optmatch::match_on(f, data = trimmed_df, method = "euclidean",
  #                           caliper = caliper_val)
  #
  # template <- reshape2::melt(as.matrix(ccc)) %>%
  #   filter(is.finite(value)) %>%
  #   as_tibble()
  #
  # # Do a bunch of matches
  # tries <- purrr::rerun(boot, bootstrap_match(template))
  #
  # # Keep biggest N's
  # best_n <- max(map_int(tries, nrow))
  # best_tries <- tries %>%
  #   purrr::keep(~ nrow(.x) == best_n)
  #
  # # Keep smallest z's
  # best_match <- best_tries %>%
  #   purrr::map(function(try) c(try$control, try$treated)) %>%
  #   purrr::map(~ trimmed_df[.x, ]) %>%
  #   purrr::map(~ RItools::xBalance(f, data = .x, report = "z")) %>%
  #   purrr::map_dbl(purrr::pluck, "results", 1) %>%
  #   which.min()
  #
  # # Figure out matching status
  # try <- best_tries[[best_match]]
  # matched_df <- trimmed_df[seq_len(nrow(trimmed_df)) %in%
  #                            c(try$control, try$treated), ] %>%
  #   mutate(Matching = "matched")
  #
  # unmatchable_df <- matching_df %>%
  #   anti_join(trimmed_df, by = ".rowid") %>%
  #   mutate(Matching = "unmatchable")
  #
  # unmatched_df <- trimmed_df %>%
  #   anti_join(matched_df, by = ".rowid") %>%
  #   mutate(Matching = "unmatched")
  #
  # df_results <- bind_rows(matched_df, unmatchable_df, unmatched_df) %>%
  #   select(.rowid, Matching)
  #
  # # Include information about matching process
  # left_join(data, df_results, by = ".rowid") %>%
  #   mutate(Matching_Var = rlang::quo_name(x),
  #          Matching_Outcome = rlang::quo_name(y),
  #          Matching_Caliper = caliper_val,
  #          Matching_Floor = lower_edge,
  #          Matching_Ceiling = upper_edge,
  #          Matching = ifelse(is.na(Matching), "missing-data", Matching)) %>%
  #   select(-.rowid)
}

find_caliper_pairs <- function(data, group, x, caliper = 0) {
  x <- enquo(x)
  group <- enquo(group)

  # Need to be able to uniquely identify rows
  if (!rlang::has_name(data, ".rowid")) {
    data <- tibble::rowid_to_column(data, ".rowid")
  }

  # Create a dummy ".join" column so that every row can be matched to any
  # other row. Then split the groups.
  each_group <- data %>%
    dplyr::mutate(.join = 1) %>%
    split(dplyr::pull(data, !! group))

  # Uniquely name the columns in each group
  each_group[[1]] <- each_group[[1]] %>%
    dplyr::rename(
      .group1 = !! group,
      .rowid1 = .data$.rowid,
      .x1 = !! x)

  each_group[[2]] <- each_group[[2]] %>%
    dplyr::rename(
      .group2 = !! group,
      .rowid2 = .data$.rowid,
      .x2 = !! x)

  # Generate all possible matches then keep only those that satisfy the
  # caliper constraint
  dplyr::full_join(each_group[[1]], each_group[[2]], by = ".join") %>%
    dplyr::mutate(diff = .data$.x1 - .data$.x2) %>%
    dplyr::filter(abs(diff) <= caliper) %>%
    dplyr::select(.data$.rowid1, .data$.rowid2, .data$diff)
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
