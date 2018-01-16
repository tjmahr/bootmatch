
#' Match two groups on a single variable using bootstrapping
#'
#' @param data a dataframe
#' @param y a column in `data` with two values or a logical expression that can
#' be evaluated inside of `data` to determine groups.
#' @param x a column in `data`
#' @param caliper a caliper constraint to set on the matching. Only matches with
#'   `abs(x[1] - x[2]) <= caliper` are legal. A caliper of 0 would enforce exact
#'   matches.
#' @param boot number of bootstrap samples to take. Defaults to 100.
#' @param id an optional column in `data` with a unique ID for each row in
#' `data`. Matches will be provided using these IDs instead of row numbers.
#' @return the best bootstrapped match. The best match has the largest number of
#'   matched pairs and the smallest _z_-score difference on `x` between the two
#'   matched subgroups.
#' @export
boot_match_univariate <- function(data, y, x, caliper = 0, boot = 100, id = NULL) {
  y <- enquo(y)
  x <- enquo(x)
  id <- enquo(id)
  x_name <- quo_name(x)
  x_missing <- is.na(eval_tidy(x, data))

  if (validate_grouping(data, !! y)) {
    y_vals <- eval_tidy(y, data)
    # If y is a character vector,
    # treat it as a factor for the purposes of numbering groups
    if (is.character(y_vals)) {
      y <- quo(as.factor(!! y))
    }
  }

  data <- tibble::rowid_to_column(data, ".rowid")

  df_matching <- data %>%
    dplyr::filter(!is.na(!! x)) %>%
    dplyr::mutate(.group = as.numeric(!! y)) %>%
    dplyr::select(.data$.rowid, .data$.group, !! x)

  # Keep only participants within +- caliper values of each other
  df_template <- find_caliper_pairs(df_matching, .data$.group, !! x, caliper)

  matchable_rows <- unique(c(df_template$.rowid1, df_template$.rowid2))
  df_unmatchable <- df_matching[!c(df_matching$.rowid %in% matchable_rows), ]

  # Do a bunch of matches
  tries <- purrr::rerun(boot, bootstrap_match(df_template))

  # Keep biggest N's
  best_n <- max(purrr::map_int(tries, nrow))
  best_tries <- tries %>%
    purrr::keep(~ nrow(.x) == best_n)

  # The balance function later on wants a formula
  f <- rlang::new_formula(quote(.group), rlang::get_expr(x))

  # Keep smallest z's
  z_scores <- best_tries %>%
    purrr::map(function(try) c(try$.rowid1, try$.rowid2)) %>%
    purrr::map(~ df_matching[c(df_matching$.rowid %in% .x), ]) %>%
    purrr::map(~ RItools::xBalance(f, data = .x, report = "z")) %>%
    purrr::map_dbl(purrr::pluck, "results", 1)

  best_match <- which.min(abs(z_scores))
  message("Z-score difference for ", best_n,
          ngettext(best_n, " pair: ", " pairs: "),
          round(z_scores[best_match], 3))

  # Figure out matching status
  df_pairs <- best_tries[[best_match]]

  data[c(df_pairs$.rowid1, df_pairs$.rowid2), "Matching"] <- "matched"
  data[df_unmatchable$.rowid, "Matching"] <- "unmatchable"
  data[x_missing, "Matching"] <- "missing-data"
  data[is.na(data$Matching), "Matching"] <- "unmatched"

  # Attach ID's of matches, defaulting to row numbers if necessary
  if (rlang::quo_is_null(id)) id <- sym(".rowid")
  id_vals <- dplyr::pull(data, !! id)
  stopifnot(length(id_vals) == length(unique(id_vals)))

  df_pairs$.match_id1 <- id_vals[df_pairs$.rowid1]
  df_pairs$.match_id2 <- id_vals[df_pairs$.rowid2]

  d1 <- df_pairs %>%
    dplyr::select(
      .rowid = .data$.rowid1,
      !! quo_name(id) := .data$.match_id1,
      Matching_MatchID = .data$.match_id2)

  d2 <- df_pairs %>%
    dplyr::select(
      .rowid = .data$.rowid2,
      !! quo_name(id) := .data$.match_id2,
      Matching_MatchID = .data$.match_id1)

  join_by <- unique(c(".rowid", quo_name(id)))
  data %>%
    dplyr::left_join(dplyr::bind_rows(d1, d2), by = join_by) %>%
    dplyr::select(-.data$.rowid) %>%
    dplyr::select(dplyr::everything(),
                  dplyr::one_of(c("Matching", "Matching_MatchID")))
}

bootstrap_match <- function(matchable) {
  done <- dplyr::slice(matchable, 0L)
  while (nrow(matchable) > 0L) {
    next_match <- dplyr::sample_n(matchable, 1L)
    matchable <- matchable[matchable$.rowid1 != next_match$.rowid1 &
                             matchable$.rowid2 != next_match$.rowid2, ]
    done <- dplyr::bind_rows(done, next_match)
  }
  done
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
