#' Count and share tables
#'
#' Calculate weighted and weighted tables using up to three variables
#'
#' @param df A dataframe containing the variable of interest and weight vector
#'
#' @param var,var2,var3 A categorical variable to tabulate.
#'
#' @param weight A numeric weight variable. If no weight is provided, the
#' tabulation is unweighted.
#'
#' @return A tibble of weighted or unweighted sums and shares.
#'
#' @note The first two variables entered are the grouping variables for
#' calculating shares. NA values are omitted from the tabulation.
#'
#' @examples
#' data("acs22")
#' # calculate the number and share of households by region, tenure, and
#' disability using household weights
#' tab(acs22, region, ten, dis, wgtp)
#'
#' # unweighted observations
#' tab(acs22, region, ten, dis)
#'
#' @export
tab <- function(df, var, var2 = NULL, var3 = NULL, weight = NULL) {
  # Check if df is a valid dataframe
  if (!inherits(df, "data.frame")) {
    stop("The provided 'df' is not a valid data frame.")
  }

  # Convert 'var' to a symbol
  var <- rlang::ensym(var)
  var2 <- rlang::enquo(var2)
  if (rlang::quo_is_null(var2)) {
    var2 <- NULL
  } else {
    var2 <- rlang::ensym(var2)
  }
  var3 <- rlang::enquo(var3)
  if (rlang::quo_is_null(var3)) {
    var3 <- NULL
  } else {
    var3 <- rlang::ensym(var3)
  }
  weight <- rlang::enquo(weight)
  if (rlang::quo_is_null(weight)) {
    weight <- NULL
  } else {
    weight <- rlang::ensym(weight)
  }

  # If 'weight' is not provided run unweighted
  if (is.null(weight)) {
    if (is.null(var2) & is.null(var3)) {
      unwgtd_1way <- df |>
        dplyr::filter(!is.na(!!var)) |>
        dplyr::group_by(!!var) |>
        dplyr::summarise(tot = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(sh = tot / sum(tot) * 100)
      return(unwgtd_1way)
    }
    else if (!is.null(var2) & is.null(var3) | is.null(var2) & !is.null(var3)) {
      unwgtd_2way <- df |>
        dplyr::filter(!is.na(!!var) & !is.na(!!var2)) |>
        dplyr::group_by(!!var, !!var2) |>
        dplyr::summarise(tot = dplyr::n()) |>
        dplyr::group_by(!!var) |>
        dplyr::mutate(sh = tot / sum(tot) * 100)
      return(unwgtd_2way)
    }
    else {
      unwgtd_3way <- df |>
        dplyr::filter(!is.na(!!var) & !is.na(!!var2) & !is.na(!!var3)) |>
        dplyr::group_by(!!var, !!var2, !!var3) |>
        dplyr::summarise(tot =dplyr::n()) |>
        dplyr::group_by(!!var, !!var2) |>
        dplyr::mutate(sh = tot / sum(tot) * 100) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = !!var3, values_from = c(tot, sh))
      return(unwgtd_3way)
    }
  }

  if (!is.null(weight)) {
    weight <- rlang::ensym(weight)

    # Check if the 'weight' column exists in the dataframe
    if (!as.character(weight) %in% names(df)) {
      stop(as.character(weight), "' is not found in the dataframe.")
    }

    if (is.null(var2) & is.null(var3)) {
      wgtd_1way <- df |>
        dplyr::filter(!is.na(!!var)) |>
        dplyr::group_by(!!var) |>
        dplyr::summarise(tot = sum(!!weight, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate(sh = tot / sum(tot) * 100)
      return(wgtd_1way)
    }
    else if (!is.null(var2) & is.null(var3) | is.null(var2) & !is.null(var3)) {
      wgtd_2way <- df |>
        dplyr::filter(!is.na(!!var) & !is.na(!!var2)) |>
        dplyr::group_by(!!var, !!var2) |>
        dplyr::summarise(tot = sum(!!weight, na.rm = TRUE)) |>
        dplyr::group_by(!!var) |>
        dplyr::mutate(sh = tot / sum(tot) * 100)
      return(wgtd_2way)
    }
    else {
      wgtd_3way <- df |>
        dplyr::filter(!is.na(!!var) & !is.na(!!var2) & !is.na(!!var3)) |>
        dplyr::group_by(!!var, !!var2, !!var3) |>
        dplyr::summarise(tot = sum(!!weight, na.rm = TRUE)) |>
        dplyr::group_by(!!var, !!var2) |>
        dplyr::mutate(sh = tot / sum(tot) * 100) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = !!var3, values_from = c(tot, sh))
      return(wgtd_3way)
    }
  }
}
