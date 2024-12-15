#' Grouped medians
#'
#' Calculate weighted and weighted medians with up to one grouping variable
#'
#' @param df A dataframe containing the variable(s) of interest and weight vector
#'
#' @param var A continuous variable to tabulate median.
#'
#' @param var2 A categorical variable to group by.
#'
#' @param weight A numeric weight variable. If no weight is provided, 
#'               the tabulation is unweighted.
#'
#' @return A tibble of weighted or unweighted medians.
#'
#' @note The second variable entered is the grouping variable. Does not
#'       currently work in combination with the mutate or summarise function. 
#'       To calculate medians when this doesn't work, use 
#'       Hmisc::wtd.quantile(var, probs=0.5).
#' 
#' @examples
#' data("acs22")
#' # calculate the weighted median household income by tenure
#' wgtd_med2(acs22, ten, hincp, wgtp) 
#' 
#' #returns unweighted medians if no weight if provided
#' wgtd_med2(acs22, ten, hincp)
#'
#' @export
wtd_med <- function(df, var, var2 = NULL, weight = NULL) {
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
  weight <- rlang::enquo(weight)
  if (rlang::quo_is_null(weight)) {
    weight <- NULL
  } else {
    weight <- rlang::ensym(weight)
  }
  
  # If 'weight' is not provided run unweighted
  if (is.null(weight)) {
    if (is.null(var2)) {
      unwgtd <- df |>
        dplyr::filter(!is.na(!!var)) |>
        dplyr::summarise(!!paste0("median_", var) :=
                           Hmisc::wtd.quantile(!!var, probs=0.5))
      return(unwgtd)
    }
    else{
      unwgtd_2var <- df |>
        dplyr::filter(!is.na(!!var) & !is.na(!!var2)) |>
        dplyr::group_by(!!var2) |>
        dplyr::summarise(!!paste0("median_", var) :=
                           Hmisc::wtd.quantile(!!var, probs=0.5)) |>
        dplyr::ungroup()
      return(unwgtd_2var)
    }
  }
  
  #if weight is provided, run as weighted median
  if (!is.null(weight)) {
    
    # Check if the 'weight' column exists in the dataframe
    if (!as.character(weight) %in% names(df)) {
      stop(as.character(weight), "' is not found in the dataframe.")
    }
    
    if (is.null(var2)) {
      unwgtd <- df |>
        dplyr::filter(!is.na(!!var)) |>
        dplyr::summarise(!!paste0("median_", var) :=
                           Hmisc::wtd.quantile(!!var, probs=0.5, !!weight))
      return(unwgtd)
    }
    else{
      unwgtd_2var <- df |>
        dplyr::filter(!is.na(!!var) & !is.na(!!var2)) |>
        dplyr::group_by(!!var2) |>
        dplyr::summarise(!!paste0("median_", var) :=
                           Hmisc::wtd.quantile(!!var, probs=0.5, !!weight)) |>
        dplyr::ungroup()
      return(unwgtd_2var)
    }
  }
}
