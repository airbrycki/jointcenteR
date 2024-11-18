#' Single-variable medians
#'
#' Calculate weighted and weighted medians using one variable
#'
#' @param df A dataframe containing the variable of interest and weight vector
#'
#' @param var A continuous variable to tabulate.
#'
#' @param weight A numeric weight variable. If no weight is provided, the tabulation is unweighted.
#'
#' @return A tibble of weighted or unweighted medians.
#' 
#' @examples
#' data("acs22")
#' # calculate the weighted median household income
#' wgtd_med(acs22, hincp, wgtp) 
#' 
#' #returns unweighted median if no weight if provided
#' wgtd_med(acs22, hincp)
#'
#' @export
wgtd_med <- function(df, var, weight = "unwgtd") {
  # Check if df is a valid dataframe
  if (!inherits(df, "data.frame")) {
    stop("The provided 'df' is not a valid data frame.")
  }
  
  # Convert 'var' to a symbol
  var <- ensym(var)
  weight <- ensym(weight)
  
  # Check if 'var' is in the dataframe columns
  if (!as.character(var) %in% names(df)) {
    stop("The specified column '", as.character(var), "' is not found in the dataframe.")
  }
  
  # If 'weight' is unwgtd, calculate unweighted median
  if (weight == "unwgtd") {
    unwgtd <- df |>
      dplyr::filter(!is.na(!!var)) |>
      dplyr::summarise(!!paste0("median_", var) :=
                  Hmisc::wtd.quantile(!!var, probs=0.5))
    return(unwgtd)
  }
  
  # If 'weight' is provided as a string (e.g., "wgtp"), convert to a symbol
  if (weight!="unwgtd") {
    
    # Check if the 'weight' column exists in the dataframe
    if (!as.character(weight) %in% names(df)) {
      stop("The specified column '", as.character(weight), "' is not found in the dataframe.")
    }
    
    # Calculate totals using the 'weight' column
    wgtd <- df |>
      dplyr::filter(!is.na(!!var)) |>
      dplyr::summarise(!!paste0("median_", var) :=
                  Hmisc::wtd.quantile(!!var, probs=0.5, !!weight))
    
    return(wgtd)
  }
}