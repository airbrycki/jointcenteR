#' Two-variable medians
#'
#' Calculate weighted and weighted tables using one variable
#'
#' @param df A dataframe containing the variable of interest and weight vector
#'
#' @param var A categorical variable to group by.
#'
#' @param var2 A continuous variable to tabulate median.
#'
#' @param weight A numeric weight variable. If no weight is provided, the tabulation is unweighted.
#'
#' @return A tibble of weighted or unweighted sums and shares.
#'
#' @note The first variable entered is the grouping variable.
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
wgtd_med2 <- function(df, var, var2, weight = "unwgtd") {
  # Check if df is a valid dataframe
  if (!inherits(df, "data.frame")) {
    stop("The provided 'df' is not a valid data frame.")
  }
  
  # Convert 'var' to a symbol
  var <- ensym(var)
  var2 <- ensym(var2)
  weight <- ensym(weight)
  
  # Check if 'var' is in the dataframe columns
  if (!as.character(var) %in% names(df)) {
    stop("The specified column '", as.character(var), "' is not found in the dataframe.")
  }
  if (!as.character(var2) %in% names(df)) {
    stop("The specified column '", as.character(var2), "' is not found in the dataframe.")
  }
  
  # If 'weight' is unwgtd, calculate unweighted median
  if (weight == "unwgtd") {
    unwgtd <- df |>
      filter(!is.na(!!var) & !is.na(!!var2)) |>
      group_by(!!var) |>
      summarise(!!paste0("median_", var2) :=
                  Hmisc::wtd.quantile(!!var2, probs=0.5)) |>
      ungroup()
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
      filter(!is.na(!!var) & !is.na(!!var2)) |>
      group_by(!!var) |>
      summarise(!!paste0("median_", var2) :=
                  Hmisc::wtd.quantile(!!var2, probs=0.5, !!weight)) |>
      ungroup()
    
    return(wgtd)
  }
}