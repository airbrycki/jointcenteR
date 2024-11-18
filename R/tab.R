#' One-way tables
#'
#' Calculate weighted and weighted tables using one variable
#'
#' @param df A dataframe containing the variable of interest and weight vector
#'
#' @param var A categorical variable to tabulate.
#'
#' @param weight A numeric weight variable. If no weight is provided, the tabulation is unweighted.
#'
#' @return A tibble of weighted or unweighted sums and shares.
#' 
#' @examples
#' data("acs22")
#' # calculate the number and share of households by tenure using household weights
#' tab(acs22, ten, wgtp) 
#' 
#' #unweighted observations
#' tab(acs22, ten)
#'
#' @export
tab <- function(df, var, weight = unwgtd) {
  # Check if df is a valid dataframe
  if (!inherits(df, "data.frame")) {
    stop("The provided 'df' is not a valid data frame.")
  }
  
  # Convert 'var' to a symbol
  var <- rlang::ensym(var)
  weight <- rlang::ensym(weight)
  
  # Check if 'var' is in the dataframe columns
  if (!as.character(var) %in% names(df)) {
    stop("The specified column '", as.character(var), "' is not found in the dataframe.")
  }
  
  # If 'weight' is NULL, calculate totals based on counts (n)
  if (weight == "unwgtd") {
    unwgtd <- df |>
      dplyr::filter(!is.na(!!var)) |>
      dplyr::group_by(!!var) |>
      dplyr::summarise(tot = n()) |>
      dplyr::ungroup() |>
      dplyr::mutate(sh = tot / sum(tot) * 100)
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
      dplyr::group_by(!!var) |>
      dplyr::summarise(tot = sum(!!weight, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(sh = tot / sum(tot) * 100)
    
    return(wgtd)
  }
}
