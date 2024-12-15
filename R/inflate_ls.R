#' Inflate with CPI-U Less Shelter
#'
#' Inflate rents from reference to target dollars using CPI-U Less Shelter, NSA.
#'
#' @param initial_rent A numeric value or vector to be inflated.
#'
#' @param reference_year The year the values to be inflated are from.
#'
#' @param target_year The year values should be inflated to.
#'
#' @return A numeric vector of inflated values.
#'
#' @note CPI-U inflation rates are pulled from FRED using tidyquant. Monthly values are averaged for the year.
#'
#' @examples
#' #inflate rent from 2000 dollars to 2023 dollars
#' inflate_ls(500, 2000, 2023)
#' 
#' data("acs22")
#' # inflate contract rents to target year dollars in new variable
#' acs22 |> mutate(rntp_infl_23 = inflate_ls(rntp, 2022, 2023))
#' 
#' @export
inflate_ls <- function(initial_rent, reference_year, target_year) {
  
  # Input Validation
  if (!is.numeric(initial_rent) || any(initial_rent <= 0, na.rm = TRUE)) {
    stop("Rent must be a positive numeric value.")
  }
  
  # Fetch CPI data for CUUR0000SA0L2 from FRED
  cpi_data <- tidyquant::tq_get("CUUR0000SA0L2", from = "1900-01-01", to = Sys.Date(), get = "economic.data")
  
  # Extract the year and calculate the annual average CPI for the reference and target years
  cpi_data <- cpi_data |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::filter(year %in% c(reference_year, target_year)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(annual_avg_cpi = mean(price, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  # Ensure that the requested years are in the available data
  if (nrow(cpi_data) < 2 & reference_year != target_year) {
    stop("The specified years are out of date bounds.")
  }
  
  # Extract CPI values for the reference and target years
  cpi_ref <- cpi_data |> 
    dplyr::filter(year == reference_year) |> 
    dplyr::pull(annual_avg_cpi)
  cpi_target <- cpi_data |> 
    dplyr::filter(year == target_year) |> 
    dplyr::pull(annual_avg_cpi)
  
  # Inflation adjustment formula
  adjusted_rent <- round(initial_rent * (cpi_target / cpi_ref), 2)
  
  return(adjusted_rent)
}
