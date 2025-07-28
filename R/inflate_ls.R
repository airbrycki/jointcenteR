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
#' @param fred_api_key Your unique FRED API key.
#'
#' @return A numeric vector of inflated values.
#'
#' @note CPI-U inflation rates are pulled from FRED using fredr.
#' Monthly values are averaged for the year. Set your unique FRED API key here:
#' https://fredaccount.stlouisfed.org/apikeys and add it to your R global
#' environment using Sys.setenv(FRED_API_KEY = '{put your key here}').
#'
#' @examples
#' # inflate rent from 2000 dollars to 2023 dollars
#' inflate_ls(500, 2000, 2023)
#'
#' data("acs22")
#' # inflate contract rents to target year dollars in new variable
#' acs22 |> mutate(rntp_infl_23 = inflate_ls(rntp, unique(year), 2023))
#'
#' @export
inflate_ls <- function(initial_rent, reference_year, target_year, fred_api_key = Sys.getenv("FRED_API_KEY")) {

  # validate amount
  if (!is.numeric(initial_rent) || any(initial_rent <= 0, na.rm = TRUE)) {
    stop("Rent must be a positive numeric value.")
  }

  # check if FRED API key is available
  if (is.null(fred_api_key) || fred_api_key == "") {
    stop("FRED API key is missing. Pass it as an argument or set it using Sys.setenv(FRED_API_KEY = 'your_key').")
  }

  # set FRED API key for the session
  fredr::fredr_set_key(fred_api_key)

  # get CPI data for CUUR0000SA0L2 from FRED
  cpi_data <- fredr::fredr(
    series_id = "CUUR0000SA0L2",
    observation_start = as.Date(paste0(min(reference_year, target_year), "-01-01")),
    observation_end   = as.Date(paste0(max(reference_year, target_year), "-12-31"))
  )

  # calculate the annual average CPI for the reference and target years
  cpi_data <- cpi_data |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::filter(year %in% c(reference_year, target_year)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(annual_avg_cpi = mean(value, na.rm = TRUE), .groups = "drop")

  # throw error if dates are out of bounds of available data
  if (nrow(cpi_data) < 2 & reference_year != target_year) {
    stop("The specified years are out of date bounds.")
  }

  # get CPI values for the reference and target years
  cpi_ref <- cpi_data |>
    dplyr::filter(year == reference_year) |>
    dplyr::pull(annual_avg_cpi)
  cpi_target <- cpi_data |>
    dplyr::filter(year == target_year) |>
    dplyr::pull(annual_avg_cpi)

  # adjust for inflation
  adjusted_rent <- round(initial_rent * (cpi_target / cpi_ref), 2)

  return(adjusted_rent)
}
