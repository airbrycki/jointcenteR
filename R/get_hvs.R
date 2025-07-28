#' Get Housing Vacancy Survey data
#'
#' Pulls and organizes Housing Vacancy Survey data using tidyquant
#'
#' @param fred_api_key Your unique FRED API key.
#'
#' @return A tibble of HVS variables for each quarter from 2Q2001 through most
#'         recent available.
#'
#' @note Adapted from Len Kiefer:
#'       http://lenkiefer.com/2017/09/18/a-tidyquant-um-of-solace/ and uses
#'       fredr instead of tidyquant. Set your unique FRED API key here:
#'       https://fredaccount.stlouisfed.org/apikeys and add it to your R global
#'       environment using Sys.setenv(FRED_API_KEY = '{put your key here}').
#'
#' @examples
#' hvs <- get_hvs()
#'
#' @export
get_hvs <- function(fred_api_key = Sys.getenv("FRED_API_KEY")) {

  # check if FRED API key is available
  if (is.null(fred_api_key) || fred_api_key == "") {
    stop("FRED API key is missing. Pass it as an argument or set it using Sys.setenv(FRED_API_KEY = 'your_key').")
  }

  # set FRED API key for the session
  fredr::fredr_set_key(fred_api_key)

  tickers <- c(
    'ETOTALUSQ176N',    # All housing units
    'EVACANTUSQ176N',   # Vacant
    'EYRVACUSQ176N',    # Year-round vacant
    'ERENTUSQ176N',     # Vacant for rent
    'ESALEUSQ176N',     # Vacant for sale
    'ERNTSLDUSQ176N',   # Vacant rented or sold
    'EOFFMARUSQ176N',   # Vacant held off market
    'EOCCUSEUSQ176N',   # Vacant occasional use
    'EUREUSQ176N',      # Vacant usual residence elsewhere
    'EOTHUSQ176N',      # Vacant other
    'ESEASONUSQ176N',   # Vacant seasonal
    'EOCCUSQ176N',      # Occupied
    'EOWNOCCUSQ176N',   # Owner occupied
    'ERNTOCCUSQ176N',   # Renter occupied
    'RRVRUSQ156N',      # Rental vacancy rate
    'RHVRUSQ156N'       # Homeowner vacancy rate
  )

  variable_labels <- c(
    'all_housing_units',
    'vacant',
    'year_round_vacant',
    'vacant_for_rent',
    'vacant_for_sale',
    'vacant_rented_or_sold',
    'vacant_held_off_market',
    'vacant_occasional_use',
    'vacant_usual_residence_elsewhere',
    'vacant_other',
    'vacant_seasonal',
    'occupied',
    'owner_occupied',
    'renter_occupied',
    'rental_vacancy_rate',
    'homeowner_vacancy_rate'
  )

  # Create a lookup dataset
  variable_table <- data.frame(symbol = tickers, var = variable_labels, stringsAsFactors = FALSE)

  # pull data using fredr
  all_data <- lapply(tickers, function(ticker) {
    fredr::fredr(
      series_id = ticker,
      observation_start = as.Date("2001-04-01")
    ) |>
      dplyr::select(date, value) |>
      dplyr::mutate(symbol = ticker)
  }) |>
    dplyr::bind_rows()

  df <- all_data |>
    dplyr::left_join(variable_table, by = "symbol") |>
    dplyr::arrange(var, date) |>
    dplyr::select(date, var, value) |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      value_4q_avg = dplyr::if_else(
        date < as.Date("2002-01-01"),
        NA_real_,
        pracma::movavg(value, 4, type = "s")
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = var,
      values_from = c("value", "value_4q_avg"),
      names_sep = "_"
    ) |>
    dplyr::mutate(
      value_homeownership_rate =
        value_owner_occupied / value_occupied * 100,
      value_4q_avg_homeownership_rate =
        value_4q_avg_owner_occupied / value_4q_avg_occupied * 100
    )

  return(df)
}
