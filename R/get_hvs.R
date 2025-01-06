#' Get Housing Vacancy Survey data
#'
#' Pulls and organizes Housing Vacancy Survey data using tidyquant
#'
#' @return A tibble of HVS variables for each quarter from 2Q2001 through most
#'         recent available.
#'
#' @note Adapted from Len Kiefer:
#'       http://lenkiefer.com/2017/09/18/a-tidyquant-um-of-solace/
#'
#' @examples
#' hvs <- get_hvs()
#'
#' @export
get_hvs <- function() {

  tickers <- c('ETOTALUSQ176N',    # All housing units
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

  variable_labels <- c('all_housing_units',
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
  variable_table<-data.frame(symbol=tickers,var=variable_labels)

  # pull data using tidyquant
  df <- tidyquant::tq_get(tickers, get = "economic.data", from = "2001-04-01") |>
          dplyr::rename(value = price) |>
          dplyr::left_join(variable_table, by = "symbol") |>
          dplyr::arrange(var, date) |>
          dplyr::select(date, var, value) |>
          dplyr::mutate(value_4q_avg = dplyr::if_else(date < 2002-01-01,
                                               NA_integer_,
                                               pracma::movavg(value, 4))) |>
          tidyr::pivot_wider(names_from = var, values_from = c("value", "value_4q_avg")) |>
          dplyr::mutate(value_homeownership_rate =
                            value_owner_occupied / value_occupied * 100,
                        value_4q_avg_homeownership_rate =
                            value_4q_avg_owner_occupied / value_4q_avg_occupied * 100)

  return(df)

}
