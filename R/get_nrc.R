#' Get New Residential Construction data
#'
#' Pulls and organizes New Residential Construction data using tidyquant
#' 
#' @param type Specifies the interval of interest, either "monthly" or "annual"
#'
#' @return A tibble of HVS variables for each quarter from 2Q2001 through most
#'         recent available.
#'
#' @note Adapted from Len Kiefer: 
#'       http://lenkiefer.com/2017/09/18/a-tidyquant-um-of-solace/
#'       Annual counts are totals of monthly NSA values, except annual under
#'       construction counts are the value in December of a given year. If 
#'       current year is incomplete, it is omitted from annual table. Monthly
#'       counts are seasonally adjusted annual rates, except under construction
#'       counts are the seasonally adjusted rate (not annualized).
#'
#' @examples
#' # pull annual data
#' nrc_a <- get_nrc(type = "annual") |> 
#'   # keep vars for year and units in buildings with 2+ units
#'   select(year, contains('_2pl')) |> 
#'   # keep years in range of interest
#'   filter(year >= 2000)
#' 
#' # pull monthly data to calculate ytd SA average
#' nrc_m <- get_nrc("monthly") |> 
#'   # reshape to make calculations easier
#'   pivot_longer(!c("year", "month"), names_to = "var", values_to = "value") |>
#'   filter(str_detect(var, '_2pl') & year == 2024) |> # keep current year & mf vars
#'   group_by(year, var) |>   
#'   mutate(avg = mean(value)) |>   
#'   filter(month == 1) |>   
#'   select(-month, -value) |>   
#'   pivot_wider(names_from = var, values_from = avg)
#' 
#' #combine annual and ytd dfs  
#' nrc <- rbind(nrc_a, nrc_m)
#' 
#' @export
get_nrc <- function(type = "monthly") {
  
  tickers <- c(
    'PERMIT',       # Permitted - total units, monthly SAAR
    'PERMIT1',      # Permitted - single family, monthly SAAR
    'PERMIT5',      # Permitted - multifam 5+, monthly SAAR
    
    'HOUST',        # Started - total units, monthly SAAR
    'HOUST1F',      # Started - single family, monthly SAAR
    'HOUST5F',      # Started - multifam 5+, monthly SAAR
    
    'UNDCONTSA',    # Under construction - total units, monthly SA
    'UNDCON1USA',   # Under construction - single family, monthly SA
    'UNDCON5MUSA',  # Under construction - multifam 5+, monthly SA
    
    'COMPUTSA',     # Completed - total units, monthly SAAR
    'COMPU1USA',    # Completed - single family, monthly SAAR
    'COMPU5MUSA',   # Completed - multifam 5+, monthly SAAR
    
    'PERMITNSA',    # Permitted - total units, NSA
    'PERMIT1NSA',   # Permitted - single family, monthly NSA
    'PERMIT5NSA',   # Permitted - multifam 5+, monthly NSA
    
    'HOUSTNSA',     # Started - total units, NSA
    'HOUST1FNSA',   # Started - single family, monthly NSA
    'HOUST5FNSA',   # Started - multifam 5+, monthly NSA
    
    'UNDCONTNSA',   # Under construction - total units, monthly NSA
    'UNDCON1UNSA',  # Under construction - single family, monthly NSA
    'UNDCON5MUNSA', # Under construction - multifam 5+, monthly NSA
    
    'COMPUTNSA',    # Completed - total units, NSA
    'COMPU1UNSA',   # Completed - single family, monthly NSA
    'COMPU5MUNSA'   # Completed - multifam 5+, monthly NSA
  )

  variable_labels <- c('permit_saar',
                       'permit_1_saar',
                       'permit_5pl_saar',
                       'start_saar',
                       'start_1_saar',
                       'start_5pl_saar',
                       'underconst_sa',
                       'underconst_1_sa',
                       'underconst_5pl_sa',
                       'compl_saar',
                       'compl_1_saar',
                       'compl_5pl_saar',
                       'permit_nsa',
                       'permit_1_nsa',
                       'permit_5pl_nsa',
                       'start_nsa',
                       'start_1_nsa',
                       'start_5pl_nsa',
                       'underconst_nsa',
                       'underconst_1_nsa',
                       'underconst_5pl_nsa',
                       'compl_nsa',
                       'compl_1_nsa',
                       'compl_5pl_nsa'
  )
  
  # Create a lookup dataset
  variable_table<-data.frame(symbol=tickers,var=variable_labels)
  
  # pull data using tidyquant
  df <- tidyquant::tq_get(tickers, get = "economic.data", from = "1970-01-01") |> 
    dplyr::rename(value = price) |> 
    dplyr::left_join(variable_table, by = "symbol") |> 
    dplyr::mutate(month = lubridate::month(date),
                  year = lubridate::year(date)) |> 
    dplyr::select(-symbol, -date, year, month, var, value)
  
  # if type is annual data, select nsa vars and sum by year
  if (type == "annual") {
    annual <- df |> 
      dplyr::filter(str_detect(var, '_nsa') & !str_detect(var, 'underconst')) |> 
      dplyr::group_by(year, var) |> 
      dplyr::summarise(tot = sum(value, na.rm=TRUE)) |> 
      tidyr::pivot_wider(names_from = var, values_from = tot) |> 
      dplyr::mutate(permit_2pl = permit_nsa - permit_1_nsa,
                    start_2pl = start_nsa - start_1_nsa,
                    compl_2pl = compl_nsa - compl_1_nsa) |> 
      ungroup()
    
    names(annual) <- sub("_nsa", "", names(annual)) 
    
    annual_underco <- df |> 
      dplyr::filter(str_detect(var, '_nsa') & str_detect(var, 'underconst')) |>
      dplyr::filter(month == 12) |> 
      tidyr::pivot_wider(names_from = var, values_from = value) |> 
      dplyr::mutate(underconst_2pl = underconst_nsa - underconst_1_nsa) |> 
      dplyr::select(-month) |> 
      dplyr::ungroup()
    
    names(annual_underco) <- sub("_nsa", "", names(annual_underco))
    
    annual_full <- annual |> 
      left_join(annual_underco, by = "year")
    
    lastyear <- max(annual_full$year)
    lastmonth <- max(df$month[df$year == lastyear])
    
    if (lastmonth < 12) {
      annual_full <- annual_full |> 
        filter(year != lastyear)
    }
    
    annual_full <- annual_full |>
      dplyr::select(year, order(colnames(annual_full))) |> 
      dplyr::select(year, starts_with("permit"), starts_with("start"), 
                    starts_with("under"), starts_with("compl"))

    
    return(annual_full)
  }
  else {
    monthly <- df |> 
      dplyr::filter(!str_detect(var, '_nsa')) |> 
      tidyr::pivot_wider(names_from = var, values_from = value) |> 
      dplyr::mutate(permit_2pl = permit_saar - permit_1_saar,
                    start_2pl = start_saar - start_1_saar,
                    underconst_2pl = underconst_sa - underconst_1_sa,
                    compl_2pl = compl_saar - compl_1_saar)  |> 
      dplyr::ungroup()
    
    names(monthly) <- sub("_saar|_sa", "", names(monthly))
    
    monthly <- monthly |>
      dplyr::select(year, month, order(colnames(monthly))) |> 
      dplyr::select(year, month, starts_with("permit"), starts_with("start"), 
                    starts_with("under"), starts_with("compl"))
    
    return(monthly)
  }
}