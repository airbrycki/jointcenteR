#' Clean AHAR data
#'
#' Puts HUD AHAR data into a usable table format with sheltered, unsheltered,
#' and overall homelessness counts by state and for the US
#' 
#' @param year Year of data to pull, from 2007 to 2023.
#' 
#' @param file File path and name of saved .xlsx point-in-time count file.
#'
#' @return A dataframe of sheltered, unsheltered, and overall homelessness counts
#'         by state and for the US for years selected.
#'
#' @note HUD posts the data as an xlsb, which is very hard to work with. First
#'       download the data from 
#'       https://www.huduser.gov/portal/sites/default/files/xls/2007-2023-PIT-Counts-by-CoC.xlsb
#'       and save locally as an xlsx. Use 2021 data with caution: unsheltered
#'       and overall counts were not complete.
#'
#' @examples
#' file <- "C:/Data/2007-2023-PIT-Counts-by-State.xlsx"
#' # single year
#' process_ahar(2023, file)
#'
#' # multiple years
#' yrs <- 2007:2023
#' ahar_st <- map(yrs, ahar_table, file = file) |> 
#'   reduce(left_join, by = "State")
#' 
#' @export
process_ahar <- function(year, file) {
  ahar <- openxlsx::read.xlsx(file, sheet=paste0("", year, "")) 

  ahar <- ahar |> 
    dplyr::select("State", "Overall.Homeless", "Sheltered.Total.Homeless", "Unsheltered.Homeless") |> 
    dplyr::rename_with(~gsub(',.[[:digit:]]+', "", .)) |> 
    dplyr::rename(!!paste0("overall_", year) := Overall.Homeless,
           !!paste0("sheltered_", year) := Sheltered.Total.Homeless,
           !!paste0("unsheltered_", year) := Unsheltered.Homeless) |> 
    dplyr::filter(State != "AS" & State!= "GU" & State!= "PR" & State!="VI" & State!="MP")

  aharyr <- paste0("ahar", year)

  assign(aharyr, ahar)
  
  ahar <- ahar %>% 
    slice(52, 1:51) %>% 
    mutate(State = if_else(State=="Total", "US", State),
           StateName = case_when(State == "DC" ~ "District of Columbia", 
                                 State == "US" ~ "United States",
                                 .default = state.name[match(State, state.abb)])) %>% 
    mutate_at(vars(-(c("StateName", "State"))), as.numeric) %>% 
    select(order(colnames(.)), -State) %>% 
    relocate(StateName) %>% 
    rename(State = StateName) %>% 
    mutate(nottoprow = if_else(State=="United States", 0, 1)) %>% 
    arrange(nottoprow, State) %>% 
    select(-nottoprow)
  
  return(ahar)
}

