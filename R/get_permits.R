#' Get Building Permits Survey
#'
#' Cleans census URLs to directly download and format annual county or state permit
#' datasets
#'
#' @param geography Select the geography to pull. Valid inputs are "state" or 
#'                  "county" 
#' 
#' @param year The year to extract, starting with 1980 for states and 1990 for
#'             counties.
#'
#' @return A tibble with 31 columns.
#'
#' @note Adapted from Chris Goodman: 
#'       "https://gist.github.com/cbgoodman/8b78153e93148cdff370e3c6f3629ade.js"
#'       Can extract multiple years at once using purrr::map (see examples).
#'
#' @examples
#' # pull state permits for 2023 and select unit variables
#' sp23 <- get_permits("state", 2023)
#' sp23 |> 
#'   select(year, region, state_name, starts_with("units_") & ends_with("unit"))
#'
#' # pull multiple years at once and bind into long df 
#' sp_list <- map(2003:2023, get_permits, geography = "state")
#' sp <- bind_rows(sp_list)
#' 
#' @export
get_permits <- function(geography, year) {
  if (geography != "state" & geography != "county") {
    stop("enter valid geography")
  }
  else if (geography == "state") {
    url_loc <- "www2.census.gov/econ/bps/State/"
    
    files <- rvest::read_html("https://www2.census.gov/econ/bps/State/") |>
      rvest::html_elements("a") |>
      rvest::html_text2()
    
    ### Just take files containing  `coYYYYa``
    fname_stubs <- files[str_detect(files, "(st)\\d{4}(:?a)")]
    fname_stubs <- fname_stubs[grepl(year, fname_stubs) == TRUE]
    
    ### Add year
    fname_year <- substr(fname_stubs, start = 3, stop = 6)
    
    ### Clean up the URLs
    fnames <- paste0("https://", url_loc, fname_stubs)
    fnames <- set_names(fnames, fname_year)
    
    ### Import
    cpb <- fnames |>
      purrr::map(~ read_csv(., skip = 1)) |>
      list_rbind(names_to = "year") |>
      dplyr::rename(
        survey = Date,
        state_fips = State,
        region = "Code...3",
        division = "Code...4",
        state_name = Name,
        bldg_1unit = "Bldgs...6",
        units_1unit = "Units...7",
        val_1unit = "Value...8",
        bldg_2unit = "Bldgs...9",
        units_2unit = "Units...10",
        val_2unit = "Value...11",
        bldg_34unit = "Bldgs...12",
        units_34unit = "Units...13",
        val_34unit = "Value...14",
        bldg_5unit = "Bldgs...15",
        units_5unit = "Units...16",
        val_5unit = "Value...17",
        bldg_1unit_rep = "Bldgs...18",
        units_1unit_rep = "Units...19",
        val_1unit_rep = "Value...20",
        bldg_2unit_rep = "Bldgs...21",
        units_2unit_rep = "Units...22",
        val_2unit_rep = "Value...23",
        bldg_34unit_rep = "Bldgs...24",
        units_34unit_rep = "Units...25",
        val_34unit_rep = "Value...26",
        bldg_5unit_rep = "Bldgs...27",
        units_5unit_rep = "Units...28",
        val_5unit_rep = "Value...29"
      )
  } 
  else {
    url_loc <- "www2.census.gov/econ/bps/County/"
    
    files <- rvest::read_html("https://www2.census.gov/econ/bps/County/") |>
      rvest::html_elements("a") |>
      rvest::html_text2()
    
    ### Just take files containing  `coYYYYa``
    fname_stubs <- files[str_detect(files, "(co)\\d{4}(:?a)")]
    # remove 2022 update file
    fname_stubs <- fname_stubs[-34]
    fname_stubs <- fname_stubs[grepl(year, fname_stubs) == TRUE]

    ### Add year
    fname_year <- substr(fname_stubs, start = 3, stop = 6)
    
    ### Clean up the URLs
    fnames <- paste0("https://", url_loc, fname_stubs)
    fnames <- set_names(fnames, fname_year)
    
    ### Import
    cpb <- fnames |>
      purrr::map(~ read_csv(., skip = 1)) |>
      list_rbind(names_to = "year") |>
      dplyr::rename(
        survey = Date,
        state_fips = State,
        county_fips = County,
        region = "Code...4",
        division = "Code...5",
        county_name = Name,
        bldg_1unit = "Bldgs...7",
        units_1unit = "Units...8",
        val_1unit = "Value...9",
        bldg_2unit = "Bldgs...10",
        units_2unit = "Units...11",
        val_2unit = "Value...12",
        bldg_34unit = "Bldgs...13",
        units_34unit = "Units...14",
        val_34unit = "Value...15",
        bldg_5unit = "Bldgs...16",
        units_5unit = "Units...17",
        val_5unit = "Value...18",
        bldg_1unit_rep = "Bldgs...19",
        units_1unit_rep = "Units...20",
        val_1unit_rep = "Value...21",
        bldg_2unit_rep = "Bldgs...22",
        units_2unit_rep = "Units...23",
        val_2unit_rep = "Value...24",
        bldg_34unit_rep = "Bldgs...25",
        units_34unit_rep = "Units...26",
        val_34unit_rep = "Value...27",
        bldg_5unit_rep = "Bldgs...28",
        units_5unit_rep = "Units...29",
        val_5unit_rep = "Value...30"
      )
    }
  return(cpb)
}
