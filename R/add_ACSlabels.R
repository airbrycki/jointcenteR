#' Add labels to ACS files
#'
#' Uses ACS PUMS data dictionary to add labels to household plus file.
#'
#' @param df A dataframe created from load_acs of a single ACS year.
#'
#' @param dict_url The URL of the data dictionary .csv file that corresponds to
#' the df year.
#'
#' @return Dataframe with ACS factor variables labeled.
#'
#' @note Labels are for standard ACS variables only. JCHS variables will need to
#' be labeled separately.
#'
#' @examples
#' dict_url <-
#' "https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/
#'  PUMS_Data_Dictionary_2023.csv"
#' load_acs(2023)
#' acs2023 <- add_ACSlabels(acs2023, dict_url)
#' acs2023 |>
#'   filter(as.integer(ten) >2) |>
#'   group_by(ten) |>
#'   summarise(tot = sum(wgtp))
#'
#' @export
add_ACSlabels <- function(df, dict_url) {
  # Load the dictionary from the URL (CSV format)
  dict <- read.csv(dict_url, sep = ",", header = FALSE) |>
    dplyr::rename(row_type = V1,
                  var_name = V2,
                  data_type = V3,
                  data_length = V4,
                  value1 = V5,
                  value = V6,
                  label = V7) |>
    dplyr::filter(row_type == "VAL" & data_type == "C" & data_length <= 4 &
                  label != "Puerto Rico" & label != "Puerto Rico/PR") |>
    dplyr::mutate(value = dplyr::if_else(value %in% c("b", "bb", "bbb", "bbbb"),
                                         NA_integer_, as.numeric(value))) |>
    dplyr::filter(!is.na(value))

  # Iterate over columns in the dataframe to apply labels
  for (n in names(df)) {
    if (tolower(n) %in% tolower(dict$var_name)) {
      # Find corresponding dictionary rows for each variable
      temp <- dplyr::filter(dict, tolower(var_name) == tolower(n))

      # Check if there are valid labels in the dictionary
      if (nrow(temp) > 0) {
        # Apply the factor labels based on 'value' and 'label'
        df[[n]] <- factor(df[[n]], levels = temp$value, labels = temp$label)
      } else {
        # If no matching labels in dictionary, leave the column as is
        message(paste("No labels found for", n))
      }
    }
  }
  return(df)
}

