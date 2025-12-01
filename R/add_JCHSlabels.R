#' Add JCHS variable labels to ACS files
#'
#' Add JCHS variable labels to household plus file.
#'
#' @param df A dataframe created from load_acs of a single ACS year.
#'
#' @return Dataframe with ACS factor variables labeled.
#'
#' @note Labels are for standard JCHS variables only. Use add_ACSlabels for
#'       other ACS labels. Created for 2023; check variables for changes before
#'       using for prior years.
#'
#' @examples
#' load_acs(2023)
#' acs2023_l <- add_JCHSlabels(acs2023)
#' acs2023_l |> filter(as.integer(tenurecat) < 3) |> group_by(tenurecat) |> summarise(tot = sum(wgtp))
#'
#' @export
add_JCHSlabels <- function(df) {
  var_name <- c(
    "tenurecat",
    "tenurecat",
    "tenurecat",
    "tensimp",
    "tensimp",
    "cost_burden",
    "cost_burden",
    "cost_burden",
    "hh_inccat",
    "hh_inccat",
    "hh_inccat",
    "hh_inccat",
    "hh_inccat",
    "hh_incquint",
    "hh_incquint",
    "hh_incquint",
    "hh_incquint",
    "hh_incquint",
    "hh_incquart",
    "hh_incquart",
    "hh_incquart",
    "hh_incquart",
    "hhtype",
    "hhtype",
    "hhtype",
    "hhtype",
    "hhtype",
    "hhtype",
    "agecat",
    "agecat",
    "agecat",
    "agecat",
    "age2cat",
    "age2cat",
    "age2cat",
    "age2cat",
    "age2cat",
    "age2cat",
    "age3cat",
    "age3cat",
    "age3cat",
    "age3cat",
    "age3cat",
    "age3cat",
    "age3cat",
    "race4cat",
    "race4cat",
    "race4cat",
    "race4cat",
    "race5cat",
    "race5cat",
    "race5cat",
    "race5cat",
    "race5cat",
    "race7cat",
    "race7cat",
    "race7cat",
    "race7cat",
    "race7cat",
    "race7cat",
    "race7cat",
    "ed1cat",
    "ed1cat",
    "ed1cat",
    "ed1cat",
    "ed1cat",
    "ed1cat",
    "ed1cat",
    "ed2cat",
    "ed2cat",
    "ed2cat",
    "ed2cat",
    "emp12",
    "emp12",
    "emp12",
    "emp12",
    "emp12_rev",
    "emp12_rev",
    "emp12_rev",
    "emp12_rev",
    "emp12_rev",
    "recentmover",
    "recentmover"
  )
  value <- c(
    1, # tenurecat
    2,
    3,
    1, # tensimp
    2,
    1, # cost_burden
    2,
    3,
    1, # hh_inccat
    2,
    3,
    4,
    5,
    1, # hh_incquint
    2,
    3,
    4,
    5,
    1, #hh_incquart
    2,
    3,
    4,
    1, # hhtype
    2,
    3,
    4,
    5,
    6,
    1, # agecat
    2,
    3,
    4,
    1, # age2cat
    2,
    3,
    4,
    5,
    6,
    1, # age3cat
    2,
    3,
    4,
    5,
    6,
    7,
    1, # race4cat
    2,
    3,
    4,
    1, # race5cat
    2,
    3,
    4,
    5,
    1, # race7cat
    2,
    3,
    4,
    5,
    6,
    7,
    1, # ed1cat
    2,
    3,
    4,
    5,
    6,
    7,
    1, # ed2cat
    2,
    3,
    4,
    1, # emp12
    2,
    3,
    4,
    1, # emp12_rev
    2,
    3,
    4,
    5,
    0, # recentmover
    1
  )
  label <- c(
    "Own_w_mtg", # tenurecat
    "Own_wo_mtg",
    "Rent", # tensimp
    "Own",
    "Rent",
    "Unburdened", # cost_burden
    "Moderately Burdened",
    "Severely Burdened",
    "Less than $15,000", # hh_inccat
    "$15,000-29,999",
    "$30,000-44,999",
    "$45,000-74,999",
    "$75,000 or more",
    "Bottom", # hh_incquint
    "Lower middle",
    "Middle",
    "Upper middle",
    "Top",
    "Bottom", # hh_incquart
    "Lower middle",
    "Upper middle",
    "Top",
    "Married w/o kids", # hhtype
    "Married w/ kids",
    "Single parent",
    "Other fam",
    "Single person",
    "Other non-fam",
    "Under 25", # agecat
    "25-44",
    "45-64",
    "65 and over",
    "Under 25", #age2cat
    "25-34",
    "35-44",
    "45-54",
    "55-64",
    "65 and over",
    "Under 18", # age3cat
    "18-24",
    "25-34",
    "35-44",
    "45-54",
    "55-65",
    "65 and over",
    "White, NH", # race4cat
    "Black, NH",
    "Hispanic",
    "Asian/another, NH",
    "White, NH", # race5cat
    "Black, NH",
    "Hispanic",
    "Asian, NH",
    "Another, NH",
    "White, NH", # race7cat
    "Black, NH",
    "Hispanic",
    "Asian, NH",
    "Native American, NH",
    "Multiracial, NH",
    "Another, NH",
    "No high school", # ed1cat
    "Some high school",
    "High school diploma/GED",
    "Some college",
    "Associates",
    "Bachelors",
    "Grad",
    "No high school diploma", # ed2cat
    "High school diploma/GED",
    "Some college",
    "Bachelor's or higher",
    "Full employment", # emp12
    "Short-term unemployment",
    "Long-term unemployment",
    "Full unemployment",
    "Full employment", # emp12_rev
    "Short-term unemployment",
    "Long-term unemployment",
    "Full unemployment",
    "Not in labor force",
    "Non-mover", # recentmover
    "Moved in past 12 months"
  )
  dict <- data.frame(var_name, value, label)

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
