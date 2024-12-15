#' Load JCHS multiyear ACS file
#'
#' Reads JCHS multiyear ACS file into global environment
#'
#' @param path A path to the location of multiyear .csv files.
#' 
#' @param filename The name of the file to load.
#'
#' @return Dataframes loaded into global environment named multiyear.
#'
#' @note Multiyear files should be in one location and saved as .csv files.
#'       Splitting the multiyear file for renters or owners will help them 
#'       load faster if you are routinely using one or the other.
#'
#' @examples
#' acspath <- "C:/Data/ACS/"
#' multfilename<- "ACS_multiyear_hhplus_RentersPlusVacant.csv"
#' load_mult()
#' load_mult(acspath, multfilename)
#' 
#' @export
load_mult <- function(path = acspath, filename = multfilename) {
  multiyear <- data.table::fread(paste0(path, filename))
  
  # Assign the data.table to the global environment
  assign("multiyear", multiyear, envir = .GlobalEnv)
}
