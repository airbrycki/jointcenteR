#' Start factchecking spreadsheet
#'
#' Splits chapter at sentence breaks and puts each new sentence on a new row
#'
#' @param txtfile The full path and name of the text file in quotations.
#'
#' @return A dataframe where each line is a sentence or header.
#'
#' @note Word files should be saved to .txt using Unicode (UTF-8) encoding.
#' Splits based on occurrence of ". " or a carriage return. Can use with
#' jointcenteR::export_table() to export to its own tab of a spreadsheet.
#'
#' @examples
#' chapter1 <- factcheck_format("C:/Data/testtext.txt")
#'
#' @export
factcheck_format <- function(txtfile) {
  inputfile <- read_file(txtfile)

  output_chr <- data.frame(strsplit(inputfile, '\\. |\\n'))
  output_df <- bind_rows(output_chr)
  names(output_df) <- c("original_text")

  return(output_df)
}
