#' Export tables to workbook
#'
#' Exports tibbles and named objects to a worksheet of the same name in either
#' a new or existing workbook
#'
#' @param tab The named tibble or object to write.
#'
#' @param input_wb The .xlsx workbook to read, including the file path,
#' in quotations.
#'
#' @param output_wb The .xlsx workbook to write to, including the file path,
#' in quotations. If the output is the same as the input_wb, the input_wb will
#' be overwritten or added to.
#'
#' @return Workbook with sheets named after input tab.
#'
#' @note Existing tab will be overwritten if worksheet already exists.
#'
#' @examples
#' tenTable <- acs2023 |> tab(tensimp, w = wgtp)
#' export_table(tenTable, "C:/Data/tabswb.xlsx", "C:/Data/tabswb.xlsx")
#'
#' @export
export_table <- function(tab, input_wb, output_wb) {

  if (file.exists(input_wb)) {
    wb <- openxlsx::loadWorkbook(input_wb)
  }
  else {
    wb <- openxlsx::createWorkbook(input_wb)
  }

  sheet_name = deparse(substitute(tab))

  if (!sheet_name %in% names(wb)) {
    openxlsx::addWorksheet(wb, sheet_name)
  }

  openxlsx::writeData(wb, sheet = sheet_name, x = tab,
                      startCol = 1, startRow = 1, colNames = TRUE)

  openxlsx::saveWorkbook(wb, output_wb, overwrite = TRUE)

}
