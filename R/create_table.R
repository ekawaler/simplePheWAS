#' Calls createPhewasTable.
#' Pretty much only exists to create consistency in function name formatting
#' (The other ones in simplePheWAS are formatted with underscores)
#' 
#' @param id.icd9.count A table containing, for each patient, a list of their 
#'  diagnoses and how often each appears in their records
#'
#' @return A table with rows corresponding to patients and columns corresponding to diagnoses
#'  Each cell of the table is TRUE/FALSE, depending on whether the patient had the 
#'  diagnosis appear a sufficient number of times in their record
#'  Also may contain NA, if exclusions were added (diagnoses too similar to the
#'  target diagnosis to be useful in an analysis).
#'
#' @export

create_phewas_table <- function(id.icd9.count, ...){
  createPhewasTable(id.icd9.count, ...)
}
