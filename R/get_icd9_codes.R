#' Conversion of ICD-9 codes to PheWAS codes
#'
#' Prints for the user a list of the PheWAS codes that encompass their
#' ICD-9 code(s) of interest.
#' 
#' @param icd9_query vector of the ICD-9 codes to convert
#'
#' @return None
#' 
#' @examples
#' get_icd9_codes()
#' get_icd9_codes(c("088.81","335"))
#' 
#' @export

get_icd9_codes <- function(icd9_query=c("088.81")) {
  for (i9 in icd9_query) {
    # Get the PheWAS codes that associate with the ICD-9
    codes<-phemap[phemap$icd9==i9,]$phewas_code

    cat("ICD-9 code",i9,"is associated with the following PheWAS code(s):\n")

    # For each code returned,
    for (code in codes){
      cat("-->",code,":",pheinfo[pheinfo$phewas_code==code,]$phewas_description,"\n")
    }
  }
}
