# Allows the user to see which PheWAS codes encompass their
# ICD9 code of interest.

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
