#' @export
# The actual PheWAS functions.
# Can either use Bonferroni correction, FDR correction, or both,
# depending on which function is called.
# Currently no "explain" is available for the "both" option.
# Maybe break this up into several files for documentation

phewas_with_bonferroni<-
  function(phenotypes,genotypes, alpha=0.05, explain=TRUE, verbose=TRUE, ...){
    res <- phewas(phenotypes,genotypes,cores=1,alpha=alpha,significance.threshold=c("bonferroni"), ...)
    if (explain) {explain_phewas_bon(res,verbose)}
    resplus <- addPhewasDescription(res, for.plots=TRUE)
    return(resplus)
  }

phewas_with_fdr<-
  function(phenotypes,genotypes, alpha=0.05, explain=TRUE, verbose=TRUE, ...){
    res <- phewas(phenotypes,genotypes,cores=1,alpha=alpha,significance.threshold=c("fdr"), ...)
    if (explain) {explain_phewas_fdr(res,verbose)}
    resplus <- addPhewasDescription(res)
    return(resplus)
  }

phewas_with_both<-
  function(phenotypes,genotypes, alpha=0.05, explain=TRUE, verbose=TRUE, ...){
    res <- phewas(phenotypes,genotypes,cores=1,alpha=alpha,significance.threshold=c("fdr","bonferroni"), ...)
    resplus <- addPhewasDescription(res)
    return(resplus)
  }

explain_phewas_bon <- function(res,verbose){
  results_des <- addPhewasDescription(res)
  signif_res <- results_des[results_des$bonferroni&!is.na(results_des$p),]
  if (dim(signif_res)[1]>0) {
    for (i in seq(1,dim(signif_res)[1])) {
      cat("The SNP ",signif_res$snp[i]," is significantly associated with ",
          signif_res$phewas_description[i]," (PheWAS code ",signif_res$phewas_code[i],
          ") with a p-value of ",signif(signif_res$p[i], digits=3)," after Bonferroni correction.\n",sep='')
      if (verbose) {
        cat("--> PheWAS code ",signif_res$phewas_code[i]," includes the following ICD-9 code(s): ",
            paste(phemap$icd9[phemap$phewas_code==signif_res$phewas_code[i]],collapse=', '),"\n\n",sep='')
      }
    }
    print(signif_res)
  }
  else {print("No results significant with Bonferroni correction.")}
}

explain_phewas_fdr <- function(res,verbose){
  results_des <- addPhewasDescription(res)
  signif_res <- results_des[results_des$fdr&!is.na(results_des$p),]
  if (dim(signif_res)[1]>0) {
    for (i in seq(1,dim(signif_res)[1])) {
      cat("The SNP ",signif_res$snp[i]," is significantly associated with ",
                        signif_res$phewas_description[i]," (PheWAS code ",signif_res$phewas_code[i],
                        ") with a p-value of ",signif(signif_res$p[i], digits=3)," after FDR correction.\n",sep='')
        if (verbose) {
          cat("--> PheWAS code ",signif_res$phewas_code[i]," includes the following ICD-9 code(s): ",
                       paste(phemap$icd9[phemap$phewas_code==signif_res$phewas_code[i]],collapse=', '),"\n\n",sep='')
        }
    }
    print(signif_res)
  }
  else {print("No results significant with Bonferroni correction.")}
}
