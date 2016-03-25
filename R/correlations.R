summarization <- function(phenotypes, genotypes, results_d){
  sig=filter(results_d,bonferroni&!is.na(p))
  pw_codes=sig$phewas_code
  #print(sig)
  for (c in pw_codes){
    result_row = sig[sig$phewas_code==c,]
    cat("For PheWAS code ",c," (",result_row$phewas_description,"):\n",sep='')
    # % won't be the same for each code - doesn't count NAs
    cat("--> Patients in dataset with phenotype: ",result_row$n_cases," (",
        round(result_row$n_cases*100/result_row$n_total,2),"% of total)\n",sep='')
    cat("--> Odds ratio with SNP ",result_row$snp,": ",result_row$OR,"\n",sep='')
    #tmp=phenotypes[!is.na(phenotypes[,c])&phenotypes[,c]=="TRUE",]
    tmp=phenotypes[!is.na(phenotypes[,c]),]
    res = left_join(tmp[,c("id",c)],genotypes,by="id")
    ctable = table(res[,2:3])
    cat("--> Of people presenting with ",result_row$phewas_description,": ",
        round(100*ctable["TRUE","0"]/sum(ctable["TRUE",]),2),"% have genotype 0, ",
        round(100*ctable["TRUE","1"]/sum(ctable["TRUE",]),2),"% have genotype 1, and ",
        round(100*ctable["TRUE","2"]/sum(ctable["TRUE",]),2),"% have genotype 2","\n",sep='')
    cat("--> Of people presenting without ",result_row$phewas_description,": ",
        round(100*ctable["FALSE","0"]/sum(ctable["FALSE",]),2),"% have genotype 0, ",
        round(100*ctable["FALSE","1"]/sum(ctable["FALSE",]),2),"% have genotype 1, and ",
        round(100*ctable["FALSE","2"]/sum(ctable["FALSE",]),2),"% have genotype 2","\n",sep='')
    cat("--> Of the ",sum(ctable[,"0"])," people with genotype 0: ",
        round(100*ctable["TRUE","0"]/sum(ctable[,"0"]),2),"% have ",
        result_row$phewas_description,"\n",sep='')
    cat("--> Of the ",sum(ctable[,"1"])," people with genotype 1: ",
        round(100*ctable["TRUE","1"]/sum(ctable[,"1"]),2),"% have ",
        result_row$phewas_description,"\n",sep='')
    cat("--> Of the ",sum(ctable[,"2"])," people with genotype 2: ",
        round(100*ctable["TRUE","2"]/sum(ctable[,"2"]),2),"% have ",
        result_row$phewas_description,"\n",sep='')
    cat("\n")
  }
}
