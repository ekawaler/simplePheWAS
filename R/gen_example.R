# Called by generate_example.
# The difference between this and generateExample is that this allows the user to
# have either no signal or have signals for as many PheWAS codes as they'd like, whereas
# the original allows for exactly one signal.
# I don't recommend using more than 3 PheWAS codes, though.

gen_example <- function(n=2000,phenotypes.per=10, hit=c("335")) {
  phewas_code <- unique(phemap$phewas_code)
  ps <- length(hit)
  #Exclude the code to add
  phewas_code <- phewas_code[!(phewas_code %in% hit)]
  #Assign individuals random phenotypes
  random <- data.frame(id=rep.int(1:n,phenotypes.per),phewas_code="",count=0)
  random$phewas_code <- sample(phewas_code,nrow(random),replace=TRUE)
  #Create the signal
  signal <- as.data.frame(MASS::mvrnorm(n=n,mu=rep(0,ps+1),Sigma=create_sigma(ps)))

  #Normalize the genotype to 0,1,2.
  names(signal)[ps+1] <- "rsEXAMPLE"
  signal$rsEXAMPLE <- signal$rsEXAMPLE+abs(min(signal$rsEXAMPLE))
  signal$rsEXAMPLE <- floor(signal$rsEXAMPLE*2.99/max(signal$rsEXAMPLE))
  signal$id <- 1:n
  signal$count <- 0

  if (ps>0) {
    for (i in seq(1,ps)) {
      #Normalize the phenotype to logical
      px <- paste("phenotype",i,sep='')
      names(signal)[i] <- px
      signal[,px] <- signal[,px]>.25 # change this number?
      signal$phewas_code <- hit[i]
      random <- rbind(random,signal[signal[,px],c("id","phewas_code","count")])
    }
  }
  random <- merge(random,phemap)
  random$count <- rpois(nrow(random),4)
  random[random$phewas_code %in% hit,]$count <- random[random$phewas_code %in% hit,]$count+2
  random <- random[random$count>0,]
  return(list(id.icd9.count=random[,c("id","icd9","count")],genotypes=signal[,c("id","rsEXAMPLE")]))
}

create_sigma <- function(ps) {
  sgma <- matrix(.25,ps+1,ps+1)
  sgma[ps+1,] <- .1*ps
  sgma[,ps+1] <- .1*ps
  diag(sgma) <- .5
  sgma[ps+1,ps+1] <- 1
  return(sgma)
}
