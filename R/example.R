#' @export
# Runs through an example workflow

ex_function <- function(){
  # Load the PheWAS package
  library(simplePheWAS)
  # Set the random seed so it is replicable
  set.seed(2)
  # Generate some example data
  #ex <- generate_example(number_of_patients=2000, phenotypes_per_patient=10, code_to_enhance=c("335","764","327.1"))
  # Extract the two parts from the returned list
  #id.icd9.count <- ex$id.icd9.count
  #genotypes <- ex$genotypes
  # Or, if not generating example data, read in the data from external sources
  data(testset1)
  genotypes=testset1$genotypes
  id.icd9.count=testset1$id.icd9.count
  # Create the PheWAS code table translates the ICD9s, adds exclusions, and reshapes to a wide format
  phenotypes <- create_phewas_table(id.icd9.count)
  # Run the PheWAS
  results <- phewas_with_bonferroni(phenotypes,genotypes)
  # Print the first ten rows
  print(slice(results,1:10))
  # Plot the results
  print(phewas_manhattan(results))
  # List the significant results
  results[results$bonferroni&!is.na(results$p),]
  # List the top 10 results
  results[order(results$p)[1:10],]
  # Print the summary table
  summarization_table(phenotypes, genotypes, results)
}
