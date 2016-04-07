# For generating example data. Basically just transforms the variable names
# to call gen_example. Might remove this all together later.

generate_example <- function(number_of_patients=5, phenotypes_per_patient=5, code_to_enhance=c("427.8")){
  gen_example(n=number_of_patients, phenotypes.per=phenotypes_per_patient,
                          hit=code_to_enhance)
}
