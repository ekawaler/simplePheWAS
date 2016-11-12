#' @export
# Calls phewasManhattan, is just here for naming consistency

phewas_manhattan <-
  function(d, add.phewas.descriptions=T, ...) {
    phewasManhattan(d, add.phewas.descriptions, ...)
  }
