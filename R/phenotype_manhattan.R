#' @export
# Makes the Manhattan plot. Basically the only difference from phenotypeManhattan is that
# it calls the altered version of phenotype_plot (keeps point labels from overlapping)

phenotype_manhattan <-
  function(d, suggestive.line=0.05, significant.line,
           OR.size=F,OR.direction=F,
           annotate.level,
           y.axis.interval=5,
           y.axis.label=expression(-log[10](italic(p))),
           max.y,
           ...) {
    phenotypeManhattan(d, suggestive.line, significant.line, OR.size,
                       OR.direction, annotate.level, y.axis.interval, 
                       y.axis.label, max.y, ...)
}
