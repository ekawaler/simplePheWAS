# The same as phenotypePlot, but uses ggrepel to keep labels from overlapping.

phenotype_plot <-
  function(d, max.y,max.x, suggestive.line, significant.line,
           size.x.labels=9, size.y.labels=9, switch.axis=F, sort.by.value=F, sort.by.category.value=F,
           #annotation
           annotate.phenotype.description,
           annotate.angle=30, annotate.size=5, annotate.level,
           annotate.phenotype=F,
           annotate.snp.w.phenotype=F,
           annotate.snp=F, annotate.snp.angle=0,
           annotate.list, annotate.only.largest=T,
           #labels
           lc.labels=F,
           x.group.labels=T, x.phenotype.labels=F,
           sizes=F, direction=F, point.size=3,
           #plot characteristics
           use.color=T,
           color.palette,
           title= paste0("Phenotype Plot ", date()),
           x.axis.label="Phenotypes",
           y.axis.label="Values",
           y.axis.interval=5) {
    phenotypePlot(d,max.y,max.x, suggestive.line, significant.line,
                  size.x.labels, size.y.labels, switch.axis, sort.by.value, sort.by.category.value,
                  #annotation
                  annotate.phenotype.description,
                  annotate.angle, annotate.size, annotate.level,
                  annotate.phenotype,
                  annotate.snp.w.phenotype,
                  annotate.snp, annotate.snp.angle,
                  annotate.list, annotate.only.largest,
                  #labels
                  lc.labels,
                  x.group.labels, x.phenotype.labels,
                  sizes, direction, point.size,
                  #plot characteristics
                  use.color,
                  color.palette,
                  title,
                  x.axis.label,
                  y.axis.label,
                  y.axis.interval)
  }
