## Load your packages, e.g. library(drake).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(checkwho_plan, 
             lock_envir = FALSE)


# p = vis_drake_graph(checkwho_plan)

# make(checkwho_plan, targets = 'pre.post.eligibility.dt', force = TRUE)
# make(checkwho_plan, targets = 'adhb.revision.one.recalculated.theatre.event.dt', force = TRUE)
# make(checkwho_plan, targets = c('pre.post.eligibility.figure.numbers.dt',
#                                 'eligibility.figure.numbers.dt'), 
#      force = TRUE)
make(checkwho_plan)

# plot(readd(mortality.time.plot))
# readd(daoh.time.plot)
# readd(daoh.time.plot)z
# plot(readd(daoh.exceedance.time.plot))

# loadd(publication.table.demographics,
#       publication.figure.daoh.with.mort,
#       publication.table.comprehensive.daoh.summary,
#       publication.figure.daoh.risk.adj,
#       publication.figure.daoh.raw,
#       publication.figure.mort.reg,
#       publication.figure.mort.reg.abridged,
#       publication.table.mort.reg,
#       publication.figure.daoh.reg,
#       publication.figure.daoh.reg.abridged,
#       publication.table.daoh.reg,
#       publication.table.facility.type.summary,
#       publication.figure.daoh.exceedance.time.plot,
#       publication.figure.mortality.time.plot,
#       publication.figure.changepoint.model.with.prior.gradient.change.plot,
#       publication.table.bayes.testing,
#       template.docx.filein,
#       report.output.dir,
#       report.docx.filein,
#       page.width.inches,
#       figure.dpi,
#       image.output.format)
# 
# report.docx = compile.manuscript(
#   asset.list = list(publication.table.demographics,
#                     publication.figure.daoh.with.mort,
#                     publication.table.comprehensive.daoh.summary,
#                     publication.figure.daoh.risk.adj,
#                     publication.figure.daoh.raw,
#                     publication.figure.mort.reg,
#                     publication.figure.mort.reg.abridged,
#                     publication.table.mort.reg,
#                     publication.figure.daoh.reg,
#                     publication.figure.daoh.reg.abridged,
#                     publication.table.daoh.reg,
#                     publication.table.facility.type.summary,
#                     publication.figure.daoh.exceedance.time.plot,
#                     publication.figure.mortality.time.plot,
#                     publication.figure.changepoint.model.with.prior.gradient.change.plot,
#                     publication.table.bayes.testing),
#   template.docx.path = template.docx.filein,
#   report.output.dir = report.output.dir,
#   report.docx.input.path = report.docx.filein,
#   page.width.inches = page.width.inches,
#   figure.dpi = figure.dpi,
#   image.output.format = image.output.format,
#   skip.report.figure.inds = 1
# )
