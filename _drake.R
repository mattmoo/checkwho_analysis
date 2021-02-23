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

rev3.targets = c(
  'report.output.dir',
  'pre.post.svy.des',
  'time.series.raw.svy.des',
  'ethnicity.svy.des',
  'pre.post.raw.svy.des',
  'daoh.mortality.plot',
  'daoh.mortality.notransform.plot',
  'daoh.pre.post.raw.plot',
  'daoh.pre.post.risk.adj.plot',
  'daoh.combined.pre.post.summary.dt',
  # 'risk.adjustment.plot.animation',
  'SSC.dra.riskgp.plot',
  'SSC.dra.riskgp.group.diff.plot',
  'ethnicity.dra.riskgp.group.diff.plot',
  # 'SSC.dra.riskgp.daoh.animation',
  'publication.table.demographics',
  'publication.table.rev3.comprehensive.daoh.summary',
  'publication.table.rev3.comprehensive.ethnicity.daoh.summary',
  'publication.table.rev3.comprehensive.maori.ethnicity.daoh.summary'
  # 'daoh.intervention.changepoint.mcp.fit',
  # 'mort.90.day.intervention.changepoint.mcp.fit'
  
)
# make(checkwho_plan, 'risk.adjusted.regression.dt')
# p = vis_drake_graph(checkwho_plan)
make(checkwho_plan, rev3.targets)
# make(checkwho_plan, 'pre.post.svy.des')
# loadd(rev3.targets)
asassasa

animate(
  plot = risk.adjustment.plot.animation,
  height = 1080,
  width = 1920,
  duration = 3, 
  fps = 60,
  renderer = av_renderer(file.path(report.output.dir, 'risk_adjustment_plot_animation.mp4'))
)

animate(
  plot = dra.riskgp.daoh.animation,
  height = 720,
  width = 1280,
  duration = 12, 
  fps = 30,
  # renderer = av_renderer(file.path(report.output.dir, 'dra_riskgp_daoh_animation.mp4'),
  renderer = gifski_renderer(file.path(report.output.dir, 'dra_riskgp_daoh_animation.gif'))
)

# make(checkwho_plan, targets = 'mort.regression.abbreviated.plot', force = TRUE)
make(checkwho_plan, targets = c('publication.results.demographics',
                                'publication.results.maori',
                                'publication.results.daoh',
                                'publication.results.mortality',
                                'publication.table.demographics',
                                'publication.figure.daoh.with.mort',
                                'publication.table.comprehensive.daoh.summary',
                                'publication.figure.daoh.risk.adj',
                                'publication.figure.daoh.raw',
                                'publication.figure.mort.reg',
                                'publication.figure.mort.reg.abridged',
                                'publication.table.mort.reg',
                                'publication.figure.daoh.reg',
                                'publication.figure.daoh.reg.abridged',
                                'publication.table.daoh.reg',
                                'publication.table.facility.type.summary',
                                'publication.figure.daoh.exceedance.time.plot',
                                'publication.figure.mortality.time.plot',
                                'publication.figure.changepoint.model.with.prior.gradient.change.plot',
                                'publication.table.bayes.testing',
                                'template.docx.filein',
                                'report.output.dir',
                                'report.docx.filein',
                                'page.width.inches',
                                'figure.dpi',
                                'image.output.format'), force = TRUE)

loadd(publication.results.demographics,
      publication.results.maori,
      publication.results.daoh,
      publication.results.mortality
)
asassasaq

loadd(publication.table.demographics,
      publication.figure.daoh.with.mort,
      publication.table.comprehensive.daoh.summary,
      publication.figure.daoh.risk.adj,
      publication.figure.daoh.raw,
      publication.figure.mort.reg,
      publication.figure.mort.reg.abridged,
      publication.table.mort.reg,
      publication.figure.daoh.reg,
      publication.figure.daoh.reg.abridged,
      publication.table.daoh.reg,
      publication.table.facility.type.summary,
      publication.figure.daoh.exceedance.time.plot,
      publication.figure.mortality.time.plot,
      publication.figure.changepoint.model.with.prior.gradient.change.plot,
      publication.table.bayes.testing,
      template.docx.filein,
      report.output.dir,
      report.docx.filein,
      page.width.inches,
      figure.dpi,
      image.output.format)

report.docx = compile.manuscript(
  asset.list = list(publication.table.demographics,
                    publication.figure.daoh.with.mort,
                    publication.table.comprehensive.daoh.summary,
                    publication.figure.daoh.risk.adj,
                    publication.figure.daoh.raw,
                    publication.figure.mort.reg,
                    publication.figure.mort.reg.abridged,
                    publication.table.mort.reg,
                    publication.figure.daoh.reg,
                    publication.figure.daoh.reg.abridged,
                    publication.table.daoh.reg,
                    publication.table.facility.type.summary,
                    publication.figure.daoh.exceedance.time.plot,
                    publication.figure.mortality.time.plot,
                    publication.figure.changepoint.model.with.prior.gradient.change.plot,
                    publication.table.bayes.testing),
  template.docx.path = template.docx.filein,
  report.output.dir = report.output.dir,
  report.docx.input.path = report.docx.filein,
  page.width.inches = page.width.inches,
  figure.dpi = figure.dpi,
  image.output.format = image.output.format,
  skip.report.figure.inds = 1
)
