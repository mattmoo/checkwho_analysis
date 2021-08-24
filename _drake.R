## Load your packages, e.g. library(drake).
source("./packages.R")

# Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(checkwho_plan, 
             lock_envir = FALSE)

rev3.targets = c(
  # 'eligibility.figure.numbers.dt',
  # 
  # 'publication.table.demographics',
  # 'publication.table.rev3.comprehensive.daoh.summary',
  # 'publication.table.rev3.comprehensive.ethnicity.daoh.summary',
  # 'publication.table.rev3.comprehensive.maori.ethnicity.daoh.summary',
  # 'publication.table.mort.reg',
  # 
  # 'publication.figure.daoh.with.mort',
  # 'publication.figure.daoh.with.mort.notrans',
  # 'publication.figure.daoh.risk.adj',
  # 'publication.figure.SSC.dra.riskgp.plot',
  # 'publication.figure.SSC.dra.riskgp.group.diff.plot',
  # 'publication.figure.ethnicity.dra.riskgp.group.diff.plot',
  # 'publication.figure.daoh.changepoint.plot',
  # 'publication.figure.mort.90.day.changepoint.plot',
  # 'publication.figure.daoh.effect.size.plot',
  # 'publication.figure.mort.90.day.effect.size.plot',
  # 'publication.figure.mort.reg',
  # 
  # 'publication.results.demographics',
  # 'publication.results.daoh',
  # 'publication.results.mortality',
  # 'publication.results.maori',
  # 'publication.results.daoh.emp.logit.changepoint',
  # 'publication.results.daoh.linear.changepoint',
  # 'publication.results.mort.90.changepoint',
  # 'publication.results.loo',
  # 
  # 'scaled.inverse.emp.logit',



  
)

pres.targets = c(  'presentation.output.dir',
                   'pres.figure.aspect.ratio',
                   'pres.figure.max.width.px',
                   'pres.figure.width.dpi',
                   
                   'presentation.figure.daoh.notrans',
                   'presentation.figure.daoh.sqrt',
                   'presentation.figure.daoh.mort.sqrt',
                   'daoh.pre.post.raw.plot',
                   
                   'mort.los.risk.density.with.margins.plot',
                   'los.risk.density.plot',
                   'mort.risk.density.plot'
                   )

# make(checkwho_plan, c('gaussian.emp.logit.daoh.ethnicity.mcmc.dt',
#                       'mort.90.day.ethnicity.mcmc.dt'))

# make(checkwho_plan, 'risk.adjusted.regression.dt')
# p = vis_drake_graph(checkwho_plan)
# make(checkwho_plan, 'publication.table.rev3.comprehensive.daoh.summary')
make(checkwho_plan, pres.targets)
# make(checkwho_plan, c('daoh.linear.simpler.changepoint.plot',
#                       'gaussian.linear.daoh.mcp.fit',
#                       'daoh.linear.effect.size.plot'))
# loadd(daoh.emp.logit.changepoint.plot)
# assaassasa

make(
  checkwho_plan,
  c(
    'template.docx.filein',
    'report.output.dir',
    'report.docx.filein',
    'page.width.inches',
    'figure.dpi',
    'image.output.format'
  ),
  force = TRUE
)


# make(checkwho_plan, c('date.status.dt', 'date.status.with.surgery.dt'))
# make(checkwho_plan, 'pre.post.svy.des')
# loadd(rev3.targets)
# asassasa
# 
# animate(
#   plot = risk.adjustment.plot.animation,
#   height = 1080,
#   width = 1920,
#   duration = 3, 
#   fps = 60,
#   renderer = av_renderer(file.path(report.output.dir, 'risk_adjustment_plot_animation.mp4'))
# )
# 
# animate(
#   plot = dra.riskgp.daoh.animation,
#   height = 720,
#   width = 1280,
#   duration = 12, 
#   fps = 30,
#   # renderer = av_renderer(file.path(report.output.dir, 'dra_riskgp_daoh_animation.mp4'),
#   renderer = gifski_renderer(file.path(report.output.dir, 'dra_riskgp_daoh_animation.gif'))
# )
saasasas

loadd(
  publication.table.demographics,
  publication.figure.daoh.with.mort,
  publication.figure.daoh.with.mort.notrans,
  publication.table.rev3.comprehensive.daoh.summary,
  publication.table.rev3.comprehensive.ethnicity.daoh.summary,
  publication.table.rev3.comprehensive.maori.ethnicity.daoh.summary,
  publication.table.mort.reg,
  
  publication.figure.daoh.risk.adj,
  publication.figure.SSC.dra.riskgp.plot,
  publication.figure.SSC.dra.riskgp.group.diff.plot,
  publication.figure.ethnicity.dra.riskgp.group.diff.plot,
  publication.figure.daoh.changepoint.plot,
  publication.figure.mort.90.day.changepoint.plot,
  publication.figure.daoh.effect.size.plot,
  publication.figure.mort.90.day.effect.size.plot,
  publication.figure.mort.reg,
  
  publication.results.demographics,
  publication.results.daoh,
  publication.results.mortality,
  publication.results.maori,
  publication.results.daoh.emp.logit.changepoint,
  publication.results.mort.90.changepoint,
  publication.results.loo,
  
  template.docx.filein,
  report.output.dir,
  report.docx.filein,
  page.width.inches,
  figure.dpi,
  image.output.format
)

report.docx = compile.manuscript(
  asset.list = list(
    publication.table.demographics,
    publication.figure.daoh.with.mort,
    publication.figure.daoh.with.mort.notrans,
    publication.table.rev3.comprehensive.daoh.summary,
    publication.table.rev3.comprehensive.ethnicity.daoh.summary,
    publication.table.rev3.comprehensive.maori.ethnicity.daoh.summary,
    publication.table.mort.reg,
    
    publication.figure.daoh.risk.adj,
    publication.figure.SSC.dra.riskgp.plot,
    publication.figure.SSC.dra.riskgp.group.diff.plot,
    publication.figure.ethnicity.dra.riskgp.group.diff.plot,
    publication.figure.daoh.changepoint.plot,
    publication.figure.mort.90.day.changepoint.plot,
    publication.figure.daoh.effect.size.plot,
    publication.figure.mort.90.day.effect.size.plot,
    publication.figure.mort.reg
  ), 
  template.docx.path = template.docx.filein,
  report.output.dir = report.output.dir,
  report.docx.input.path = report.docx.filein,
  page.width.inches = page.width.inches,
  figure.dpi = figure.dpi,
  image.output.format = image.output.format,
  skip.report.figure.inds = 1
)
