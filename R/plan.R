set.seed(12611000406909 %% .Machine$integer.max)

base.input.directory = 'P:/FMHSfiles/SCIENCE/CheckWHO/data/derived/encrypted_id_source'
moh.csv.directory = 'moh'
adhb.csv.directory = 'adhb'

moh.patient.dt.filename = 'rerun_CheckWHO_pus10784_cohort_IdEncrypted.csv'
moh.diag.op.dt.filename = 'rerun_CheckWHO_pus10784_diags_IdEncrypted.csv'
moh.event.dt.filename = 'rerun_CheckWHO_pus10784_events_IdEncrypted.csv'
moh.nnpac.dt.filename = 'nap0917_nap0917_IdEncrypted.csv'
adhb.event.op.patient.dt.filename = '19123 Level 8 Theatre Events_Data_IdEncrypted.csv'

# Some older events, only first event per period per patient. 
base.revision.one.input.directory = 'P:/FMHSfiles/SCIENCE/CheckWHO/data/derived/older_data/encrypted_id_source/'
adhb.revision.one.event.op.patient.dt.filename = 'indexEventsACH_IdEncrypted.csv'
moh.revision.one.patient.dt.filename = 'nhidata_MOH_IdEncrypted.csv'
moh.revision.one.event.dt.filename = 'events_MOH_IdEncrypted.csv'
figureDaohData.filename = 'figureDaohData.csv'

base.moh.lookup.directory = 'P:/FMHSfiles/SCIENCE/MOH_general/'
facility.lookup.dt.filename = 'facility/facilities20180501.csv'
icd10.am3.lookup.dt.filename = 'icd/csv_output/icd10_am3.csv'
op.lookup.dt.filename = 'icd/csv_output/opMergeTableACHI8.csv'
diag.lookup.dt.filename = 'icd/csv_output/diagnosisMergeTableACHI8.csv'
ethnicity.lookup.dt.filename = 'ethnicity/output/ethnicityMergeDT1-20190731.csv'

n.iterations.for.perm.tests = 10000
mcp.args = list(
  iter = 80000,
  adapt = 5000,
  # cores = parallel::detectCores() - 1
  cores = 1
)


checkwho_plan =
  drake_plan(
    
    template.docx.filein = 'P:/FMHSfiles/WRITING/blankTemplate.docx',
    report.output.dir = 'P:/FMHSfiles/SCIENCE/CheckWHO/reports',
    report.docx.filein = file_in('P:/FMHSfiles/SCIENCE/CheckWHO/manuscripts/Revision2/checkwho_v6.2.1.docx'),
    
    
    daoh.limits = c(0,89),
    
    min.date = as_datetime("2004-07-01"),
    max.date = as_datetime("2014-01-01") - seconds(1),
    
    pre.period.start = as_datetime("2006-05-01"),
    pre.period.end = as_datetime("2007-11-01") - seconds(1),
    
    post.period.start = as_datetime("2009-05-01"),
    post.period.end = as_datetime("2010-11-01") - seconds(1),
    
    ssc.implementation.start = as_datetime("2007-11-01"),
    ssc.implementation.end = as_datetime("2009-04-30") - seconds(1),
    
    page.width.inches = 6.5,
    figure.dpi = 1200,
    image.output.format = 'png',
      
    mortality.covariates = c(
      'gender',
      'age.group',
      # 'maori.ethnicity',
      'asa.status',
      'asa.acuity',
      # 'CCI',
      'ethnicity',
      # 'acuity',
      'icd.chapter.grouped'
    ),
    
    ineligible.chapters = c('Noninvasive, cognitive and Other Interventions, Not Elsewhere Classified',
                            'Imaging Services',
                            'Procedures on Eye and Adnexa',
                            'Radiation Oncology Procedures'),
    ineligible.procedure.strings = c('dialysis',
                                     'catheter',
                                     'shunt',
                                     'stent',
                                     'ventilatory support',
                                     'CPAP'),

    # ineligible.chapters = c(),
    # ineligible.procedure.strings = c(),
    
    
    daoh.covariates = mortality.covariates,
    
    risk.adjust.on.covariates = mortality.covariates,
    
    daoh.quantreg.quantiles = c(0.1, 0.25, 0.5, 0.75),
    
    # Model for mcp changepoint analysis. Binomial model with two changepoints
    # and autoregression throughout.
    changepoint.model.func = function(measure) {
      list(as.formula(paste0(measure, ' | trials(N) ~ 1 + month.numeric')),
           ~ 0 + month.numeric,
           ~ 0 + month.numeric)
    },
    
    changepoint.model.null.func = function(measure) {
      list(as.formula(paste0(measure, ' | trials(N) ~ 1 + month.numeric')))
    },
    
    # changepoint.measure.list = c(
    #   "Exceeds DAOH 10%" = "exceeds.daoh.10",
    #   "Exceeds DAOH 25%" = "exceeds.daoh.25",
    #   "Exceeds DAOH Median" = "exceeds.daoh.median",
    #   "Exceeds DAOH 75%" = "exceeds.daoh.75",
    #   "30-day mortality" = "mort.30.day",
    #   "90-day mortality" = "mort.90.day"
    # ),

    changepoint.measure.list = c(
      "Exceeds RA DAOH 10%" = "exceeds.daoh.risk.adj.10",
      "Exceeds RA DAOH 25%" = "exceeds.daoh.risk.adj.25",
      "Exceeds RA DAOH Median" = "exceeds.daoh.risk.adj.median",
      "Exceeds RA DAOH 75%" = "exceeds.daoh.risk.adj.75",
      "30-day mortality" = "mort.30.day",
      "90-day mortality" = "mort.90.day"
    ),
    
    # Hypotheses for the changepoint during intervention, namely 
    changepoint.during.intervention.hypothesis = c(
      # That it should be between SSC implementation start and end.
      paste0("cp_1 >= ",
             as.numeric(as.Date(ssc.implementation.start)),
             ' & cp_1 <= ',
             as.numeric(as.Date(ssc.implementation.end))),
      # That it should not.
      paste0("cp_1 < ",
             as.numeric(as.Date(ssc.implementation.start)),
             ' | cp_1 > ',
             as.numeric(as.Date(ssc.implementation.end)))), 
    
    # Hypotheses for the changepoint during intervention, namely 
    changepoint.during.intervention.or.post.hypothesis = c(
      # That it should be between SSC implementation start and end.
      paste0("cp_1 >= ",
             as.numeric(as.Date(ssc.implementation.start)),
             ' & cp_1 <= ',
             as.numeric(as.Date(ssc.implementation.end))),
      # That it should be in the post period.
      paste0("cp_1 >= ",
             as.numeric(as.Date(post.period.start)),
             ' & cp_1 <= ',
             as.numeric(as.Date(post.period.end))),
      # That it should not be in either
      paste0("cp_1 < ",
             as.numeric(as.Date(ssc.implementation.start)),
             ' | cp_1 > ',
             as.numeric(as.Date(ssc.implementation.end)))), 
    
    
    # Priors for changepoint Bayesian analysis
    prior.positive.gradient.list = list(
      # Gradient initially negative
      month.numeric_1 = "dnorm(0, 3 / (MAXX - MINX)) T(0,)"
      ,
      # Gradient greater after implementation
      month.numeric_2 = "dnorm(month.numeric_1, 3 / (MAXX - MINX)) T(month.numeric_1,)"
      ,
      # Gradient negative after
      month.numeric_3 = "dnorm(0, 3 / (MAXX - MINX)) T(0,)"
      ),
    
    prior.negative.gradient.list = list(
      # Gradient initially negative
      month.numeric_1 = "dnorm(0, 3 / (MAXX - MINX)) T(,0)"
      ,
      # Gradient greater after implementation
      month.numeric_2 = "dnorm(month.numeric_1, 3 / (MAXX - MINX)) T(,month.numeric_1)"
      ,
      # Gradient negative after
      month.numeric_3 = "dnorm(0, 3 / (MAXX - MINX)) T(,0)"
      
    ),
    
    # A default prior using Dirichlet distribution
    prior.changepoint.dirichlet = list(
      cp_1 = "dirichlet(1)",
      cp_2 = "dirichlet(1)"
    ),
    
    prior.changepoint.during.intervention.dunif.list = list(
      # Change point one between implementation start and finish
      cp_1 = paste0(
        "dunif(",
        as.numeric(as.Date(ssc.implementation.start)),
        ',',
        as.numeric(as.Date(ssc.implementation.end)),
        ")"
      )
    ),
    
    # prior.changepoint.during.intervention.dnorm.list = list(
    #   # Change point one between implementation start and finish
    #   cp_1 = paste0(
    #     "dnorm(",
    #     as.numeric(as.Date(ssc.implementation.end)),
    #     ',',
    #     (as.numeric(as.Date(ssc.implementation.end)) - as.numeric(as.Date(ssc.implementation.start)))/2,
    #     ") "
    #   )
    # ),
    # 
    
    # For restricting values of first changepoint so that they're not right at the
    # start or end.
    prior.changepoint.cropped.dunif.list = list(
      # Change point in entire period
      cp_1 = paste0(
        "dunif(MINX + 240, MAXX - 240)"
      )
    ),
    
    # Assemble hypotheses for DAOH (+gradient) and mortality (-gradient)
    changepoint.prior.dirichlet.daoh = c(
      prior.changepoint.dirichlet
      ,
      prior.positive.gradient.list
    ),
    
    changepoint.prior.dirichlet.mortality = c(
      prior.changepoint.dirichlet
      ,
      prior.negative.gradient.list
    ),

    changepoint.prior.dunif.daoh = c(
      prior.changepoint.during.intervention.dunif.list
      ,
      prior.positive.gradient.list
    ),
    
    changepoint.prior.dunif.mortality = c(
      prior.changepoint.during.intervention.dunif.list
      ,
      prior.negative.gradient.list
    ),
    
    # changepoint.prior.dnorm.daoh = c(
    #   prior.changepoint.during.intervention.dnorm.list
    #   ,
    #   prior.positive.gradient.list
    # ),
    # 
    # changepoint.prior.dnorm.mortality = c(
    #   prior.changepoint.during.intervention.dnorm.list
    #   ,
    #   prior.negative.gradient.list
    # ),
    
    changepoint.prior.gradient.change.daoh = c(
      prior.changepoint.cropped.dunif.list,
      prior.positive.gradient.list
    ),
    
    changepoint.prior.gradient.change.mortality = c(
      prior.changepoint.cropped.dunif.list,
      prior.negative.gradient.list
    ),
    
    # Put changepoints into list, one for each measure. All have gradient
    # restrictions.
    # Default Dirichlet distribution.
    changepoint.prior.dirichlet.list = list(
      changepoint.prior.dirichlet.daoh,
      changepoint.prior.dirichlet.daoh,
      changepoint.prior.dirichlet.daoh,
      changepoint.prior.dirichlet.daoh,
      changepoint.prior.dirichlet.mortality,
      changepoint.prior.dirichlet.mortality
    ),
    
    # Priors with a changepoint in the implementation period .
    changepoint.prior.dunif.list = list(
      changepoint.prior.dunif.daoh,
      changepoint.prior.dunif.daoh,
      changepoint.prior.dunif.daoh,
      changepoint.prior.dunif.daoh,
      changepoint.prior.dunif.mortality,
      changepoint.prior.dunif.mortality
    ),

    # changepoint.prior.dnorm.list = list(
    #   changepoint.prior.dnorm.daoh,
    #   changepoint.prior.dnorm.daoh,
    #   changepoint.prior.dnorm.daoh,
    #   changepoint.prior.dnorm.daoh,
    #   changepoint.prior.dnorm.mortality,
    #   changepoint.prior.dnorm.mortality
    # ),
    
    # Priors with a changepoint anywhere except the first and last few months.
    changepoint.prior.gradient.change.list = list(
      changepoint.prior.gradient.change.daoh,
      changepoint.prior.gradient.change.daoh,
      changepoint.prior.gradient.change.daoh,
      changepoint.prior.gradient.change.daoh,
      changepoint.prior.gradient.change.mortality,
      changepoint.prior.gradient.change.mortality
    ),
    
    # Construct input directories.
    moh.input.directory = file.path(base.input.directory, moh.csv.directory),
    adhb.input.directory = file.path(base.input.directory, adhb.csv.directory),
    
    # Read raw data.
    moh.patient.raw.dt = fread(file.path(moh.input.directory, moh.patient.dt.filename)),
    moh.diag.op.raw.dt = fread(file.path(moh.input.directory, moh.diag.op.dt.filename)),
    moh.event.raw.dt = fread(file.path(moh.input.directory, moh.event.dt.filename)),
    moh.nnpac.raw.dt = fread(file.path(moh.input.directory, moh.nnpac.dt.filename)),
    adhb.event.op.patient.raw.dt = fread(file.path(adhb.input.directory, adhb.event.op.patient.dt.filename)),
    
    moh.revision.one.event.raw.dt = fread(file.path(base.revision.one.input.directory, moh.revision.one.event.dt.filename)),
    moh.revision.one.patient.raw.dt = fread(file.path(base.revision.one.input.directory, moh.revision.one.patient.dt.filename)),
    adhb.revision.one.event.op.patient.raw.dt = fread(file.path(base.revision.one.input.directory, adhb.revision.one.event.op.patient.dt.filename)),
    figureDaohData = fread(file.path(base.revision.one.input.directory,figureDaohData.filename)),
    
    # Read lookup tables.
    facility.lookup.dt = fread(file.path(base.moh.lookup.directory, 
                                         facility.lookup.dt.filename)),
    op.lookup.dt = merge.data.table(x = icd10amachi::icd10achi.clinical.code.dt,
                                    y = icd10amachi::icd10achi.chapter.dt,
                                    by = c('CLIN_SYS','chapter_code'),
                                    all.x = TRUE),
    # diag.lookup.dt = fread(file.path(base.moh.lookup.directory, diag.lookup.dt.filename)),
    # icd10.am3.lookup.dt = fread(file.path(base.moh.lookup.directory, icd10.am3.lookup.dt.filename)),
    # op.lookup.dt = icd10.am3.lookup.dt[clinical.code.type == 'O', .(
    #   clinical.code.system,
    #   clinical.code.type,
    #   block, 
    #   block.description,
    #   code.procedure = as.numeric(clinical.code),
    #   desc.procedure = clinical.code.description
    # )],
    # diag.lookup.dt = icd10.am3.lookup.dt[clinical.code.type == 'O', .(
    #   clinical.code.system,
    #   clinical.code.type,
    #   code.diagnosis = clinical.code,
    #   desc.diagnosis = clinical.code.description
    # )], 
    ethnicity.lookup.dt = fread(file.path(base.moh.lookup.directory, ethnicity.lookup.dt.filename)),
    
    # Clean MOH data.
    moh.patient.dt = clean.moh.patient.dt(moh.patient.raw.dt),
    moh.diag.op.dt = clean.moh.diag.op.dt(moh.diag.op.raw.dt),
    moh.event.dt = clean.moh.event.dt(moh.event.raw.dt),
    moh.nnpac.dt = clean.moh.nnpac.dt(moh.nnpac.raw.dt), 
    
    moh.revision.one.patient.dt = clean.moh.revision.one.patient.dt(moh.revision.one.patient.raw.dt),
    adhb.revision.one.event.op.patient.dt = clean.adhb.revision.one.event.op.patient.dt(adhb.revision.one.event.op.patient.raw.dt),
    
    #Extract operations from operations, causes, and diagnoses
    moh.op.dt = generate.moh.op.dt(moh.diag.op.dt, op.lookup.dt),
    # moh.diag.dt = generate.moh.diag.dt(moh.diag.op.dt, diag.lookup.dt),
    
    # Clean ADHB data. It's all provided in one file, so kind of messy.
    adhb.event.op.patient.dt = clean.adhb.event.op.patient.dt(adhb.event.op.patient.raw.dt),
    adhb.event.dt = clean.adhb.event.dt(adhb.event.op.patient.dt),
    adhb.theatre.event.dt = clean.adhb.theatre.event.dt(adhb.event.op.patient.dt),
    adhb.op.dt = clean.adhb.op.dt(adhb.event.op.patient.dt),
    adhb.patient.dt = clean.adhb.patient.dt(adhb.event.op.patient.dt),
    
    # Get the equivalent of the older events, where no patients younger than 16
    # were provided, and only the first operation from each patient in each
    # period was taken further. 
    # The Revision One data is just used to compare, shouldn't be reused, as the
    # databases have changed since.
    adhb.revision.one.recalculated.event.op.patient.dt = generate.adhb.revision.one.recalculated.event.op.patient.dt(
      adhb.event.op.patient.dt,
      pre.period.start,
      pre.period.end,
      post.period.start,
      post.period.end,
      adhb.revision.one.event.op.patient.dt
    ),
    # adhb.revision.one.recalculated.event.op.patient.dt = clean.adhb.revision.one.recalculated.event.op.patient.dt(adhb.revision.one.recalculated.event.op.patient.raw.dt),
    adhb.revision.one.recalculated.event.dt = clean.adhb.event.dt(adhb.revision.one.recalculated.event.op.patient.dt),
    # adhb.revision.one.event.dt = clean.adhb.revision.one.recalculated.event.dt(adhb.revision.one.event.op.patient.dt),
    adhb.revision.one.recalculated.theatre.event.dt = clean.adhb.theatre.event.dt(adhb.revision.one.recalculated.event.op.patient.dt),
    adhb.revision.one.recalculated.op.dt = clean.adhb.op.dt(adhb.revision.one.recalculated.event.op.patient.dt),
    adhb.revision.one.recalculated.patient.dt = clean.adhb.patient.dt(adhb.revision.one.recalculated.event.op.patient.dt),
    
    # Get ASA from operation codes
    asa.dt = generate.asa.dt(moh.op.dt),
    
    # Get event and opdate combinations.
    event.opdate.dt = generate.event.opdate.dt(adhb.event.dt, 
                                               moh.event.dt, 
                                               moh.op.dt,
                                               asa.dt,
                                               facility.lookup.dt,
                                               ethnicity.lookup.dt,
                                               ineligible.chapters,
                                               ineligible.procedure.strings),
    
    # Also do a merge on the basis of patient and opdate, as per Revision 1.
    revision.one.recalculated.event.opdate.dt = generate.revision.one.event.opdate.dt(
      adhb.revision.one.recalculated.theatre.event.dt,
      adhb.revision.one.recalculated.event.dt,
      moh.event.dt,
      moh.op.dt,
      asa.dt,
      facility.lookup.dt,
      ethnicity.lookup.dt,
      ineligible.chapters,
      ineligible.procedure.strings,
      pre.period.start,
      pre.period.end,
      post.period.start,
      post.period.end
    ),
    
    # Generate table with variables for assessing eligibility.
    eligibility.dt = generate.eligibility.dt(
      event.opdate.dt,
      adhb.op.dt,
      moh.patient.dt,
      moh.op.dt,
      min.date,
      max.date,
      pre.period.start,
      pre.period.end,
      post.period.start,
      post.period.end
    ), 
    
    
    # Generate table with variables, but only for pre/post based on Revision One
    pre.post.eligibility.dt = generate.pre.post.eligibility.dt(
      revision.one.recalculated.event.opdate.dt,
      adhb.op.dt,
      moh.patient.dt,
      moh.op.dt,
      min.date,
      max.date,
      pre.period.start,
      pre.period.end,
      post.period.start,
      post.period.end,
      # adhb.revision.one.event.dt,
      figureDaohData
    ), 
    
    # Apply eligibility for both analyses.
    index.event.dt = generate.index.event.dt(eligibility.dt),
    pre.post.index.event.dt = generate.pre.post.index.event.dt(pre.post.eligibility.dt),
    
    # Compile the different hospitalisation data sources.
    hospitalisation.dt = generate.hospitalisation.dt(moh.event.dt, 
                                                     moh.nnpac.dt, 
                                                     moh.patient.dt),
    
    # Calculate DAOH
    daoh.dt = generate.daoh.dt(index.event.dt,
                               moh.patient.dt,
                               hospitalisation.dt,
                               daoh.limits),
    pre.post.daoh.dt = generate.daoh.dt(pre.post.index.event.dt,
                                        moh.patient.dt,
                                        hospitalisation.dt,
                                        daoh.limits), 
    
    monthly.summary.dt = generate.monthly.summary.dt(daoh.dt[time.series.eligible.and.unique == TRUE]),
    
    facility.type.summary.ht = draw.facility.type.summary.ht(
      moh.event.dt,
      facility.lookup.dt
    ),
    
    # Generate a data.table with DAOH and variables for doing regression on
    regression.dt = generate.regression.dt(daoh.dt[time.series.eligible.and.unique == TRUE |
                                                     pre.eligible.and.unique == TRUE |
                                                     post.eligible.and.unique == TRUE],
                                           moh.patient.dt,
                                           adhb.patient.dt,
                                           event.opdate.dt),
    
    pre.post.regression.dt = generate.regression.dt(pre.post.daoh.dt,
                                                    moh.patient.dt,
                                                    adhb.revision.one.recalculated.patient.dt,
                                                    revision.one.recalculated.event.opdate.dt),

    # Generate a quantile regression model for risk adjustment. The model will
    # be calculated from the larger time series data, but applied also to the
    # per/post data, which are mainly a subset.
    daoh.risk.adjust.model = generate.quantreg.model(
      regression.dt[time.series.eligible.and.unique == TRUE],
      output.variable = 'daoh',
      covariates = risk.adjust.on.covariates,
      quantiles.to.assess = seq(0.15, 0.75, by = 0.1)
    ),
    
    mort.90.risk.adjust.model = generate.regression.model(
      input.dt = regression.dt[time.series.eligible.and.unique == TRUE],
      outcome = 'mort.90.day',
      predictors = c(),
      covariates = risk.adjust.on.covariates,
      family = 'binomial'
    ),
    
    mort.30.risk.adjust.model = generate.regression.model(
      input.dt = regression.dt[time.series.eligible.and.unique == TRUE],
      outcome = 'mort.30.day',
      predictors = c(),
      covariates = risk.adjust.on.covariates,
      family = 'binomial'
    ),
    
    # Generate a data.table with risk adjusted values
    risk.adjusted.regression.dt = risk.adjust.regression.dt(
      regression.dt,
      daoh.risk.adjust.model,
      mort.90.risk.adjust.model,
      mort.30.risk.adjust.model
    ), 
    
    pre.post.risk.adjusted.regression.dt = risk.adjust.regression.dt(
      pre.post.regression.dt,
      daoh.risk.adjust.model,
      mort.90.risk.adjust.model,
      mort.30.risk.adjust.model
    ), 
    
    # Data.table for generating figures etc.
    pre.post.figure.dt = generate.pre.post.figure.dt(pre.post.risk.adjusted.regression.dt),
    time.series.figure.dt = generate.time.series.figure.dt(risk.adjusted.regression.dt,
                                                           ssc.implementation.start,
                                                           ssc.implementation.end),

    
    # Draw plots
    period.rect.plot = draw.period.rect.plot(
      pre.period.start,
      pre.period.end,
      post.period.start,
      post.period.end,
      ssc.implementation.start,
      ssc.implementation.end
    ),
    
    mortality.time.summary.dt = generate.binary.time.summary.dt(
      data.dt = time.series.figure.dt,
      time.col.name = 'daoh.period.start',
      measure.col.names = c('mort.30.day',
                            'mort.90.day'),
      round.unit = '3 month',
      ci.method = "clopper-pearson"
    ),
    
    mortality.time.plot = draw.binary.time.plot(
      mortality.time.summary.dt,
      minor.date.breaks = '3 months',
      major.date.breaks = '1 year',
      date.label.format = '%Y',
      y.breaks = seq(0, 1, 0.01),
      x.lims = NULL,
      x.title = "Year",
      y.title = NULL,
      legend.title = "Mortality",
      legend.labels.rename = c(mort.90.day = '90-day',
                               mort.30.day = '30-day'),
      draw.ci = TRUE,
      draw.smooth.line = FALSE,
      period.rect.plot = period.rect.plot),

    daoh.exceedance.time.summary.dt = generate.binary.time.summary.dt(
      data.dt = time.series.figure.dt,
      time.col.name = 'daoh.period.start',
      # measure.col.names = c('exceeds.daoh.10',
      #                       'exceeds.daoh.25',
      #                       'exceeds.daoh.median',
      #                       'exceeds.daoh.75'),
      measure.col.names = c('exceeds.daoh.risk.adj.10',
                            'exceeds.daoh.risk.adj.25',
                            'exceeds.daoh.risk.adj.median',
                            'exceeds.daoh.risk.adj.75'),
      round.unit = '3 month',
      ci.method = "clopper-pearson"),
    
    
    daoh.exceeds.75.gradient = (daoh.exceedance.time.summary.dt[time == max(time) & measure == 'exceeds.daoh.75', proportion] - 
                          daoh.exceedance.time.summary.dt[time == min(time) &  measure == 'exceeds.daoh.75', proportion]) / 
      daoh.exceedance.time.summary.dt[, as.numeric(max(time)) - as.numeric(min(time))],
    
    
    daoh.exceedance.time.plot = draw.binary.time.plot(
      daoh.exceedance.time.summary.dt,
      minor.date.breaks = '3 months',
      major.date.breaks = '1 year',
      date.label.format = '%Y',
      y.breaks = seq(0, 1, 0.05),
      x.lims = NULL,
      x.title = "Year",
      y.title = "Percentage DAOH equalling or exceeding overall percentile",
      legend.title = "Exceeds overall DAOH",
      legend.labels.rename = c(exceeds.daoh.risk.adj.10 = '10%',
                               exceeds.daoh.risk.adj.25 = '25%',
                               exceeds.daoh.risk.adj.median = '50%',
                               exceeds.daoh.risk.adj.75 = '75%'),
      draw.ci = TRUE,
      draw.smooth.line = FALSE,
      period.rect.plot = period.rect.plot),
    
    
    daoh.risk.adj.exceedance.time.summary.dt = generate.binary.time.summary.dt(
      data.dt = time.series.figure.dt,
      time.col.name = 'daoh.period.start',
      measure.col.names = c('exceeds.daoh.risk.adj.10',
                            'exceeds.daoh.risk.adj.25',
                            'exceeds.daoh.risk.adj.median',
                            'exceeds.daoh.risk.adj.75'),
      round.unit = '3 month',
      ci.method = "clopper-pearson"),
    
    
    
    
    daoh.risk.adj.exceedance.time.plot = draw.binary.time.plot(
      daoh.risk.adj.exceedance.time.summary.dt,
      minor.date.breaks = '3 months',
      major.date.breaks = '1 year',
      date.label.format = '%Y',
      y.breaks = seq(0, 1, 0.05),
      x.lims = NULL,
      x.title = "Year",
      y.title = "Percentage risk-adjusted DAOH equalling or exceeding overall percentile",
      legend.title = "Exceeds overall risk-adjusted DAOH",
      legend.labels.rename = c(exceeds.daoh.risk.adj.10 = '10%',
                               exceeds.daoh.risk.adj.25 = '25%',
                               exceeds.daoh.risk.adj.median = '50%',
                               exceeds.daoh.risk.adj.75 = '75%'),
      draw.ci = TRUE,
      draw.smooth.line = FALSE,
      period.rect.plot = period.rect.plot),
    
    daoh.quantile.summary.dt = generate.quantile.time.summary.dt(
      data.dt = time.series.figure.dt,
      time.col.name = 'daoh.period.start',
      measure.col.names = 'daoh',
      probs = c(0.1, 0.25, 0.5, 0.75),
      round.unit = '3 month'
    ),
    
    daoh.time.plot = draw.quantile.time.plot(
      quantile.time.summary.dt = daoh.quantile.summary.dt,
      minor.date.breaks = '3 months',
      major.date.breaks = '1 year',
      date.label.format = '%Y',
      y.breaks = seq(0, 90, 10),
      x.lims = NULL,
      x.title = "Year",
      y.title = "DAOH",
      draw.ci = TRUE,
      draw.smooth.line = FALSE,
      period.rect.plot = period.rect.plot
    ),
    
    daoh.risk.adj.quantile.summary.dt = generate.quantile.time.summary.dt(
      data.dt = time.series.figure.dt,
      time.col.name = 'daoh.period.start',
      measure.col.names = 'daoh.risk.adj',
      probs = c(0.1, 0.25, 0.5, 0.75),
      round.unit = '3 month'
    ),
    
    daoh.risk.adj.time.plot = draw.quantile.time.plot(
      quantile.time.summary.dt = daoh.risk.adj.quantile.summary.dt,
      minor.date.breaks = '3 months',
      major.date.breaks = '1 year',
      date.label.format = '%Y',
      y.breaks = seq(0, 130, 10),
      y.lims = c(0,130),
      x.lims = NULL,
      x.title = "Year",
      y.title = "DAOH (Risk-adjusted)",
      draw.ci = TRUE,
      draw.smooth.line = FALSE,
      period.rect.plot = period.rect.plot
    ),
    
    daoh.risk.adj.pre.post.plot = plot.daoh.density(
      input.dt = pre.post.figure.dt,
      by.group = 'SSC',
      daoh.col.name = 'daoh.risk.adj',
      xlimits = pre.post.figure.dt[, c(min(daoh.risk.adj) - 0.5,
                                       max(daoh.risk.adj) + 0.5)]
    ),
    
    daoh.pre.post.plot = plot.daoh.density(
      input.dt = pre.post.figure.dt,
      by.group = 'SSC',
      daoh.col.name = 'daoh'
    ),
    
    rename.coefficients.list = c(
      'Constant' = '(Intercept)',
      'Post-SSC (vs Pre-SSC)' = 'SSCPost',
      'Female (vs Male)' = 'genderFemale',
      'Acute (vs Not acute or unknown)' = 'asa.acuityAcute',
      '34-48yo (vs 16-33)' = 'age.group34-48',
      '49-64yo (vs 16-33)' = 'age.group49-64',
      '65-78yo (vs 16-33)' = 'age.group65-78',
      '79+yo (vs 16-33)' = 'age.group79+',
      'ASA2 (vs ASA1)' = 'asa.statusASA 2',
      'ASA3 (vs ASA1)' = 'asa.statusASA 3',
      'ASA4-5 (vs ASA1)' = 'asa.statusASA 4-5',
      # "M\u101ori (vs non-M\u101ori)" = 'maori.ethnicityTRUE',
      "European (vs M\u101ori)" = 'ethnicityEuropean',
      "Asian (vs M\u101ori)" = 'ethnicityAsian',
      "Pacific Peoples (vs M\u101ori)" = 'ethnicityPacific Peoples',
      "Other ethnicity (vs M\u101ori)" = 'ethnicityOther',
      'Digestive system (vs Ortho)' = 'icd.chapter.groupedProcedures on Digestive System',
      'Urinary system (vs Ortho)' = 'icd.chapter.groupedProcedures on Urinary System',
      'Nervous system (vs Ortho)' = 'icd.chapter.groupedProcedures on Nervous System',
      'Cardiovascular system (vs Ortho)' = 'icd.chapter.groupedProcedures on Cardiovascular System',
      'Dermatological and plastic (vs Ortho)' = 'icd.chapter.groupedDermatological and Plastic Procedures',
      'Male genital organs (vs Ortho)' = 'icd.chapter.groupedProcedures on Male Genital Organs',
      'Other (vs Ortho)' = 'icd.chapter.groupedOther'
    ),
    
    rename.interaction.coefficients.list = c(
      'SSC * Gastro' = 'SSCPost:icd.chapter.groupedProcedures on Digestive System',
      'SSC * Uro' = 'SSCPost:icd.chapter.groupedProcedures on Urinary System',
      'SSC * Neuro' = 'SSCPost:icd.chapter.groupedProcedures on Nervous System',
      'SSC * Cardio' = 'SSCPost:icd.chapter.groupedProcedures on Cardiovascular System',
      'SSC * Derma' = 'SSCPost:icd.chapter.groupedDermatological and Plastic Procedures',
      'SSC * Andro' = 'SSCPost:icd.chapter.groupedProcedures on Male Genital Organs',
      'SSC * Other' = 'SSCPost:icd.chapter.groupedOther',
      'SSC * Acute' = 'SSCPost:asa.acuityAcute',
      'SSC * 34-48yo' = 'SSCPost:age.group34-48',
      'SSC * 49-64yo' = 'SSCPost:age.group49-64',
      'SSC * 65-78yo' = 'SSCPost:age.group65-78',
      'SSC * 79+yo' = 'SSCPost:age.group79+'
    ), 
    
    group.coefficients.list = list(
      group.1 = c('SSCPost',
                  'genderFemale',
                  # 'maori.ethnicityTRUE',
                  'ethnicityEuropean',
                  'ethnicityAsian',
                  'ethnicityPacific Peoples',
                  'ethnicityOther'),
      group.2 = c('acuityAcute',
                  'age.group34-48',
                  'age.group49-64',
                  'age.group65-78',
                  'age.group79+'),
      group.3 = c('asa.statusASA 2',
                  'asa.statusASA 3',
                  'asa.statusASA 4-5'),
      group.4 = c('icd.chapter.groupedProcedures on Digestive System',
                  'icd.chapter.groupedProcedures on Urinary System',
                  'icd.chapter.groupedProcedures on Nervous System',
                  'icd.chapter.groupedProcedures on Cardiovascular System',
                  'icd.chapter.groupedDermatological and Plastic Procedures',
                  'icd.chapter.groupedProcedures on Male Genital Organs',
                  'icd.chapter.groupedOther')
    ),
    
    group.interaction.coefficients.list = list(
      group.5 = c('SSCPost:icd.chapter.groupedProcedures on Digestive System',
                  'SSCPost:icd.chapter.groupedProcedures on Urinary System',
                  'SSCPost:icd.chapter.groupedProcedures on Nervous System',
                  'SSCPost:icd.chapter.groupedProcedures on Cardiovascular System',
                  'SSCPost:icd.chapter.groupedDermatological and Plastic Procedures',
                  'SSCPost:icd.chapter.groupedProcedures on Male Genital Organs',
                  'SSCPost:icd.chapter.groupedOther',
                  'SSCPost:asa.acuityAcute',
                  'SSCPost:age.group34-48',
                  'SSCPost:age.group49-64',
                  'SSCPost:age.group65-78',
                  'SSCPost:age.group79+')
    ),
    
    # Mortality regression
    mort.30.regression.model.initial = generate.regression.model(
      input.dt = pre.post.figure.dt,
      outcome = 'mort.30.day',
      covariates = mortality.covariates,
      family = 'binomial'
    ),
    
    mort.90.regression.model.initial = generate.regression.model(
      input.dt = pre.post.figure.dt,
      outcome = 'mort.90.day',
      covariates = mortality.covariates,
      family = 'binomial'
    ),
    
    # Add each covariate's interaction with SSC and do a likelihood ratio test
    # to determine whether it should be added to the overall model.
    mort.interaction.test.dt = generate.interaction.test.dt(
      models = list(mort.30.regression.model.initial,
                    mort.90.regression.model.initial)
    ),
    
    final.mortality.covariates = c(mortality.covariates, unique(mort.interaction.test.dt[pvalue.fdr < 0.05, interaction.term])),
    
    mort.30.regression.model = generate.regression.model(
      input.dt = pre.post.figure.dt,
      outcome = 'mort.30.day',
      covariates = final.mortality.covariates,
      family = 'binomial'
    ),
    
    mort.90.regression.model = generate.regression.model(
      input.dt = pre.post.figure.dt,
      outcome = 'mort.90.day',
      covariates = final.mortality.covariates,
      family = 'binomial'
    ),
    
    maori.mortality.covariates = c(
      final.mortality.covariates[!(
        final.mortality.covariates %ilike% 'ethnicity' |
          final.mortality.covariates %ilike% 'ssc'
      )],
      'maori.ethnicity'),
    
    # Remove all ethnicity terms, and replace with one for Maori ethnicity.
    mort.30.maori.regression.model = generate.regression.model(
      input.dt = time.series.figure.dt,
      outcome = 'mort.30.day',
      predictors = NULL,
      covariates = maori.mortality.covariates, 
      family = 'binomial'
    ),
    mort.90.maori.regression.model = generate.regression.model(
      input.dt = time.series.figure.dt,
      outcome = 'mort.90.day',
      predictors = NULL,
      covariates = maori.mortality.covariates,
      family = 'binomial'
    ),
    
    mort.30.regression.plot = draw.binary.regression.plot(
      models = mort.30.regression.model,
      coefs = rename.coefficients.list,
      groups = group.coefficients.list),

    mort.90.regression.plot = draw.regression.plot(
      models = mort.90.regression.model,
      coefs = rename.coefficients.list,
      groups = group.coefficients.list),

    
    mort.regression.plot = draw.regression.plot(
      models = list(mort.30.regression.model, mort.90.regression.model),
      model.names = c('30-day mortality',
                      '90-day mortality'),
      coefs = rename.coefficients.list,
      groups = group.coefficients.list
    ),
    
   
    mort.regression.table = draw.regression.table(
      models = list(mort.30.regression.model,
                    mort.90.regression.model),
      model.names = c('30-day mortality',
                      '90-day mortality'),
      coefs = rename.coefficients.list),
    
    
    mort.maori.regression.table = draw.regression.table(
      models = list(mort.30.maori.regression.model,
                    mort.90.maori.regression.model),
      model.names = c('30-day mortality',
                      '90-day mortality')),
  
    mort.regression.abbreviated.plot = draw.binary.regression.plot(
      models = list(mort.30.regression.model,
                    mort.90.regression.model),
      coefs = c('SSC (vs Pre-SSC)' = 'SSCPost'),
      model.names = c('30-day mortality',
                      '90-day mortality'),
      ylabs = FALSE),
    
    # DAOH regression stuff
    daoh.regression.models.initial = generate.quantile.regression.models(
      input.dt = pre.post.figure.dt,
      outcome = 'daoh',
      predictors = 'SSC',
      covariates = daoh.covariates,
      tau = daoh.quantreg.quantiles
    ),
    
    # Add each covariate's interaction with SSC and do a Wald test to determine
    # whether it should be added to the overall model.
    daoh.interaction.test.dt = generate.interaction.test.dt(
      models = daoh.regression.models.initial
    ),
    
    final.daoh.covariates = c(daoh.covariates, unique(daoh.interaction.test.dt[pvalue.fdr < 0.05, interaction.term])),
    
    # DAOH regression, but with any interaction terms.
    daoh.regression.models = generate.quantile.regression.models(
      input.dt = pre.post.figure.dt,
      outcome = 'daoh',
      predictors = 'SSC',
      covariates = final.daoh.covariates,
      tau = daoh.quantreg.quantiles
    ),
    
    # Remove all ethnicity terms, and replace with one for Maori ethnicity.
    maori.daoh.covariates = c(
      final.daoh.covariates[!(
        final.daoh.covariates %ilike% 'ethnicity' |
          final.daoh.covariates %ilike% 'ssc'
      )],
      'maori.ethnicity'),
    
    daoh.maori.regression.models = generate.quantile.regression.models(
      input.dt = time.series.figure.dt,
      outcome = 'daoh',
      predictors = NULL,
      covariates = maori.daoh.covariates,
      tau = daoh.quantreg.quantiles
    ),
    
    daoh.maori.regression.table = draw.regression.table(
      models = daoh.maori.regression.models,
      model.names = scales::percent(daoh.quantreg.quantiles)
    ),
      
    daoh.regression.table = draw.regression.table(
      models = daoh.regression.models,
      model.names = scales::percent(daoh.quantreg.quantiles),
      coefs = c(rename.coefficients.list, rename.interaction.coefficients.list)
    ),
    
    daoh.regression.abbreviated.plot = draw.regression.plot(
      models = daoh.regression.models,
      coefs = c('SSC (vs Pre-SSC)' = 'SSCPost'),
      model.names = scales::percent(daoh.quantreg.quantiles),
      xlab= 'DAOH',
      legend.title = 'DAOH percentile',
      ylabs = FALSE
    ),
    daoh.regression.plot = draw.regression.plot(
      models = daoh.regression.models,
      coefs = c(rename.coefficients.list, rename.interaction.coefficients.list),
      model.names = scales::percent(daoh.quantreg.quantiles),
      groups = c(group.coefficients.list,group.interaction.coefficients.list),
      xlab= 'DAOH',
      legend.title = 'DAOH percentile'
    ),
    # A DAOH plot with mortal cases highlighted
    daoh.mortality.plot = draw.daoh.mortality.plot(
      input.dt = time.series.figure.dt
    ),
    
    # demographic.table.html = draw.demographic.table.html(
    #   pre.post.figure.dt,
    #   time.series.figure.dt
    # ),
    
    demographic.table.gtsummary = draw.demographic.table.gtsummary(
      pre.post.figure.dt,
      time.series.figure.dt
    ),
    
    #A DAOH table with means and quantiles, all compared.
    comprehensive.daoh.summary.ht =
      draw.comprehensive.summ.ht(pre.post.figure.dt,
                                 time.series.figure.dt,
                                 n.iterations = n.iterations.for.perm.tests),
    
    # Changepoint models
    # changepoint.model.list = generate.changepoint.model.list(
    #   time.series.figure.dt,
    #   changepoint.measure.list,
    #   ssc.implementation.start,
    #   ssc.implementation.end,
    #   changepoint.model.func = changepoint.model.func,
    #   adapt = mcp.args$adapt,
    #   iter = mcp.args$iter,
    #   cores = mcp.args$cores
    # ), 
    
    changepoint.model.with.prior.dirichlet.list = generate.changepoint.model.list(
      time.series.figure.dt,
      changepoint.measure.list,
      ssc.implementation.start,
      ssc.implementation.end,
      changepoint.model.func = changepoint.model.func,
      changepoint.prior.list = changepoint.prior.dirichlet.list,
      adapt = mcp.args$adapt,
      iter = mcp.args$iter,
      cores = mcp.args$cores
    ), 
    
    changepoint.model.with.prior.dunif.list = generate.changepoint.model.list(
      time.series.figure.dt,
      changepoint.measure.list,
      ssc.implementation.start,
      ssc.implementation.end,
      changepoint.prior.dunif.list,
      adapt = mcp.args$adapt,
      iter = mcp.args$iter,
      cores = mcp.args$cores
    ),
    
    # changepoint.model.with.prior.dnorm.list = generate.changepoint.model.list(
    #   time.series.figure.dt,
    #   changepoint.measure.list,
    #   ssc.implementation.start,
    #   ssc.implementation.end,
    #   changepoint.prior.dnorm.list,
    #   changepoint.model.func = changepoint.model.func,
    #   adapt = mcp.args$adapt,
    #   iter = mcp.args$iter
    # ), 
    
    changepoint.model.with.prior.gradient.change.list = generate.changepoint.model.list(
      time.series.figure.dt,
      changepoint.measure.list,
      ssc.implementation.start,
      ssc.implementation.end,
      changepoint.prior.list = changepoint.prior.gradient.change.list,
      changepoint.model.func = changepoint.model.func,
      adapt = mcp.args$adapt,
      iter = mcp.args$iter,
      cores = mcp.args$cores
    ), 
    
    changepoint.model.null.list = generate.changepoint.model.list(
      time.series.figure.dt,
      changepoint.measure.list,
      ssc.implementation.start,
      ssc.implementation.end,
      changepoint.prior.list = NULL,
      changepoint.model.func = changepoint.model.null.func,
      adapt = mcp.args$adapt,
      iter = mcp.args$iter,
      cores = mcp.args$cores
    ), 
    
    # Graphs of plots in changepoint model lists
    # changepoint.plot = draw.changepoint.plot(changepoint.model.list,
    #                                          names(changepoint.measure.list),
    #                                          period.rect.plot),
    
    changepoint.model.with.prior.dirichlet.plot = draw.changepoint.plot(
      changepoint.model.with.prior.dirichlet.list,
      names(changepoint.measure.list),
      period.rect.plot
    ),
    
    changepoint.with.prior.dunif.plot = draw.changepoint.plot(
      changepoint.model.with.prior.dunif.list,
      names(changepoint.model.with.prior.dunif.list),
      period.rect.plot
    ),
    
    # changepoint.with.prior.dnorm.plot = draw.changepoint.plot(
    #   changepoint.model.with.prior.dnorm.list,
    #   names(changepoint.measure.list),
    #   period.rect.plot
    # ),
    
    changepoint.model.with.prior.gradient.change.plot = draw.changepoint.plot(
      changepoint.model.with.prior.gradient.change.list,
      names(changepoint.measure.list),
      period.rect.plot
    ),
    
    # Testing hypothesis that there is a change point between implementation
    # start and implementation end.
    changepoint.after.pre.hypothesis.testing.list =
      generate.changepoint.hypothesis.testing.list(
        hypothesis = changepoint.during.intervention.or.post.hypothesis,      
        changepoint.model.list = changepoint.model.with.prior.gradient.change.list
      ),
    
    # changepoint.during.intervention.hypothesis.testing.list =
    #   generate.changepoint.hypothesis.testing.list(
    #     hypothesis = changepoint.during.intervention.hypothesis,      
    #     changepoint.model.list = changepoint.model.with.prior.dunif.list
    #   ),
    
    # Test the fit of the model with prior of uniform likelihood of intervention
    # during implementation vs one with no change points at all.
    # loo.testing.list = generate.loo.testing.list(
    #   changepoint.model.null.list,
    #   # changepoint.model.with.prior.dunif.list,
    #   changepoint.model.with.prior.gradient.change.list
    #   # changepoint.model.with.prior.dnorm.list,
    #   # changepoint.model.with.prior.dirichlet.list
    # ),
    # 
    # # Generate a table for presenting LOO results.
    # loo.testing.ht = draw.loo.testing.ht(loo.testing.list,
    #                                      changepoint.measure.list), 
    
    # Generate a table for the Bayesian hypothesis testing results.
    bayes.testing.ht = draw.bayes.hypothesis.testing.ht(
      hypothesis.testing.list = changepoint.after.pre.hypothesis.testing.list,
      changepoint.measure.list,
      changepoint.model.with.prior.gradient.change.list
    ),
    

    eligibility.figure.numbers.dt = generate.eligibility.figure.numbers.dt(
      eligibility.dt,
      eligibility.factor.vector = c(
        "unique.moh.event.opdate",
        "matched.moh.patient",
        "after.min.date",
        "before.max.date",
        "age.older.than.16",
        "recorded.ethnicity",
        # "has.operation.adhb",
        "has.operation.moh",
        "eligible.procedure",
        "overlapping.op.and.admission",
        # "first.operation.adhb",
        "has.asa",
        "not.asa.6",
        "alive.on.op.date"
      )
    ),
    
    pre.post.eligibility.figure.numbers.dt = generate.pre.post.eligibility.figure.numbers.dt(
      pre.post.eligibility.dt,
      eligibility.factor.vector = c(
        "unique.moh.event.opdate",
        "matched.moh.patient",
        # "after.min.date",
        # "before.max.date",
        "age.older.than.16",
        "recorded.ethnicity",
        # "has.operation.adhb",
        "has.operation.moh",
        # "eligible.procedure",
        "overlapping.op.and.admission",
        # "first.operation.adhb",
        "has.asa",
        "not.asa.6",
        "alive.on.op.date"
      )
    ),
    
    publication.table.demographics = generate.publication.table(
      name = 'groupSummary',
      input.table = demographic.table.gtsummary,
      caption = paste0(
        'Demographic summaries for Pre-SSC, Post-SSC, and Extended periods. '
      )
    ),
    
    publication.figure.daoh.with.mort = generate.publication.figure(
      name = 'rawPlot',
      input.plot = daoh.mortality.plot,
      caption = paste0(
        "Distribution of DAOH\u2089\u2080 for Extended period (light grey). ",
        "Overlaid in dark grey is the distribution for procedures where the patient died during the 90-day follow-up period. ",
        "Note square root transform on y-axis."
      ),
      aspect.ratio = 1.5
    ),
    
    publication.table.comprehensive.daoh.summary = generate.publication.table(
      name = 'daohTab',
      input.table = comprehensive.daoh.summary.ht,
      caption = paste0("DAOH\u2089\u2080 for the three periods, both unadjusted and risk-adjusted, and the differences between the Pre-SSC and Post-SSC groups. ",
                       "Differences in the overall DAOH\u2089\u2080 distribution were assessed using Wilcoxon-Mann-Whitney U tests. ",
                       "Differences in other values were assessed using absolute difference. ",
                       "P-values were generated using permutation tests with ",
                       format(n.iterations.for.perm.tests, big.mark = ','),
                       " permutations. ")
    ),
    
    publication.figure.daoh.risk.adj = generate.publication.figure(
      name = 'daohGroupPlotRiskAdj',
      input.plot = daoh.risk.adj.pre.post.plot,
      caption = paste0("Distribution of risk-adjusted DAOH\u2089\u2080 for both Pre-SSC and Post-SSC periods. ",
                       "Scores from each group are transposed in histograms, with probability density curves overlaid. ",
                       "Note square root transform of y-axis, to facilitate comparisons at intermediate DAOH\u2089\u2080 values."),
      aspect.ratio = 1.5
    ),
    
    publication.figure.daoh.raw = generate.publication.figure(
      name = 'daohGroupPlotRaw',
      input.plot = daoh.pre.post.plot,
      caption = paste0("Distribution of unadjusted DAOH\u2089\u2080 for both Pre-SSC and Post-SSC periods. ",
                       "Scores from each group are transposed in histograms, with probability density curves overlaid. ",
                       "Note square root transform of y-axis, to facilitate comparisons at intermediate DAOH\u2089\u2080 values."),
      aspect.ratio = 1.5
    ),
    
    publication.figure.mort.reg = generate.publication.figure(
      name = 'mortReg',
      input.plot = mort.regression.plot,
      caption = paste0("Odds ratios of effects in a logit regression model for 30-day and 90-day mortality. ",
                       "Thin and thick bars respectively indicate 95% and 90% confidence intervals. ",
                       "Predictors have been divided into separate odds ratio axes, as the effects are on different scales."),
      aspect.ratio = 0.5
    ),
    
    publication.figure.mort.reg.abridged = generate.publication.figure(
      name = 'mortRegSSC',
      input.plot = mort.regression.abbreviated.plot,
      caption = paste0(
        "Odds ratios of 30- and 90-day mortality for Post-SSC vs Pre-SSC periods in a multivariate logistic regression model. ",
        "Thin and thick bars respectively indicate 95% and 90% confidence intervals. ",
        "Full models are presented in TAB_mortReg_REF and FIG_mortReg_REF."
      ),
      aspect.ratio = 4
    ),
    
    publication.table.mort.reg = generate.publication.table(
      name = 'mortReg',
      input.table = mort.regression.table,
      caption = paste0("Logit regression models for 30-day and 90-day mortality (effect size and 95% CI).")
    ),
    
    publication.figure.daoh.reg = generate.publication.figure(
      name = 'daohReg',
      input.plot = daoh.regression.plot,
      caption = paste0("Quantile regression model for DAOH\u2089\u2080, fitted at 0.1, 0.25, 0.5, and 0.75 quantiles. ",
                       "Thin and thick bars respectively indicate 95% and 90% confidence intervals. ",
                       "Predictors have been divided into separate axes, as the effects are on different scales."),
      aspect.ratio = 0.5
    ),
    
    publication.figure.daoh.reg.abridged = generate.publication.figure(
      name = 'daohRegSSC',
      input.plot = daoh.regression.abbreviated.plot,
      caption = paste0(
        "DAOH\u2089\u2080 Pre-SSC vs Post-SSC periods in a multivariate quantile regression model, fitted at 0.1, 0.25, 0.5, and 0.75 quantiles. ",
        "Thin and thick bars respectively indicate 95% and 90% confidence intervals. ",
        "Full models are presented in TAB_daohReg_REF and FIG_daohReg_REF."
      ),
      aspect.ratio = 4
    ),
    
    publication.table.daoh.reg = generate.publication.table(
      name = 'daohReg',
      input.table = daoh.regression.table,
      caption = paste0("Quantile regression models for DAOH\u2089\u2080, fitted at 0.1, 0.25, 0.5, and 0.75 quantiles (effect size and 95% CI).")
    ),
    
    publication.table.facility.type.summary = generate.publication.table(
      name = 'facilityTypeCounts',
      input.table = facility.type.summary.ht,
      caption = paste0("Types of facility to which patients were admitted.")
    ),
    
    publication.figure.daoh.exceedance.time.plot = generate.publication.figure(
      name = 'daohTimePlot',
      input.plot = daoh.exceedance.time.plot,
      caption = paste0(
        "Percentage of patients per quarter with risk-adjusted DAOH\u2089\u2080 exceeding specified quantiles. ",
        "Preregistered Pre-SSC and post-SSC periods are shaded red and blue respectively, implementation period is shaded green."
      ),
      aspect.ratio = 1
    ),
    
    publication.figure.mortality.time.plot = generate.publication.figure(
      name = 'mortTimePlot',
      input.plot = mortality.time.plot,
      caption = paste0(
        "Mortality (30-day and 90-day) per quarter. ",
        "Preregistered Pre-SSC and post-SSC periods are shaded red and blue respectively, implementation period is shaded green."
      ),
      aspect.ratio = 1.5
    ),
    
    
    publication.figure.changepoint.model.with.prior.gradient.change.plot = generate.publication.figure(
      name = 'changepointWithPriorGradient',
      input.plot = changepoint.model.with.prior.gradient.change.plot,
      caption = paste0("Changepoint models on time-series data as generated by mcp, with prior of two changepoints, with positive gradients. ",
                       "Percentages of patients equalling or exceeding various quantiles (percentiles) of overall risk-adjusted DAOH, and mortality. ",
                       "Density of changepoint estimates is denoted by curves at the bottom of each plot."),
      aspect.ratio = 0.5
    ), 
    
    # publication.table.loo.testing = generate.publication.table(
    #   name = 'loo',
    #   input.table = loo.testing.ht,
    #   caption = paste0("Model weights generated by model averaging via stacking for a model with an upwards inflection in patient outcomes during the SSC implementation period, and a mocel with no changepoint at all.")
    # ),
    
    publication.table.bayes.testing = generate.publication.table(
      name = 'bayes',
      input.table = bayes.testing.ht,
      caption = paste0(
        "Results of Bayesian changepoint analysis, including ",
        "point estimate of first changepoint and 95% credible intervals, plus ",
        "Bayes factors (BFs) comparing the hypothesis that the first changepoint was during the SSC implementation period, to that in which it was outside. "
      )
    ), 
    
    publication.results.demographics = list(
      adhb.event.op.patient.raw.dt[`Actual Into Theatre Date Time` > min.date & `Actual Into Theatre Date Time` < max.date,.N],
      n.pre = pre.post.eligibility.dt[pre.eligible.and.unique == TRUE, .N],
      n.post = pre.post.eligibility.dt[post.eligible.and.unique == TRUE, .N],
      n.pre.post = pre.post.eligibility.dt[pre.post.unique == TRUE, .N],
      n.time.series = time.series.figure.dt[,.N],
      p.maori = time.series.figure.dt[,.N, by = maori.ethnicity][,p := N/sum(N)]
      
    ),
    
    publication.results.daoh = list(
      pre.post.riskadj.comparison.w = wilcox.test(daoh.risk.adj ~ SSC, data = pre.post.figure.dt)$statistic,
      pre.post.riskadj.comparison.p = as.numeric(comprehensive.daoh.summary.ht[11,6]),
      
      pre.post.perm.10.diff = as.numeric(comprehensive.daoh.summary.ht[15,5]),
      pre.post.perm.10.p = as.numeric(comprehensive.daoh.summary.ht[15,6]),
      pre.post.perm.25.diff = as.numeric(comprehensive.daoh.summary.ht[16,5]),
      pre.post.perm.25.p = as.numeric(comprehensive.daoh.summary.ht[16,6]),
      pre.post.perm.50.diff = as.numeric(comprehensive.daoh.summary.ht[17,5]),
      pre.post.perm.50.p = as.numeric(comprehensive.daoh.summary.ht[17,6]),
      pre.post.perm.75.diff = as.numeric(comprehensive.daoh.summary.ht[18,5]),
      pre.post.perm.75.p = as.numeric(comprehensive.daoh.summary.ht[18,6]),
      pre.post.perm.90.diff = as.numeric(comprehensive.daoh.summary.ht[19,5]),
      pre.post.perm.90.p = as.numeric(comprehensive.daoh.summary.ht[19,6]),
      
      final.daoh.covariates = final.daoh.covariates,
      
      pre.post.qr.10.diff = summary(daoh.regression.models[[1]])$coefficients["SSCPost", "Value"],
      pre.post.qr.10.low = summary(daoh.regression.models[[1]])$coefficients["SSCPost", "Value"] -
        1.96 * summary(daoh.regression.models[[1]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.10.high = summary(daoh.regression.models[[1]])$coefficients["SSCPost", "Value"] +
        1.96 * summary(daoh.regression.models[[1]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.10.p = summary(daoh.regression.models[[1]])$coefficients["SSCPost", "Pr(>|t|)"],
      
      pre.post.qr.25.diff = summary(daoh.regression.models[[2]])$coefficients["SSCPost", "Value"],
      pre.post.qr.25.low = summary(daoh.regression.models[[2]])$coefficients["SSCPost", "Value"] -
        1.96 * summary(daoh.regression.models[[2]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.25.high = summary(daoh.regression.models[[2]])$coefficients["SSCPost", "Value"] +
        1.96 * summary(daoh.regression.models[[2]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.25.p = summary(daoh.regression.models[[2]])$coefficients["SSCPost", "Pr(>|t|)"],
      
      pre.post.qr.50.diff = summary(daoh.regression.models[[3]])$coefficients["SSCPost", "Value"],
      pre.post.qr.50.low = summary(daoh.regression.models[[3]])$coefficients["SSCPost", "Value"] -
        1.96 * summary(daoh.regression.models[[3]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.50.high = summary(daoh.regression.models[[3]])$coefficients["SSCPost", "Value"] +
        1.96 * summary(daoh.regression.models[[3]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.50.p = summary(daoh.regression.models[[3]])$coefficients["SSCPost", "Pr(>|t|)"],
      
      pre.post.qr.75.diff = summary(daoh.regression.models[[4]])$coefficients["SSCPost", "Value"],
      pre.post.qr.75.low = summary(daoh.regression.models[[4]])$coefficients["SSCPost", "Value"] -
        1.96 * summary(daoh.regression.models[[4]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.75.high = summary(daoh.regression.models[[4]])$coefficients["SSCPost", "Value"] +
        1.96 * summary(daoh.regression.models[[4]])$coefficients["SSCPost", "Std. Error"],
      pre.post.qr.75.p = summary(daoh.regression.models[[4]])$coefficients["SSCPost", "Pr(>|t|)"]
      
    ),
    
    publication.results.mortality = list(
      mort.90.day.pre = pre.post.figure.dt[SSC == 'Pre',.SD[,.N,by = mort.90.day][mort.90.day==TRUE,N]/.N][1],
      mort.90.day.post = pre.post.figure.dt[SSC == 'Post',.SD[,.N,by = mort.90.day][mort.90.day==TRUE,N]/.N][1],
      
      mort.30.day.pre = pre.post.figure.dt[SSC == 'Pre',.SD[,.N,by = mort.30.day][mort.30.day==TRUE,N]/.N][1],
      mort.30.day.post = pre.post.figure.dt[SSC == 'Post',.SD[,.N,by = mort.30.day][mort.30.day==TRUE,N]/.N][1],

      final.mortality.covariates = final.mortality.covariates,
      
      pre.post.mort.90.reg.or = exp(summary(mort.90.regression.model)$coefficients["SSCPost", "Estimate"]),
      pre.post.mort.90.reg.or.low = exp(
        summary(mort.90.regression.model)$coefficients["SSCPost", "Estimate"] -
          1.96 * summary(mort.90.regression.model)$coefficients["SSCPost", "Std. Error"]
      ),
      pre.post.mort.90.reg.or.high = exp(
        summary(mort.90.regression.model)$coefficients["SSCPost", "Estimate"] +
          1.96 * summary(mort.90.regression.model)$coefficients["SSCPost", "Std. Error"]
      ),
      pre.post.mort.90.reg.or.p = summary(mort.90.regression.model)$coefficients["SSCPost", "Pr(>|z|)"],
      
      
      pre.post.mort.30.reg.or = exp(summary(mort.30.regression.model)$coefficients["SSCPost", "Estimate"]),
      pre.post.mort.30.reg.or.low = exp(
        summary(mort.30.regression.model)$coefficients["SSCPost", "Estimate"] -
          1.96 * summary(mort.30.regression.model)$coefficients["SSCPost", "Std. Error"]
      ),
      pre.post.mort.30.reg.or.high = exp(
        summary(mort.30.regression.model)$coefficients["SSCPost", "Estimate"] +
          1.96 * summary(mort.30.regression.model)$coefficients["SSCPost", "Std. Error"]
      ),
      pre.post.mort.30.reg.or.p = summary(mort.30.regression.model)$coefficients["SSCPost", "Pr(>|z|)"]
    
      
      ),
    
    publication.results.maori = list(
      maori.mort.30.day = time.series.figure.dt[maori.ethnicity == TRUE,
                                                binom.test(x = .SD[, .N, by = mort.30.day][mort.30.day == TRUE, N], 
                                                           n = .N)][c('estimate', 'conf.int')], 
      maori.mort.90.day = time.series.figure.dt[maori.ethnicity == TRUE,
                                                binom.test(x = .SD[, .N, by = mort.90.day][mort.90.day == TRUE, N], 
                                                           n = .N)][c('estimate', 'conf.int')], 
      nonmaori.mort.30.day = time.series.figure.dt[maori.ethnicity == FALSE,
                                                   binom.test(x = .SD[, .N, by = mort.30.day][mort.30.day == TRUE, N], 
                                                              n = .N)][c('estimate', 'conf.int')], 
      nonmaori.mort.90.day = time.series.figure.dt[maori.ethnicity == FALSE,
                                                   binom.test(x = .SD[, .N, by = mort.90.day][mort.90.day == TRUE, N], 
                                                              n = .N)][c('estimate', 'conf.int')],
      
      
      maori.qr.10.diff = summary(daoh.maori.regression.models[[1]])$coefficients["maori.ethnicityTRUE", "Value"],
      maori.qr.10.low = summary(daoh.maori.regression.models[[1]])$coefficients["maori.ethnicityTRUE", "Value"] -
        1.96 * summary(daoh.maori.regression.models[[1]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.10.high = summary(daoh.maori.regression.models[[1]])$coefficients["maori.ethnicityTRUE", "Value"] +
        1.96 * summary(daoh.maori.regression.models[[1]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.10.p = summary(daoh.maori.regression.models[[1]])$coefficients["maori.ethnicityTRUE", "Pr(>|t|)"],
      
      maori.qr.25.diff = summary(daoh.maori.regression.models[[2]])$coefficients["maori.ethnicityTRUE", "Value"],
      maori.qr.25.low = summary(daoh.maori.regression.models[[2]])$coefficients["maori.ethnicityTRUE", "Value"] -
        1.96 * summary(daoh.maori.regression.models[[2]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.25.high = summary(daoh.maori.regression.models[[2]])$coefficients["maori.ethnicityTRUE", "Value"] +
        1.96 * summary(daoh.maori.regression.models[[2]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.25.p = summary(daoh.maori.regression.models[[2]])$coefficients["maori.ethnicityTRUE", "Pr(>|t|)"],
      
      maori.qr.50.diff = summary(daoh.maori.regression.models[[3]])$coefficients["maori.ethnicityTRUE", "Value"],
      maori.qr.50.low = summary(daoh.maori.regression.models[[3]])$coefficients["maori.ethnicityTRUE", "Value"] -
        1.96 * summary(daoh.maori.regression.models[[3]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.50.high = summary(daoh.maori.regression.models[[3]])$coefficients["maori.ethnicityTRUE", "Value"] +
        1.96 * summary(daoh.maori.regression.models[[3]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.50.p = summary(daoh.maori.regression.models[[3]])$coefficients["maori.ethnicityTRUE", "Pr(>|t|)"],
      
      maori.qr.75.diff = summary(daoh.maori.regression.models[[4]])$coefficients["maori.ethnicityTRUE", "Value"],
      maori.qr.75.low = summary(daoh.maori.regression.models[[4]])$coefficients["maori.ethnicityTRUE", "Value"] -
        1.96 * summary(daoh.maori.regression.models[[4]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.75.high = summary(daoh.maori.regression.models[[4]])$coefficients["maori.ethnicityTRUE", "Value"] +
        1.96 * summary(daoh.maori.regression.models[[4]])$coefficients["maori.ethnicityTRUE", "Std. Error"],
      maori.qr.75.p = summary(daoh.maori.regression.models[[4]])$coefficients["maori.ethnicityTRUE", "Pr(>|t|)"],
      
      maori.mort.90.reg.or = exp(summary(mort.90.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Estimate"]),
      maori.mort.90.reg.or.low = exp(
        summary(mort.90.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Estimate"] -
          1.96 * summary(mort.90.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Std. Error"]
      ),
      maori.mort.90.reg.or.high = exp(
        summary(mort.90.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Estimate"] +
          1.96 * summary(mort.90.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Std. Error"]
      ),
      maori.mort.90.reg.or.p = summary(mort.90.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Pr(>|z|)"],
      
      
      maori.mort.30.reg.or = exp(summary(mort.30.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Estimate"]),
      maori.mort.30.reg.or.low = exp(
        summary(mort.30.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Estimate"] -
          1.96 * summary(mort.30.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Std. Error"]
      ),
      maori.mort.30.reg.or.high = exp(
        summary(mort.30.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Estimate"] +
          1.96 * summary(mort.30.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Std. Error"]
      ),
      maori.mort.30.reg.or.p = summary(mort.30.maori.regression.model)$coefficients["maori.ethnicityTRUE", "Pr(>|z|)"]
    ),
    
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
    
  )
