##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param asset.list A list of all the assets that will be inserted.
##' @param report.docx.input.path Path for the report, with keywords to replace.
##' @param template.docx.path Path for a blank template, so that tables can be
##'   written to their own files.
##' @param report.output.dir
##' @param page.width.inches
##' @param figure.dpi
##' @param image.output.format
##' @param skip.report.figure.inds
##' @param skip.report.table.inds
##' @param skip.supp.figure.inds
##' @param skip.supp.table.inds
##' @param vectorise.word.figures Insert figures into word as vector EMF images?
##'   (PNG otherwise)
compile.manuscript <- function(asset.list,
                               report.docx.input.path,
                               report.output.dir,
                               template.docx.path,
                               page.width.inches = 6.5,
                               figure.dpi = 400,
                               image.output.format = 'PNG',
                               skip.report.figure.inds = NULL,
                               skip.report.table.inds = NULL,
                               skip.supp.figure.inds = NULL,
                               skip.supp.table.inds = NULL,
                               vectorise.word.figures = FALSE) {
  
  time.stamp = format(Sys.time(), "%Y-%m-%d_%H%M%S")
  
  report.output.dir = file.path(report.output.dir,
                                paste0('checkwho_', time.stamp))
  
  # Paths where standalone files will be held
  asset.output.paths = list()
  asset.output.paths$figure.directory = file.path(report.output.dir,'figures')
  asset.output.paths$supp.figure.directory = file.path(report.output.dir,'figures/supplementary')
  asset.output.paths$table.directory = file.path(report.output.dir,'tables')
  asset.output.paths$supp.table.directory = file.path(report.output.dir,'tables/supplementary')
  figure.legend.file.name = 'FigureLegends.docx'
  
  report.file.name = paste0(gsub(".docx","_",basename(report.docx.input.path)), time.stamp)
  
  figure.inds = which(unlist(lapply(asset.list, FUN = function(x) x$type)) =='figure')
  table.inds = which(unlist(lapply(asset.list, FUN = function(x) x$type)) =='table')
  
  message('AVAILABLE FIGURES:')
  message(paste(lapply(asset.list[figure.inds], FUN = function(x) x$name), collapse = "\n"))
  message()
  message('AVAILABLE TABLES:')
  message(paste(lapply(asset.list[table.inds], FUN = function(x) x$name), collapse = "\n"))
  message()
  
  
  #Get references to assets and their order in the documents
  asset.dt = data.table(
    asset = asset.list,
    name = as.character(lapply(asset.list, FUN = function(x) x$name)),
    type = as.character(lapply(asset.list, FUN = function(x) x$type))
  )
  
  if (unique(asset.dt[,.(name,type)])[,.N] != asset.dt[,.N]) {
    stop('Duplicate entries exist in asset list')
  }
  
  asset.dt[,asset.id := .I]
  
  get.ref.string = function(type, name, reference.type = c('ASSET', 'CAPTION', 'CROSSREF')[1]) {
    
    type.string = as.character(type)
    type.string[type %ilike% 'figure'] = 'FIG'
    type.string[type %ilike% 'table'] = 'TAB'
    
    reference.type.string = as.character(reference.type)
    reference.type.string[reference.type %ilike% 'CAPTION'] = 'CAP'
    reference.type.string[reference.type %ilike% 'CROSSREF'] = 'REF'
    reference.type.string[reference.type %ilike% 'ASSET'] = 'ASSET'
    
    # if (reference.type %ilike% 'CAPTION') {
    #   reference.type = 'CAP'
    # } else if (reference.type %ilike% 'CROSSREF') {
    #   reference.type = 'REF'
    # } else if (reference.type %ilike% 'ASSET') {
    #   reference.type = 'ASSET'
    # } else {
    #   stop(paste0('Could not find reference type: ', reference.type))
    # }
    
    
    result = paste0(type.string, '_', as.character(name), '_', reference.type.string)
    
    return(result)
  }
  
  # A data.table with the details of strings to replace.
  replacement.dt =
    CJ(
      asset.id = asset.dt[, asset.id],
      reference.type = c('ASSET', 'CAPTION', 'CROSSREF')
    )
  replacement.dt = data.table:::merge.data.table(
    x = replacement.dt,
    y = asset.dt[,.(asset.id, name, type)],
    by = 'asset.id'
  )
  replacement.dt[, reference.string := as.character(get.ref.string(type = type, name, reference.type))]
  
  
  # # Attach data on order to asset table
  # asset.dt = data.table:::merge.data.table(
  #   x = asset.dt,
  #   y = replacement.dt[reference.type == 'ASSET', .(asset.id, reference.string)],
  #   by = c('asset.id')
  # )
  
  
  #Load DOCX
  report.docx = officer::read_docx(path = report.docx.input.path)
  supplementary.docx = officer::read_docx(path = template.docx.path)   #Blank documents for supplementary and figure legends
  figure.legends.docx = officer::read_docx(path = template.docx.path) %>%
    cursor_begin()
  report.content = officer::docx_summary(report.docx)
  supplementary.content = officer::docx_summary(supplementary.docx)
  
  
  
  message(paste0("SEEKING ASSETS IN ", report.docx.input.path, ":"))
  # Find strings formatted as asset references in text.
  mentioned.assets.vector = unique(unlist(
    lapply(
      X = report.content$text,
      FUN = function(x)
        stringr::str_extract_all(x,
                                 pattern = '(FIG|TAB)_[:alnum:]+_[:upper:]+',
                                 simplify = TRUE)
    )
  ))
  
  
  # Get first instances of reference strings in text.
  # Get first paragraph mentioned.
  replacement.dt[, first.mention.paragraph := as.numeric(lapply(
    X = reference.string,
    FUN = function(x)
      min(c(which(
        stringr::str_detect(report.content$text, x)), 
        Inf))
  ))]
  # Get first character mentioned.
  for (row.ind in 1:replacement.dt[, .N]) {
    if (!is.infinite(replacement.dt[row.ind, first.mention.paragraph])) {
      
      data.table::set(
        x = replacement.dt,
        i = row.ind,
        j = 'first.mention.character',
        value = stringr::str_locate(report.content$text[replacement.dt[row.ind, first.mention.paragraph]],
                                    replacement.dt[row.ind, reference.string])[1, 1]
      )
    }
  }
  
  
  # Compare cross references.
  cross.ref.docx = mentioned.assets.vector[mentioned.assets.vector %like% '_REF']
  cross.ref.provided = replacement.dt[reference.type == 'CROSSREF', reference.string]
  
  cross.ref.missing.from.provided = setdiff(cross.ref.docx, cross.ref.provided)
  cross.ref.missing.from.docx = setdiff(cross.ref.provided, cross.ref.docx)
  
  message("ASSETS REFERENCED IN DOCX, BUT NOT PROVIDED (SHOULD BE NONE):")
  message(paste0(cross.ref.missing.from.provided,
                 collapse = '\n'))
  message(" ")
  message("ASSETS PROVIDED, BUT NOT REFERENCED IN DOCX:")
  message(paste0(cross.ref.missing.from.docx,
                 collapse = '\n'))
  message(" ")
  
  # Compare assets explicitly inserted in report
  asset.ref.docx = mentioned.assets.vector[mentioned.assets.vector %like% '_ASSET']
  asset.ref.provided = replacement.dt[reference.type == 'ASSET', reference.string]
  
  asset.ref.missing.from.provided = setdiff(asset.ref.docx, asset.ref.provided)
  asset.ref.missing.from.docx = setdiff(asset.ref.provided, asset.ref.docx)
  
  message("ASSETS INSERTED IN DOCX, BUT NOT PROVIDED (SHOULD BE NONE):")
  message(paste0(asset.ref.missing.from.provided,
                 collapse = '\n'))
  
  message(" ")
  message("ASSETS PROVIDED, BUT NOT INSERTED IN DOCX:")
  message(paste0(asset.ref.missing.from.docx,
                 collapse = '\n'))
  
  # Give the explicitly inserted tables and figures their numbers.
  asset.dt = merge(x = asset.dt,
                   y = replacement.dt[reference.string %in% asset.ref.docx, 
                                      .(asset.id, 
                                        report.index = order(first.mention.paragraph, first.mention.character)), 
                                      by = type],
                   by = c('asset.id', 'type'),
                   all.x = TRUE)
  
  asset.dt[is.na(report.index)][asset.id %in% replacement.dt[reference.string %in% cross.ref.docx, asset.id]]
  # Anything else referenced but not explicitly inserted is supplementary, and
  # inserted in order of reference.
  asset.dt = merge(
    x = asset.dt,
    y = replacement.dt[reference.string %in% cross.ref.docx &
                         asset.id %in% asset.dt[is.na(report.index), asset.id], 
                       .(asset.id,
                         supplementary.index = order(first.mention.paragraph, first.mention.character)),
                       by = type],
    by = c('asset.id', 'type'),
    all.x = TRUE
  )
  
  # Skip the indices that need skipping
  for (skip.report.figure.ind in skip.report.figure.inds) {
    asset.dt[type == 'figure' & report.index >= skip.report.figure.ind, 
             report.index := report.index + 1]
  }
  for (skip.report.table.ind in skip.report.table.inds) {
    asset.dt[type == 'table' & report.index >= skip.report.table.ind, 
             report.index := report.index + 1]
  }
  
  # Put the indices in the replacements table
  replacement.dt = merge(
    x = replacement.dt,
    y = asset.dt[,.(asset.id, report.index, supplementary.index)],
    by = 'asset.id',
    all.x = TRUE)[order(type, report.index, supplementary.index)]
  
  # Only references that exist.
  report.replacement.dt = replacement.dt[!is.na(report.index)]
  supplementary.replacement.dt = replacement.dt[!is.na(supplementary.index)]
  
  construct.asset.label = function(type, number, supplementary = FALSE) {
    result = paste0(
      stringr::str_to_title(type),
      ' ',
      ifelse(supplementary, yes = 'S', no = ''),
      number
    )
  }
  
  construct.asset.filename = function(type, number, supplementary = FALSE) {
    result = paste0(
      stringr::str_to_lower(type),
      ifelse(supplementary, yes = 'S', no = ''),
      sprintf(fmt = '%02d', number)
    )
  }
  
  construct.caption = function(type, number, caption, supplementary = FALSE) {
    result = paste0(
      construct.asset.label(type = type, number = number, supplementary = supplementary),
      '. ',
      trimws(caption)
    )
  }
  
  #See if keyword exists, otherwise return FALSE.
  #Will probably return FALSE in a whole lot of other situations as well, careful.
  keyWordExists = function(inputDoc, keyword) {
    result = tryCatch({
      cursor_reach(inputDoc, keyword = keyword)
      T
    }, warning = function(w) {
      
    }, error = function(e) {
      F
    }, finally = {
      
    })
    return(result)
  }
  
  #Insert a table, if keyword is empty, do it at current cursor
  insertTable = function(inputDocx,
                         asset,
                         keyWord = NULL,
                         insertCaption = F,
                         saveSeparatelyTo = NULL,
                         caption = '') {
    
    asset$ft = flextable::autofit(asset$ft)
    
    if (!is.null(keyWord) && keyWordExists(inputDocx, keyWord)) {
      #Go to the right place to insert image
      inputDocx = officer::cursor_reach(inputDocx, keyword = keyWord)
    }
    
    #Print the table, need to figure out how to do the row height properly
    if (insertCaption) {
      inputDocx = officer::body_add_par(inputDocx, caption, style = "Table heading")
    }
    inputDocx = flextable::body_add_flextable(inputDocx,
                                              asset$ft,
                                              align = 'center')
    
    if (!is.null(saveSeparatelyTo)) {
      tableDocx = officer::read_docx(template.docx.path) %>%
        officer::cursor_begin() %>%
        officer::body_add_par(caption, style = "Table heading") %>%
        flextable::body_add_flextable(asset$ft, align = 'center')
      print(tableDocx, target = saveSeparatelyTo)
    }
    
    
    if (keyWordExists(inputDocx, keyWord)) {
      #Remove image keyword
      inputDocx = officer::cursor_reach(inputDocx, keyword = keyWord) %>%
        officer::body_remove()
    }
    
    return(inputDocx)
  }
  
  # Go through the references replacing captions
  for (replacement.ind in 1:report.replacement.dt[, .N]) {
    reference.type = report.replacement.dt[replacement.ind, reference.type]
    
    # For captions in report
    if (reference.type == 'CAPTION') {
      target.asset.id = report.replacement.dt[replacement.ind, asset.id]
      reference.string = report.replacement.dt[replacement.ind, reference.string]
      type = report.replacement.dt[replacement.ind, type]
      report.index = report.replacement.dt[replacement.ind, report.index]
      
      full.caption = construct.caption(type, 
                                       report.index, 
                                       asset.dt[asset.id == target.asset.id,
                                                asset[[1]]$caption],
                                       supplementary = FALSE)
      
      officer::body_replace_all_text(x = report.docx,
                                     old_value = reference.string,
                                     new_value = full.caption
      )
      
    }
    
  }
  
  
  #Insert a figure, if keyword is empty, do it at current cursor
  insertFigure = function(inputDocx,
                          asset,
                          keyWord = NULL,
                          insertCaption = F,
                          vectoriseWordFigs = T,
                          res = 450,
                          saveSeparatelyTo = NULL,
                          caption = '',
                          pageWidthWordInches = NULL) {
    
    aspectRatio = asset$aspect.ratio
    plot = asset$ggplot
    
    if (!is.null(keyWord) && keyWordExists(inputDocx, keyWord)) {
      #Go to the right place to insert image
      inputDocx = officer::cursor_reach(inputDocx, keyword = keyWord)
    }
    
    if (is.null(pageWidthWordInches)) {
      #Get page width
      pageDims = officer::docx_dim(inputDocx)
      pageWidthWordInches = pageDims$page["width"] - pageDims$margins["left"] - pageDims$margins["right"]
      # if (is.na(width) || is.null(width) || width==0 || length(width)==0) {
      #   width = pageWidthWordInches
      # }
    }
    width = asset$width.proportion * pageWidthWordInches
    
    #Print figure for word.
    if (vectoriseWordFigs) {
      filename = tempfile(fileext = ".emf")
      emf(file = filename, 
          width = pageWidthWordInches, 
          height = pageWidthWordInches/aspectRatio)
    } else {
      filename = tempfile(fileext = ".png")
      png(file = filename, 
          res = res,
          units = 'in',
          width = pageWidthWordInches, 
          height = pageWidthWordInches/aspectRatio)
    }
    print(plot)
    dev.off()
    
    if (!is.null(saveSeparatelyTo)) {
      tiff(file = saveSeparatelyTo,
           res = res,
           width = pageWidthWordInches, 
           height = pageWidthWordInches/aspectRatio,
           units = "in",
           compression = "zip+p")
      print(plot)
      dev.off()
    }
    
    #Add image to docx
    inputDocx =  body_add_img(inputDocx, 
                              src = filename, 
                              width = pageWidthWordInches, 
                              height = pageWidthWordInches/aspectRatio,
                              style = 'Figure')
    
    if (insertCaption) {
      inputDocx = body_add_par(inputDocx, caption, style = "Figure Caption")
    }
    
    if (keyWordExists(inputDocx, keyWord)) {
      #Remove image keyword
      inputDocx = cursor_reach(inputDocx, keyword = keyWord) %>% 
        body_remove()
    }
    
    return(inputDocx)
  }
  
  
  # Do cross references, there might be some in the captions.
  for (replacement.ind in 1:replacement.dt[, .N]) {
    reference.type = replacement.dt[replacement.ind, reference.type]
    
    if (reference.type == 'CROSSREF') {
      target.asset.id = replacement.dt[replacement.ind, asset.id]
      reference.string = replacement.dt[replacement.ind, reference.string]
      type = replacement.dt[replacement.ind, type]
      report.index = replacement.dt[replacement.ind, report.index]
      supplementary.index = replacement.dt[replacement.ind, supplementary.index]
      
      asset.index = ifelse(!is.na(report.index),
                           yes = report.index,
                           no = supplementary.index)
      asset.label = construct.asset.label(type, asset.index, supplementary = is.na(report.index))
      
      if (!is.na(report.index) | !is.na(supplementary.index)) {

        officer::body_replace_all_text(
          x = report.docx,
          old_value = reference.string,
          new_value = asset.label
        )
      }
    }
  }
  
  manuscript.docx = copy(report.docx)  #Document with no assets, just [Insert x here]
  
  dir.create(asset.output.paths$supp.figure.directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(asset.output.paths$supp.table.directory, recursive = TRUE, showWarnings = FALSE)
  
  # Go through the references replacing assets
  for (replacement.ind in 1:replacement.dt[, .N]) {
    reference.type = replacement.dt[replacement.ind, reference.type]
    
    if (reference.type == 'ASSET') {
      
      target.asset.id = replacement.dt[replacement.ind, asset.id]
      reference.string = replacement.dt[replacement.ind, reference.string]
      type = replacement.dt[replacement.ind, type]
      report.index = replacement.dt[replacement.ind, report.index]
      supplementary.index = replacement.dt[replacement.ind, supplementary.index]
      asset.index = ifelse(!is.na(report.index),
                           yes = report.index,
                           no = supplementary.index)
      
      asset = asset.dt[asset.id == target.asset.id,
                       asset][[1]]
      
      
      file.name = construct.asset.filename(type = type, 
                                           number = asset.index, 
                                           supplementary = is.na(report.index))
      
      full.caption = construct.caption(type = type,
                                       number = asset.index,
                                       caption = asset$caption,
                                       supplementary = is.na(report.index))
      
      asset.label = construct.asset.label(type = type, 
                                          number = report.index, 
                                          supplementary = is.na(report.index))
      
      if (type == 'table') {
        if (!is.na(report.index)) {
          
          report.docx = insertTable(
            inputDocx = report.docx,
            asset = asset,
            keyWord = reference.string,
            saveSeparatelyTo = file.path(asset.output.paths$table.directory, paste0(file.name, '.docx')),
            caption = full.caption
          )
          
          
        } else if (!is.na(supplementary.index)) {
          
          supplementary.docx = insertTable(
            inputDocx = supplementary.docx,
            asset = asset,
            insertCaption = TRUE,
            saveSeparatelyTo = file.path(asset.output.paths$supp.table.directory, paste0(file.name, '.docx')),
            caption = full.caption
          )
        }
      } else if (type == 'figure') {
        if (!is.na(report.index)) {
          
          report.docx = insertFigure(
            inputDocx = report.docx,
            asset = asset,
            res = figure.dpi,
            keyWord = reference.string,
            vectoriseWordFigs = vectorise.word.figures,
            saveSeparatelyTo = file.path(asset.output.paths$figure.directory, paste0(file.name, '.tiff')),
            caption = full.caption
          )
          
          
          
        } else if (!is.na(supplementary.index)) {
          
          supplementary.docx = insertFigure(
            inputDocx = supplementary.docx,
            asset = asset,
            res = figure.dpi,
            insertCaption = TRUE,
            vectoriseWordFigs = vectorise.word.figures,
            saveSeparatelyTo = file.path(asset.output.paths$supp.figure.directory, paste0(file.name, '.tiff')),
            caption = full.caption
          )
        }
        
        #Add to figure legends file.
        figure.legends.docx = officer::body_add_par(figure.legends.docx, full.caption, style = "Normal no indent")
        
      }
      
      #Edit assetless manuscript
      if (!is.na(report.index)) {
        
        if (keyWordExists(manuscript.docx, reference.string)) {
          manuscript.docx = officer::cursor_reach(manuscript.docx,
                                                  keyword = reference.string) %>%
            officer::body_remove() %>%
            officer::body_add_par(paste("[INSERT", toupper(asset.label), "HERE]"), style = "Figure")
        }
      }
    }
    
  }
  
  
  print(report.docx,
        target = file.path(
          report.output.dir,
          paste0(report.file.name,
                 '_CompiledManuscript.docx')
        ))
  
  print(supplementary.docx,
        target = file.path(
          report.output.dir,
          paste0(report.file.name,
                 '_SupplementaryMaterial.docx')
        ))
  
  print(manuscript.docx, 
        target = file.path(
          report.output.dir,
          paste0(report.file.name,
                 '_NoAssetManuscript.docx')
        ))
  
  print(figure.legends.docx,
        target = file.path(
          asset.output.paths$figure.directory,
          paste0(report.file.name,
                 '_FigureLegends.docx')
        ))
  
  return(report.docx)
}
