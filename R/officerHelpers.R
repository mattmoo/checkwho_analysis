

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

#Replace all instances of text, similar to body_replace_all_text, but ignoring chunking.
replaceAllDocx = function(inputDocx, old_value, new_value) {
  nReplace = 0
  maxIts = 100
  while (keyWordExists(inputDocx, old_value) && nReplace<maxIts) {
    inputDocx = cursor_reach(inputDocx, old_value)
    # inputDocx = body_bookmark(inputDocx, old_value)
    inputDocx = body_replace_all_text(inputDocx, old_value, new_value, only_at_cursor = T, fixed = T)
    
    nReplace = nReplace + 1
  }
  print(nReplace)
  return(inputDocx)
}

#Insert a table, if keyword is empty, do it at current cursor
insertTable = function(inputDocx, asset, keyWord = NULL, insertCaption = F, saveSeparatelyTo = NULL, caption = '') {
  
  if (!is.null(keyWord) && keyWordExists(inputDocx, keyWord)) {
    #Go to the right place to insert image
    inputDocx = cursor_reach(inputDocx, keyword = keyWord)
  }
  
  #Print the table, need to figure out how to do the row height properly
  if (insertCaption) {
    inputDocx = body_add_par(inputDocx, caption, style = "Table heading")
  }
  inputDocx = body_add_flextable(inputDocx, 
                                 asset@ft, 
                                 align = 'center')
  
  if (!is.null(saveSeparatelyTo)) {
    tableDocx = read_docx(blankTemplatePath) %>%
      cursor_begin() %>%
      body_add_par(caption, style = "Table heading") %>%
      body_add_flextable(asset@ft, align = 'center')
    print(tableDocx, target = saveSeparatelyTo)
  }
  
  
  if (keyWordExists(inputDocx, keyWord)) {
    #Remove image keyword
    inputDocx = cursor_reach(inputDocx, keyword = keyWord) %>% 
      body_remove()
  }
  
  return(inputDocx)
}

#Insert a figure, if keyword is empty, do it at current cursor
insertFigure = function(inputDocx,
                        asset,
                        keyWord = NULL,
                        insertCaption = F,
                        vectoriseWordFigs = T,
                        res = 450,
                        saveSeparatelyTo = NULL,
                        caption = '') {
  
  aspectRatio = asset@aspectRatio
  width = asset@widthInches
  plot = asset@plot
  
  if (!is.null(keyWord) && keyWordExists(inputDocx, keyWord)) {
    #Go to the right place to insert image
    inputDocx = cursor_reach(inputDocx, keyword = keyWord)
  }
  
  #Get page width
  pageDims = docx_dim(inputDocx)
  pageWidthWordInches = pageDims$page["width"] - pageDims$margins["left"] - pageDims$margins["right"]
  if (is.na(width) || is.null(width) || width==0 || length(width)==0) {
    width = pageWidthWordInches
  }
  
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
         res = 450,
         width = pageWidthWordInches, 
         height = pageWidthWordInches/aspectRatio,
         units = "in")
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

#Find references to the assets in a document for the purposes of ordering.
findAssetParagraphsInDocs = function(inputDocxContent, assetString) {
  refParagraphs = grep(assetString,inputDocxContent$text)
  if (length(refParagraphs) > 1) {
    message(paste0('More than one asset match for ', assetString, ", only first reference will be heeded."))
    refParagraphs = min(refParagraphs)
  }
  if (length(refParagraphs) == 0) {
    refParagraphs = 0
  }
  return(refParagraphs)
}

getRefStrings = function(assetName, assetType = c('figure','table')[1]) {
  if (assetType == 'figure') {
    typeString = 'FIG'
  } else if (assetType == 'table') {
    typeString = 'TAB'
  }
  result = list(asset = paste0(typeString, '_', assetName, '_ASSET'),
                cap = paste0(typeString, '_', assetName, '_CAP'),
                ref = paste0(typeString, '_', assetName, '_REF'))
  
  return(result)
}