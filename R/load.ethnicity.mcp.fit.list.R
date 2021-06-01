##' Load the ethnicity mcp fit calculations (calulated on the VM), and get
##' ethnicity name from the filename.
##' 
##' They're actually RDA files, despite extension, sorry.
##'
##' @title
##' @param files
load.ethnicity.mcp.fit.list <- function(file.paths) {

  ethnicity.names = 'Asian|European|Maori|Other|(Pacific Peoples)'
  
  result = list()
  for (file.path in file.paths) {
    ethnicity = stringr::str_extract(file.path, ethnicity.names)
    print(file.path)
    print(tools::file_ext(file.path))
    if (stringr::str_detect(tools::file_ext(file.path), fixed('rds', ignore_case=TRUE))) {
      load(file.path)
      result[[ethnicity]] = iter.gaussian.emp.logit.daoh.mcp.fit
    } else if (stringr::str_detect(tools::file_ext(file.path), fixed('rda', ignore_case=TRUE))) {
      result[[ethnicity]] = readRDS(file.path)

    }
    
  }
  
  return(result)
  
}
