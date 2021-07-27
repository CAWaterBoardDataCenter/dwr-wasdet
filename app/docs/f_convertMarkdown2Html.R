## Convert Word Document to markdown for application.

convertMarkown2Html <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd(paste0(old_wd, "/docs"))

  
  pandoc_convert(input = input_file,
                 to = "html",
                 output = "demand-scenarios.html"
  )
  
  setwd(old_wd)
 
}
