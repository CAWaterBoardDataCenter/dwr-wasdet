## Convert FAQ Word Document to HTML FAQ for application.

faq2html <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd(paste0(old_wd, "/docs"))

  
  pandoc_convert(input = input_file,
                 to = "html",
                 from = "docx",
                 output = "faq-source.html"
  )
  
  setwd(old_wd)
 
}
