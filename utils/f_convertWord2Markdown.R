## Convert Word Document to markdown for application.

convertWord2Markown <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd(paste0(old_wd, "/docs"))

  
  pandoc_convert(input = input_file,
              #   to = "html",
                 from = "docx",
                 output = "faq-source.md"
  )
  
  setwd(old_wd)
 
}
