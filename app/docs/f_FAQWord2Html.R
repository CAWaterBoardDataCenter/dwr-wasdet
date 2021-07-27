## Convert FAQ Word Document to HTML FAQ for application.

FAQWord2Html <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd(paste0(old_wd, "/docs"))

  
  pandoc_convert(input = "bugs-issues.md",
                 to = "html",
              #   from = "docx",
                 output = "bugs-issues.html"
  )
  
  setwd(old_wd)
 
}
