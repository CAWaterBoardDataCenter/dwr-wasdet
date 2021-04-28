## Convert FAQ Word Document to HTML FAQ for application.

FAQWord2Html <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd(paste0(old_wd, "/docs"))

  
  pandoc_convert(input = input_file,
                 to = "html",
                 from = "docx",
                 output = "faq.html"
  )
  
  setwd(old_wd)
 
}
