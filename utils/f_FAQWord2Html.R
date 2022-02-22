library(rmarkdown)

## Convert FAQ Word Document to HTML FAQ for application.

FAQWord2Html <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd(paste0(old_wd, "/app/docs"))

  
  pandoc_convert(input = "DWR-WaSDET-FAQ-v1.5.docx",
                 to = "html",
              #   from = "docx",
                 output = "faq.html"
  )
  
  setwd(old_wd)
 
}
