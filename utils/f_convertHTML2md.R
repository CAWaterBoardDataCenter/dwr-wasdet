## Convert HTML to markdown for application.

convertHTML2md <- function(input_file) {
  
  library(rmarkdown)
  
  old_wd <- getwd()

  setwd("./app/docs")

  
  pandoc_convert(input = input_file,
                 to = "markdown_strict",
                 output = "demand-scenarios.md"
  )
  
  setwd(old_wd)
 
}
