## Convert Markdown to HTML.

  library(rmarkdown)
  
  old_wd <- getwd()

  setwd("~/R/GitHub/work/dwr-wasdet/app/docs")

  
  pandoc_convert(input = "demand-scenarios.md",
                 to = "html",
                 output = "demand-scenarios.html"
  )
  
  setwd(old_wd)
 
## NOTE:
## Manually add <p></p> at the end of table in resulting HTML file.
