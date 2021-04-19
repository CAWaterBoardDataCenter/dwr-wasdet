library(rmarkdown)

setwd("./docs")


pandoc_convert(input = "faq-source.docx",
               to = "html",
               from = "docx",
               output = "faq-source.html"
)

pandoc_convert(input = "faq-source.docx",
               to = "markdown",
               from = "docx",
               "faq-source.md"
)
