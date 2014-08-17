## code snippet to run the report

library(knitr)
setwd("C:\\Users\\Morten\\RepData_PeerAssessment1")
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")