# This code will render the output .md file from our .rmd
rmarkdown::render("Project-2.Rmd", output_file = "report.md", params = list(data_channel = "entertainment"))