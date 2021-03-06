# King-Camino-Project-2
Project 2 for St558 
Authors: Tommy King and Steph Camino

## Purpose  

In this repo you will find code for this project in the Project-2.Rmd file. The file starts off accessing data reguarding the prediction of number of shares in social networks from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity). Afterwards, there is some brief exploratory data analysis where summary statistics and plots are created. Four models are then made, two linear regression models, one random forest model, and one boosted tree model. Lastly, all four models are compared and the winning model is chosen based off of RMSE.   

## R packages used:  

* [`TidyVerse`](https://www.tidyverse.org/): This package was loaded to retrieve the packages below.
  * [`purrr`](https://purrr.tidyverse.org/): This package was used to map over multiple inputs simultaneously.
  * [`dplyr`](https://dplyr.tidyverse.org/): This package was used to `select`, `filter`, and `summarise` our data. 
  * [`tibble`](https://tibble.tidyverse.org/): This package was used to change the data into a tibble without simplifying the data.
  * [`ggplot2`](https://ggplot2.tidyverse.org/): This package was used to create our plots for the exploratory data analysis.
  * [`readr`](https://readr.tidyverse.org/): This package was used to load in our data. 
* [`Knitr`](https://cran.r-project.org/web/packages/knitr/index.html): This package was used to present tables nicely and to render the .Rmd file. 
* [`GridExtra`](https://cran.r-project.org/web/packages/gridExtra/index.html): This package was used to display plots neatly and nicely.
* [`Caret`](https://cran.r-project.org/web/packages/caret/index.html): This package was used to create our regression and classification models. 
* [`randomForest`](https://cran.r-project.org/web/packages/randomForest/index.html): This package was used as an add-on to `caret` for our random forest model. 
* [`gbm`](https://cran.r-project.org/web/packages/gbm/index.html): This package was used as an add-on to `caret` for our boosted tree model.


## The links to the generated anaysis for each data channel is written below:  

* [Lifestyle](Project2-lifestyle.md)
* [Entertainment](Project2-entertainment.md)
* [Business](Project2-bus.md)
* [Social Media](Project2-socmed.md)
* [Tech](Project2-tech.md)
* [World](Project2-world.md)

## Render Code  
  
The code below can be run in the R Console to generate the individual reports for each of the data channels. We wrote a function that takes the channel name as an argument and generates the `.md` file passing the specified channel as a parameter. We then loop through the 6 available channels to generate all of the reports.  
  
`renderReport <- function(type){`  
 &emsp; &emsp; `rmarkdown::render("Project-2.Rmd",`  
 &emsp; &emsp; &emsp; `params = list(type = type),`  
 &emsp; &emsp; &emsp; `output_format = "github_document",`  
 &emsp; &emsp; &emsp; `output_options = list(html_preview = FALSE),`  
 &emsp; &emsp; &emsp; `output_file = paste0("Project2-", type, ".md")`  
 &emsp; &emsp; &emsp; `)`  
`}`

`channels <- c("lifestyle","entertainment","bus","socmed","tech","world")` 

`for(c in channels){`  
&emsp; `renderReport(c)`  
`}`
