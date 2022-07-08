Project 2
================
Tommy King and Steph Camino
2022-06-30

``` r
library(tidyverse)
library(knitr)
library(gridExtra)
library(caret)
library(randomForest)
library(gbm)
```

# Introduction

For this project, we’re going to be taking a look at news article
popularity data in the hopes of gaining some insight into the key
factors that lead to articles accumulating shares on social media. To
start out, we’re going to hone in on the Entertainment data channel to
do some exploration before automating our process for use with any of
the channels. Once we take our subset of the data based on our channel
selection, we’ll initially take a look at the variables corresponding to
the day of the week an article was published on, as well as the number
of words in the title/body of the article and the number of images and
videos included.

Once we’ve completed our exploratory analysis, we’ll select features
that we think would be a good fit in various types of models. We’ll
compare and evaluate the models we build on several metrics to decide
which model provides the most accurate forecasts of article popularity.

# Data

``` r
# URL for the Online News Popularity Data Folder
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip"

# Creates two temporary files
temp <- tempfile()
temp2 <- tempfile()

# Downloads the zipped folder from the URL and saves it in temp
download.file(url, temp)

# Unzips temp and saves it in temp2
unzip(zipfile = temp, exdir = temp2)

# Reads in the data from temp2 and saves it as data
data <- readr::read_csv(file.path(temp2, "OnlineNewsPopularity/OnlineNewsPopularity.csv"))

# Unlinks our temporary files
unlink(c(temp, temp2), force = TRUE)
```

``` r
# Creates a new dataset "entertainment"
# Filters data for when the data channel is chosen
# Removes Variables that all start with data_channel_is_, url, and timedelta
# url and timedelta are non-predictive

# Added this variable for automation purposes
channel_selection <- paste0("data_channel_is_", params$type)

# Here we're filtering based on whatever channel was selected
dataSubset <- data %>% as_tibble() %>%
  filter(!!as.symbol(channel_selection) == 1) %>%
  select(-c(url, timedelta, starts_with("data_channel_is_")))

# Creates a new character variable "day" that states what day it is. Derived from the binary variables for each day. 
# Factors the variable so we can use it as a categorical variable.
# Orders the factors in the order of the days of the week.
dataSubset2 <- dataSubset
dataSubset2$day <- ifelse(dataSubset$weekday_is_monday == 1, "Monday",
                            ifelse(dataSubset$weekday_is_tuesday == 1, "Tuesday",
                                   ifelse(dataSubset$weekday_is_wednesday == 1, "Wednesday",
                                          ifelse(dataSubset$weekday_is_thursday == 1, "Thursday",
                                                 ifelse(dataSubset$weekday_is_friday == 1, "Friday",
                                                        ifelse(dataSubset$weekday_is_saturday == 1, "Saturday",
                                                               ifelse(dataSubset$weekday_is_sunday == 1, "Sunday", "NA"))))))) %>%
                      as.factor() %>% 
                      ordered(levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
```

# Summarizations

-   The first set of plots:

    -   When you look at the first set of plots, the plot in the upper
        left-hand corner is a histogram of shares. Here you can see the
        distribution of the response variable. Look out for potential
        skewness and potential outliers.

    -   The plot in the upper right-hand corner is a scatterplot for
        number of words in the title versus the number of shares it got.
        Here you can look at the relationship between the two variables.
        Watch out for any potential transformations that need to be made
        when modeling your linear regression models.

    -   The next plot is the middle left, it is a scatterplot of the
        number of words in the content versus the number of shares it
        got. Here you can look at the relationship between the two
        variables. Watch out for any potential transformations that need
        to be made when modeling your linear regression models.

    -   The middle right plot is a scatterplot of number of images
        versus the number of shares it got. Here you can look at the
        relationship between the two variables. Watch out for any
        potential transformations that need to be made when modeling
        your linear regression models.

    -   The scatterplot of number of videos versus number of shares it
        got is in the bottom left-hand corner. Here you can look at the
        relationship between the two variables. Watch out for any
        potential transformations that need to be made when modeling
        your linear regression models.

    -   The scatterplot in the bottom right-hand corner is the number of
        links versus the number of shares it got. Here you can look at
        the relationship between the two variables. Watch out for any
        potential transformations that need to be made when modeling
        your linear regression models.

-   The second set of plots:

    -   In the upper left-hand corner is a density plot of text
        subjectivity. Here a kernel density estimate was used to show
        the probability density function of the variable. Look out for
        potential skewness and potential data distributions here.

    -   In the upper right-hand corner is a density plot of text
        sentimental polarity. Here a kernel density estimate was used to
        show the probability density function of the variable. Look out
        for potential skewness and potential data distributions here.

    -   In the lower left-hand corner is a density plot of the rate of
        positive words in the content. Here a kernel density estimate
        was used to show the probability density function of the
        variable. Look out for potential skewness and potential data
        distributions here.

    -   The boxplot in the lower right-hand corner are the boxplots of
        shares for every day of the week. Here you can look for skewness
        and potential outliers in the data. You can also look for
        consistencies or inconsistencies across days, which will shed
        light on the relationship between day of the week and number of
        shares.

-   The final image:

    -   Here you can see a scatterplot for each day of the week of
        Number of words in the number of words in the content and number
        of shares it got. If there are big differences across the plots
        there may be an interaction effect between day of the week and
        number of words in the content.

``` r
# Histogram for shares
g <- ggplot(dataSubset, aes(x = shares)) + 
        geom_histogram(fill = "blue", binwidth = 5000) + 
        labs(x = "Shares", y = "Count", title = "Histogram of Shares")

# Scatterplot for n_tokens_title vs shares
g2 <- ggplot(dataSubset2, aes(x = n_tokens_title, y = shares)) + 
          geom_point() + 
          labs(x = "Number of Words in the Title", y = "Shares", title = "Title Word Count vs Shares") 

# Scatterplot for n_tokens_content vs shares
g3 <- ggplot(dataSubset2, aes(x = n_tokens_content, y = shares)) + 
          geom_point(color = "Green") + 
          labs(x = "Number of Words in the Content", y = "Shares", title = "Content Word Count vs Shares") 
# Scatter plots for number of images, videos and links compared to shares
g4 <- ggplot(dataSubset, aes(x = num_imgs, y = shares)) + geom_point(color = "Red") + labs(x = "Number of Images", y = "Shares", title = "Images vs Shares") 
g5 <- ggplot(dataSubset, aes(x = num_videos, y = shares)) + geom_point(color = "Aquamarine") + labs(x = "Number of Videos", y = "Shares", title = "Videos vs Shares") 
g6 <- ggplot(dataSubset, aes(x = num_hrefs, y = shares)) + geom_point(color = "Purple") + labs(x = "Number of Links", y = "Shares", title = "Links vs Shares") 

# Arranging all the plots we have thus far for ease of use
grid.arrange(g, g2, g3, g4, g5, g6, ncol = 2, nrow = 3)
```

![](Project2-tech_files/figure-gfm/plots-1.png)<!-- -->

``` r
# Looking into plotting some of the distributions of the more obscure variables
g7 <- ggplot(dataSubset, aes(global_subjectivity)) + 
  geom_density(kernel = "gaussian", color = "Coral", fill = "Coral", alpha = .5) + labs(y = "Density", x = "Text Subjectivity", title = "Density Plot: Text Subjectivity") 

g8 <- ggplot(dataSubset, aes(global_sentiment_polarity)) + 
  geom_density(kernel = "gaussian", color = "Blue", fill = "Blue", alpha = .5) + labs(y = "Density", x = "Text Sentimental Polarity", title = "Density Plot: Text Sentimental Polarity")

g9 <- ggplot(dataSubset, aes(global_rate_positive_words)) + 
  geom_density(kernel = "gaussian", color = "Green", fill = "Green", alpha = .5) + labs(y = "Density", x = "Rate of Positive Words in the Content", title = "Density plot: Positive Words")

# Plots a Boxplot of number of shares per day of the week
g10 <- ggplot(dataSubset2, aes(x = day, y = shares)) + 
          geom_boxplot(fill = "grey") + 
          labs(x = "Day of the Week", y = "Shares", title = "Boxplot of Shares per Day") +
          scale_x_discrete(labels = c("Monday" = "Mon", "Tuesday" = "Tue", "Wednesday" = "Wed", "Thursday" = "Thu", "Friday" = "Fri", "Saturday" = "Sat", "Sunday" = "Sun"))

grid.arrange(g7, g8, g9, g10, ncol = 2, nrow = 2)
```

![](Project2-tech_files/figure-gfm/plots-2.png)<!-- -->

``` r
# Plot for every day of the week for Number of Words in the Content vs Shares
g11 <- ggplot(dataSubset2, aes(x = n_tokens_content, y = shares)) + 
          geom_point() + 
          facet_wrap(~ day) + 
          labs(x = "Number of Words in the Content", y = "Shares", title = "Content Word Count vs Shares for Every Day of the Week")
g11
```

![](Project2-tech_files/figure-gfm/plots-3.png)<!-- -->

Below is a numerical summary for the response variable, number of
shares. Here you can see the minimum, average, median, maximum, and
variance. Here you can see the spread of the data and if the data is
skewed, if the average and median are drastically different.

``` r
# Calculates the minimum, average, median, maximum, and variance of Shares.
shareSum <- dataSubset %>% summarise(min = min(shares), avg = mean(shares), med = median(shares), max = max(shares), var = var(shares))

# Creates vector of summary statistic types.
word <- c("Minimum", "Average", "Median", "Maximum", "Variance")

# Prints out the summaries for Shares.
for(i in 1:5){
  print(paste0("The ", word[i], " of Shares is ", shareSum[i]))
}
```

    ## [1] "The Minimum of Shares is 36"
    ## [1] "The Average of Shares is 3072.28328341955"
    ## [1] "The Median of Shares is 1700"
    ## [1] "The Maximum of Shares is 663600"
    ## [1] "The Variance of Shares is 81438781.071952"

Below is a numerical summary for test subjectivity. Here you can see the
minimum, average, median, maximum, and variance. Here you can see the
spread of the data and if the data is skewed, if the average and median
are drastically different.

``` r
# Calculates the minimum, average, median, maximum, and variance of Text Subjectivity. 
globalSubSum <- dataSubset %>% summarise(min = min(global_subjectivity), avg = mean(global_subjectivity), med = median(global_subjectivity), max = max(global_subjectivity), var = var(global_subjectivity))

# Prints out the summaries for Text Subjectivity.
for(i in 1:5){
  print(paste0("The ", word[i], " of Text Subjectivity is ", globalSubSum[i]))
}
```

    ## [1] "The Minimum of Text Subjectivity is 0"
    ## [1] "The Average of Text Subjectivity is 0.456805703307875"
    ## [1] "The Median of Text Subjectivity is 0.4597693179835"
    ## [1] "The Maximum of Text Subjectivity is 0.812692307692"
    ## [1] "The Variance of Text Subjectivity is 0.00574835336950984"

Below is a contingency table for the number of words in the title and if
it was on a weekend or weekday. Here you can see the relationship
between these two categorical variables.

``` r
# Creates a Contingency for Number of words in the title and if it's on a weekend or not.
kable(table(dataSubset$is_weekend, dataSubset$n_tokens_title), caption = "Counts for Number of Words in the Title for Weekends (0) or Weekdays (1)")
```

|     |   4 |   5 |   6 |   7 |   8 |    9 |   10 |   11 |  12 |  13 |  14 |  15 |  16 |  17 |  18 |  19 |  20 |
|:----|----:|----:|----:|----:|----:|-----:|-----:|-----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| 0   |   4 |  37 | 147 | 420 | 782 | 1060 | 1218 | 1071 | 808 | 487 | 240 | 103 |  23 |  17 |   6 |   1 |   1 |
| 1   |   0 |   4 |  51 |  54 |  76 |  182 |  144 |  167 | 114 |  72 |  41 |  11 |   3 |   2 |   0 |   0 |   0 |

Counts for Number of Words in the Title for Weekends (0) or Weekdays (1)

Below is a contingency table for the number of keywords in the metadata
and the day of the week. Here you can see the relationship between these
two categorical variables.

``` r
# Creates a Contingency for the Number of Keywords in the Metadata for every day of the week.
kable(table(dataSubset2$day, dataSubset2$num_keywords), caption = "Counts for Number of Keywords in the Metadata per Day of the Week")
```

|           |   2 |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 |
|:----------|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| Monday    |   2 |   4 |  22 |  96 | 200 | 225 | 232 | 182 | 272 |
| Tuesday   |   0 |   9 |  33 | 102 | 209 | 316 | 250 | 220 | 335 |
| Wednesday |   1 |   7 |  36 | 116 | 213 | 319 | 233 | 215 | 277 |
| Thursday  |   0 |   5 |  27 | 119 | 176 | 274 | 235 | 219 | 255 |
| Friday    |   0 |   7 |  25 |  82 | 152 | 188 | 192 | 126 | 217 |
| Saturday  |   0 |   3 |   9 |  24 |  37 |  98 | 108 |  89 | 157 |
| Sunday    |   0 |   4 |   6 |  26 |  30 |  59 |  71 |  70 | 130 |

Counts for Number of Keywords in the Metadata per Day of the Week

# Modeling

## Train/Test Split

The code chunk below splits our data into our training and testing sets
using `caret`:

``` r
set.seed(1024)

train_index <- createDataPartition(dataSubset$shares, p = 0.7, list = FALSE)

train <- dataSubset[train_index, ]
test <- dataSubset[-train_index, ]
```

## Linear Models

Linear regression fits a linear equation to the data, attempting to
model the relationship between two variables, the response and
explanatory variables. This equation is most commonly found by using the
method of least-squares, which minimizes the sum of squared residuals.
These models are usually written as
![Y_i = \\beta_0 + \\beta_1x\_{i1}+ ... + \\beta_px\_{ip} + E_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_i%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1x_%7Bi1%7D%2B%20...%20%2B%20%5Cbeta_px_%7Bip%7D%20%2B%20E_i "Y_i = \beta_0 + \beta_1x_{i1}+ ... + \beta_px_{ip} + E_i").

``` r
# Creates a linear regression model with main effects and interaction terms.
fitMLR1 <- train(shares ~ .^2, data = train,
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 5))
```

``` r
# linear regression model using just the factors themselves
fitMLR2 <- train(shares ~ ., data = train,
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 5))
```

## Ensamble Models

### Random Forest Model

Random forest is a an ensemble learning method that is an extension of
the idea of the bagging method. Like the bagging method, the random
forest algorithm uses bagging, also known as bootstrap aggregation, to
resample from the data or a fitted model randomly. Then multiple
decision trees are created from these samples to create an uncorrelated
forest and the results are then averaged. Unlike bagging, random forest
doesn’t use all of it’s predictors but uses a random subset of
predictors for each bootstrap sample. If there is a strong predictor,
it’ll likely be used for every first split in bagging, so randomly
subsetting predictors will reduce correlation of tree predictions in
random forest models.

``` r
# Training the Random Forest model with 5 fold cross-validation with center and scaling the data via preprocess, and considering the values of mtry of 1 to a third of number of columns in the data with tuneGrid.
fitRF <- train(shares ~ ., data = train,
                method = "rf",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 5))
                tuneGrid = expand.grid(mtry = c(1:round(ncol(dataSubset)/3)))

# Print out Model
fitRF
```

    ## Random Forest 
    ## 
    ## 5145 samples
    ##   52 predictor
    ## 
    ## Pre-processing: centered (52), scaled (52) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4115, 4116, 4116, 4116, 4117 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared     MAE     
    ##    2    8220.866  0.025384961  2491.962
    ##   27    9381.841  0.009856328  2706.678
    ##   52    9675.381  0.008895162  2726.181
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 2.

``` r
# Plot the hyperparameters
plot(fitRF)
```

![](Project2-tech_files/figure-gfm/steph_model_e-1.png)<!-- -->

### Boosted Tree Model

A boosted tree model is a type of ensemble learning method that
functions somewhat similarly to a random forest model but utilizes a
technique known as boosting instead of bagging. Boosting as a whole
attempts to correct errors created by individual trees in the ensemble
method. In this case, the boosting process is iterative and each new
decision tree that is made considers errors made by previous trees in
order to increase the overall accuracy of the model. We’re making use of
several tuning parameters here during the model building process.

``` r
fitBT <- train(shares ~., data = train, method = 'gbm',
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", number = 5),
               tuneGrid = expand.grid(n.trees = c(50,100,150,200), interaction.depth = c(1,2,3,4,5,6),
                                      shrinkage = 0.1, n.minobsinnode = 25),
               verbose = FALSE)
# Model printout
fitBT
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 5145 samples
    ##   52 predictor
    ## 
    ## Pre-processing: centered (52), scaled (52) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4116, 4114, 4118, 4117, 4115 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared     MAE     
    ##   1                   50      8486.674  0.004354500  2505.689
    ##   1                  100      8630.536  0.005981854  2566.076
    ##   1                  150      8589.560  0.006054979  2563.097
    ##   1                  200      8549.868  0.006993214  2543.382
    ##   2                   50      8443.790  0.010208792  2478.686
    ##   2                  100      8567.445  0.010783692  2518.744
    ##   2                  150      8634.732  0.011054106  2525.791
    ##   2                  200      8727.390  0.013059261  2549.923
    ##   3                   50      8452.325  0.012182942  2480.952
    ##   3                  100      8529.562  0.014196882  2492.700
    ##   3                  150      8647.096  0.014563293  2525.971
    ##   3                  200      8747.495  0.012350648  2550.175
    ##   4                   50      8482.862  0.016450663  2511.858
    ##   4                  100      8572.679  0.019209605  2539.024
    ##   4                  150      8786.229  0.016776491  2623.039
    ##   4                  200      8838.765  0.014896002  2634.931
    ##   5                   50      8450.937  0.017161811  2499.060
    ##   5                  100      8565.275  0.017867990  2538.590
    ##   5                  150      8687.029  0.018183416  2568.935
    ##   5                  200      8719.481  0.019031608  2601.327
    ##   6                   50      8423.436  0.029064962  2507.117
    ##   6                  100      8532.810  0.028813134  2549.409
    ##   6                  150      8681.093  0.025755423  2620.630
    ##   6                  200      8733.108  0.025971010  2664.077
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was
    ##  held constant at a value of 25
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 50, interaction.depth = 6, shrinkage = 0.1
    ##  and n.minobsinnode = 25.

``` r
# Plot hyperparameters
plot(fitBT)
```

![](Project2-tech_files/figure-gfm/tommy_model_e-1.png)<!-- -->

# Comparison

``` r
# Predicts Shares using the models and test data
predFitMLR1 <- predict(fitMLR1, newdata = test)
predFitMLR2 <- predict(fitMLR2, newdata = test)
predFitRF <- predict(fitRF, newdata = test)
predFitBT <- predict(fitBT, newdata = test)

# Compares the predicted shares found above to the shares in the test dataset
postMLR1 <- postResample(predFitMLR1, obs = test$shares)
postMLR2 <- postResample(predFitMLR2, obs = test$shares)
postRF <- postResample(predFitRF, obs = test$shares)
postBT <- postResample(predFitBT, obs = test$shares)

# Creates the names of the different models 
modelNames <- c("Interaction and Main Effects Model", "Main Effects Model", "Random Forest Model", "Boosting Tree Model")

# row binds the post comparisons and makes it a dataframe
try <- rbind(postMLR1, postMLR2, postRF, postBT) %>% data.frame()

# Changes the row names to the names of the different models created above
rownames(try) <- modelNames

# Chooses the winner of the models by finding minimum RMSE
winner <- try %>% filter(RMSE == min(RMSE))

# Prints out the minimum
print(paste0("The winning model is the ", rownames(winner)))
```

    ## [1] "The winning model is the Random Forest Model"
