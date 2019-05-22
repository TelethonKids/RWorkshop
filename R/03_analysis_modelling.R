#' 03_analysis_modelling.R
#' This is the lab file for the Telethon Kids Institiute Introduction to R
#' workshop session Analysis and Modelling.
#' Last updated by Seb Rauschert on 22 May 2019
#' 


#### Running code ----

#' To execute a section of code, highlight the desired chunk and press Ctrl+Enter
#' To execute the entire R Script, press Ctrl+Shift+Enter


#### Getting help ----

#' if at any point you need help with a function, type "?<function name>" into
#' the console, for example: ?read_csv


#### Import libraries ----
library(biometrics)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)
library(ggpubr)
library(ggfortify) #autoplot function, see below in diagnostics
theme_set(theme_classic())

#### Read in data ----
# Read in a CSV files

demoData <- read_csv("data/demo.csv")


#### Summary of the data

summary(demoData)


#### Getting to kow the distribution of variables ----
#' Here we first focus on the day1 - day3 variables
#' Boxplot the data to look for outlier

demoData %>%
  gather(Days, measurement, day1:day3, factor_key=TRUE) %>%
  ggplot(aes(y=measurement,x=Days, fill=Days)) +
  labs(title = "Days: 1 to 3 with outlier", x = "", y = "Measurment") +
  geom_boxplot() +
  scale_color_telethonkids("light") +
  theme_minimal() 

#' Q: What do you see in this exercise? how do you handle the observation?

#' A: We need to exclude this definite outlier. It is so far away from any other data point, that it
#' is very likely a measurment error rather than a misclassification


#### Remove the outlier: ----
#' here we can see that no value is above 50, so we can use dplyr
#' to filter the data set for values that are < 50 days and store the results in 
#' a new data frame, so we don't override the full data set. After that, we plot
#' the data in a boxplot again

no_out <- demoData %>%
  filter(day2 < 100)
  

no_out %>%
  gather(Days, measurement, day1:day3, factor_key=TRUE) %>%
  ggplot(aes(y=measurement,x=Days, fill=Days)) +
  labs(title = "Days: 1 to 3 outlier removed", x = "", y = "Measurment") +
  geom_boxplot() +
  scale_color_telethonkids("light") +
  theme_minimal() +
  


#'Now we use the quantile-quantile plot (QQ plot) of the variables to check for normal distribution of the data
no_out %>%                                                  # Select the demo data and utilise the pipe
  gather(Days, measurement, day1:day3, factor_key=TRUE) %>%   
  ggplot(aes(sample=measurement)) + 
  stat_qq() +
  stat_qq_line() +  # This ads a line to the plot to better judge the deviation
  theme_minimal() +
  facet_wrap(~Days, nrow = 1) 
  
#' We want the plots to be aproximately on one line in order to show some normality

#### Let's plot the three day measurements against each other to see if there is a tendency of
#' association. ----

plot_day1_day2 <- 
  no_out %>%
  ggplot(aes(x=day1, y=day2)) +
  labs(title = "Day 1 and Day 2", x = "day 1", y = "day 2") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()

plot_day2_day3 <- 
  no_out %>%
  ggplot(aes(x=day2, y=day3)) +
  labs(title = "Day 2 and Day 3", x = "day 2", y = "day 3") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()

plot_day1_day3 <- 
  no_out %>%
  ggplot(aes(x=day1, y=day3)) +
  labs(title = "Day 1 and Day 3", x = "day 1", y = "day 3") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()



grid.arrange(plot_day1_day2, plot_day2_day3, plot_day1_day3, nrow=2, ncol=2)

###Linear regression model ----
#' In the plots, we can see that day 2 and day 3 seem to be correlated. Let's perform a linear model 
#' and see if we can find a significant association

lm(day3 ~ day2, data=no_out)

#' This function alone does not tell you anything about significane. We need to use the wrapper function "summary"
#' to get more information about the model. Therefore, we store the model in a variable:

model_da2_day3 <- lm(day3 ~ day2, data=no_out)

#' Once we have done this, we can use the model name and query it for more information
#' Let's use teh summary() function to get the p-value and R and R2 values, as well as
#' other relevant model statistics

summary(model_da2_day3)

#' What do you see in the R output?



#### Model Diagnostic plots ----
#' With base R it is very easy to look at the model diagnostic plots.
#' just wrap the function plot() around your variable that you created to store the model

plot(model_da2_day3)

#' R will prompt you now to hit return to go through all the plots. Anicer way, in line with the tidy
#' way of coding, is by simpply wrapping the "autoplot()" function around the model.

autoplot(model_da2_day3)

#' Here you get all the plots on one page and it looks nice and tidy if you want to include it
#' in a publication.



#### Multiple Linear regression ----
#' The function for the multiple linear regression is the same as for the simple linear regression.
#' It is "lm()". But now we add as many variables as we want with "+" on the right side of the 
#' equation: "lm(y ~ x + covar1 + covar2 + ... + covar3, data=data)"
#' 
#' Let's try it in our example data. Check the available variables again:

names(no_out)

#' We have three variables that might be interesting in the model: sex, as represented in the "males"
#' variable, "intervention" and "smoker". Let's start with adding one by one and look at the results

modelIntervention <- lm(day3 ~ day2 + intervention, data=no_out)
summary(modelIntervention)

#' As we can see, the intervention is highly significantly associated with the day3 value and the day1 
#' value is no longer significant in the model.
#' Let's try and visualise this, by coloring the intervention variable in a scatterplot

no_out %>%
  ggplot(aes(x=day3, y=day2, col=intervention)) +
  geom_point()

#' In the plot you can see that there are now three clusters. The day2 and day3 values are actually
#' dependent on the intervention group. We can visualise this in a boxplot to show the different levels

no_out %>%
  ggplot(aes(x=intervention, y=day3,  col=intervention)) +
  geom_boxplot()

#' Let's see if sex also influences the association between day 2 and day3

modelSex <- lm(day3 ~ day2 + male, data=no_out)
summary(modelSex)

#' As we can see, the sex does not matter in the association between the day 2 and day3 values.
#' Now let's check the last variabel, smoking status

modelSmoker <- lm(day3 ~ day2 + smoker, data=no_out)
summary(modelSmoker)

#'Smoking status is also not changeing the association between day2 and day3.



#### Logistic regression ----
#'For the logistic regression, we have to switch to the function for a "generalised linear model"
#'in R: "glm()". To specify that we are indeed interested in a logistic regression, we have to set the
#'family to binomial(link='logit'). Let's see if being a smoker is associated with being male.

logModelSmoker <- glm(smoker ~ male, data=no_out, family=binomial(link='logit'))
summary(logModelSmoker)




