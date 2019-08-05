#' 03_data_manipulation.R
#' This is the lab file for the Telethon Kids Institiute Introduction to R
#' workshop session Data Manipulation.
#' Last updated by Paul Stevenson on 11 April 2019
#' 





#### Running code ----

#' To execute a section of code, highlight the desired chunk and press Ctrl+Enter
#' To execute the entire R Script, press Ctrl+Shift+Enter





#### Import libraries ----

# the full tidyverse can loaded with library(tidyverse)

library(dplyr)
library(lubridate)
library(readr)
library(tidyr)





#### Getting help ----

#' if at any point you need help with a function, type "?<function name>" into
#' the console, for example: ?read_csv





#### Read in data ----

# Read in a CSV files

read_csv("data/demo.csv")

# Read in a RData file - notice that this can contain multiple data sets

load("data/dat.RData")





#### Expand on classes (data/time [lubridate - year], string processing) ----

# Vectors - single class

vec_character <- c("One", "Two", "Three")

vec_integer <- c(1, 2, 3)

vec_logical <- c(T, T, F)

# data frame of vectors that may have different classes

df <- tibble(
  character = c("One", "Two", "Three"),
  integer = 1:3,
  numeric = integer * 1.0,
  logical = c(T, T, F),
  factor = factor(c("dog", "cat", "Dog"), levels = c("dog", "Dog", "cat", "Cat"), labels = c("Woof", "Woof", "Meow", "Meow")),
  date = ymd(c("2019-04-11", "2019-05-11", "2019-06-11")),
  missing = NA
)

# Vector class

class(vec_logical)

class(df$date)

# Summary of data set

str(df)

# Dates - lubridate can import dates in several different formats
# All dates should be in the same format!

dmy("16/02/1985")
mdy("Feb 16 1985")
ymd("1985-February 16")

ymd_hms("1985/02/05 12:30:00", tz = "Australia/Perth")

# format can be used to change the date format - for example printing in tables/figuers

format(tki_demo$dob, "%d %b %Y") %>%
  head()




#### Summarise/tidy (cleaning) ----

summary(tki_demo)




#### Filter ----

tki_demo %>%
  filter(dob > ymd("2005-01-01"), smoker, intervention == "Drug 2")





#### Select ----

tki_demo %>%
  select(id, dob, intervention)

tki_demo %>%
  select(-dob, -day1)





#### Mutate (case_when, if/else) ----

tki_demo %>%
  mutate(age = interval(dob, Sys.Date()) %>% as.duration() %>% as.numeric("years"), # using the lubridate package
         teenager = ifelse(age >= 12, T, F), # ifelse
         age_cat = case_when( # case_when (nested if/else)
           age < 5 ~ "Younger than 5 years old",
           age < 10 ~ "5 - 9 years old",
           age < 15 ~ "10 - 14 years old",
           age >= 15 ~ "Older than 15 years",
           T ~ NA_character_
         )) %>%
  select(id, dob, age, teenager, age_cat)





#### Subset/merge/join ----

tki_demo_join <- left_join(tki_demo,
          tki_demo_complications,
          by = "id")

tki_demo_join %>%
  filter(!is.na(complications))





#### Gather/spread/melt/cast/separate/reshape ----

# Gather - wide to long

tki_demo_long <- tki_demo %>%
  gather(key = day, value = score, -id, -dob, -intervention, -male, -smoker)

# Spread - long to wide (back to original format)

tki_demo_long %>%
  spread(key = day, value = score)





#### Summarise ----

tki_demo %>%
  summarise(n = n(),
            day1_mean = mean(day1, na.rm = T),
            day2_median = median(day2, na.rm = T),
            day3_sd = sd(day3, na.rm = T))

# Summarise multiple columns

tki_demo %>%
  summarise_at(c("day1", "day2", "day3"), mean, na.rm = T)

# Summarise by a single group

tki_demo %>%
  group_by(intervention) %>%
  summarise(mean = mean(day1, na.rm = T),
            sd = sd(day1, na.rm = T))

# Summarise by multiple groups

tki_demo %>%
  group_by(intervention, smoker) %>%
  summarise(mean = mean(day1, na.rm = T),
            sd = sd(day1, na.rm = T))




#### Reshaping data ----

# remotes::install_github("tidyverse/tidyr")
library(tidyr)


# Wide to long format (preferred data structure)

tmp <- tki_demo %>%
  pivot_longer(cols = starts_with("day"), # i.e. c("day1", "day2", "day3")
               names_to = "day",
               values_to = "score",
               names_prefix = "day", # regular expression to remove matching text
               names_ptypes = list(day = integer()))

# Long to wide

tmp %>%
  pivot_wider(names_from = day,
              values_from = score,
              names_prefix = "day")

# Unite multiple columns into one

(united <- tmp %>%
    unite("treatment", day, intervention, sep = "."))

# Separate a column that holds multiple pieces of information into many

united %>%
  separate(treatment, c("day", "intervention"), sep = "\\.")
  # Note the "\\" - . is a special character in regular expressions




#### String Manipulation ----

library(stringr)

tibble(patient = 1:5,
       market_name = c("Humira", "Eliquis", "Revlimid", "Keytruda", "Eliquis"),
       name = c("adalimumab", "apixaban", "lenalidomide", "pembrolizumab", "apixaban")) %>%
  mutate(apixaban = str_detect(.data$name, "^ap.*$"))





#### Functions in R ----

adder <- function(x, y, z) x + y + z

adder(5, 17, -1)





#### Applying functions (apply/tidyverse) ----

tki_demo %>%
  mutate(total = adder(day1, day2, day3))

# apply the same function to multiple columns

square <- function(x) ifelse(lubridate::is.Date(x), x, x^2)

tki_demo %>%
  mutate_at(c("day1", "day2", "day3"), list(~square(.)))

# apply function to columns that meet a criteria

tki_demo %>%
  mutate_if(is.numeric, list(~square(.)))

# apply a function to data frame subsets

tki_demo %>%
  split(f = tki_demo$intervention) %>%
  lapply(function(x) {
    # x is each split element of the data frame, which gets acted on one at a time
    # the last item is returned
    
    x2 <- x %>%
      mutate(new = ifelse(male & smoker, day1, day2 + day3))
    
    x2
    
  }) %>%
  bind_rows() # combine split data frame back into one




#### Test Your Skills

#' With the tki_demo and tki_demo_complications data sets, for each participant,
#' find the first time the "Man Flu" complication was observed by participants
#' taking Drug 1 and a measurement greater than 15.0 was observed and return the
#' participants id, intervention, gender, trial day, and the measurement.
#' (note, only males can suffer from Man Flu virus)
