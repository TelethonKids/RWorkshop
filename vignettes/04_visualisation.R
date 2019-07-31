## ----init, include = FALSE, echo = FALSE---------------------------------
library(biometrics)
library(lubridate)
library(tidyverse)
library(kableExtra)

## ----read_csv_options, echo = F------------------------------------------
tibble(
  Option = c("col_names", "na", "trim_ws", "skip", "n_max", "guess_max", "skip_empty_rows"),
  Description = c("If TRUE, the first row of the input will be used as the column names, and will not be included in the data frame.",
                  "Character vector of strings to interpret as missing values.",
                  "Should leading and trailing whitespace be trimmed from each field before parsing it?",
                  "Number of lines to skip before reading data.",
                  "Maximum number of records to read.",
                  "Maximum number of records to use for guessing column types.",
                  "Should blank rows be ignored altogether?")) %>%
  mutate(Option = paste0("<div style = 'font-size: 0.5em;'><code>", Option, "</code></div>"),
         Description = paste0("<div style = 'font-size: 0.5em;'>", Description, "</div>")) %>% # add <code> tag to all options
  kable("html", escape = F) %>%
  kable_styling("hover", full_width = F)


## ----data----------------------------------------------------------------
load("../data/dat.RData")


## ----str_data------------------------------------------------------------
str(tki_demo)


## ----summary_data--------------------------------------------------------
summary(tki_demo)


## ----head_data-----------------------------------------------------------
head(tki_demo)


## ----ifelse--------------------------------------------------------------
tki_demo %>%
  mutate(age = interval(dob, Sys.Date()) %>% as.duration() %>% as.numeric("years") %>% round(1),
         teenager = ifelse(age >= 13, T, F)) %>%
  select(id, dob, age, teenager) %>%
  head()


## ----case_when, eval = F-------------------------------------------------
#  tki_demo %>%
#    mutate(age = interval(dob, Sys.Date()) %>%
#             as.duration() %>%
#             as.numeric("years") %>%
#             round(1),
#           age_category = case_when(
#             age < 12 ~ "Younger than 12 years",
#             age < 14 ~ "12 - 13 years",
#             age < 16 ~ "14 - 15 years",
#             age < 18 ~ "15 - 17 years",
#             T ~ "Older than 18 years"
#           ),
#           factor(age_category,
#                  levels = c("Younger than 12 years","12 - 13 years",
#                             "14 - 15 years", "15 - 17 years",
#                             "Older than 18 years"))) %>%
#    select(id, dob, age, age_category) %>%
#    head()
#  

## ----case_when_output, echo = F------------------------------------------
tki_demo %>%
  mutate(age = interval(dob, Sys.Date()) %>%
           as.duration() %>%
           as.numeric("years") %>%
           round(1),
         age_category = case_when(
           age < 12 ~ "Younger than 12 years",
           age < 14 ~ "12 - 13 years",
           age < 16 ~ "14 - 15 years",
           age < 18 ~ "15 - 17 years",
           T ~ "Older than 18 years"
         ),
         factor(age_category,
                levels = c("Younger than 12 years","12 - 13 years",
                           "14 - 15 years", "15 - 17 years",
                           "Older than 18 years"))) %>%
  select(id, dob, age, age_category) %>%
  head()


## ----funciton------------------------------------------------------------
square <- function(x) {
  x^2
}

square(4)


## ---- eval = F-----------------------------------------------------------
#  day1_mean <- function(x) {
#    tibble(id = x$id,
#           day1_mean = mean(x$day1, na.rm = T)
#    )
#  }
#  
#  tki_demo %>%
#    split(.$id) %>%
#    lapply(day1_mean) %>%
#    bind_rows()
#  

## ---- echo = F-----------------------------------------------------------
day1_mean <- function(x) {
  tibble(id = x$id,
         day1_mean = mean(x$day1, na.rm = T)
  )
}

tki_demo %>%
  split(.$id) %>%
  lapply(day1_mean) %>%
  bind_rows()


