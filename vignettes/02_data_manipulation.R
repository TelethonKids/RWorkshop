## ----init, include = FALSE, echo = FALSE---------------------------------
library(knitr)
library(biometrics)
library(lubridate)
library(tidyverse)
library(kableExtra)

source("assets/R/hooks.R")

load("../data/dat.RData")


## ---- eval = F, out------------------------------------------------------
#  install.packages("tidyverse")
#  

## ---- echo = F, out.extra = "figure"-------------------------------------
include_graphics("assets/images/02_tidyverse.png")


## ---- eval = F-----------------------------------------------------------
#  library(dplyr)
#  library(readr)
#  library(tidyr)
#  # OR
#  library(tidyverse)
#  
#  library(lubridate)
#  

## ---- eval = F-----------------------------------------------------------
#  read_csv("data/demo.csv")
#  

## ---- eval = F-----------------------------------------------------------
#  ?read_delim
#  

## ---- eval = F-----------------------------------------------------------
#  load("data/demo.RData")
#  
#  readRDS("path/to.file")
#  

## ---- eval = F-----------------------------------------------------------
#  library(readxl)
#  read_xlxs("path/to/file.xlsx")
#  

## ---- eval = F-----------------------------------------------------------
#  library(readstata13)
#  readstata13("path/to/file.sta")
#  

## ---- results = "hide"---------------------------------------------------
vec_character <- c("One", "Two", "Three")

vec_integer <- c(1, 2, 3)

vec_logical <- c(T, T, F)


## ---- results = "hide"---------------------------------------------------
df <- tibble(
  character = c("One", "Two", "Three"),
  integer = 1:3,
  numeric = integer * 1.0,
  logical = c(T, T, F),
  factor = factor(c("dog", "cat", "Dog"),
                  levels = c("dog", "Dog", "cat", "Cat"),
                  labels = c("Woof", "Woof", "Meow", "Meow")),
  date = ymd(c("2019-04-11", "2019-05-11", "2019-06-11")),
  missing = NA
)


## ---- echo = F, comment = NA---------------------------------------------
df


## ---- comment = NA-------------------------------------------------------
class(vec_logical)


## ---- comment = NA-------------------------------------------------------
class(df$date)


## ---- comment = NA-------------------------------------------------------
str(df)


## ---- results = "hide"---------------------------------------------------
library(lubridate)


## ---- results = "hide"---------------------------------------------------
dmy("16/02/1985")
mdy("Feb 16 1985")
ymd("1985-February 16")

## ---- echo = F, comment = NA---------------------------------------------
dmy("16/02/1985")


## ---- comment = NA-------------------------------------------------------
ymd_hms("1985/02/05 12:30:00", tz = "Australia/Perth")


## ----date_output_format, comment = NA------------------------------------
format(tki_demo$dob, "%d %b %Y") %>%
  head(3)


## ----summary_data, comment = NA, class = "small"-------------------------
summary(tki_demo[, c("male", "day1", "day2", "day3")])


## ----eval = F------------------------------------------------------------
#  library(dplyr)
#  

## ---- eval = F-----------------------------------------------------------
#  Data frame %>% filter() %>% select() %>%
#    mutate() %>% summarise() %>% view()
#  

## ----filter, comment = NA------------------------------------------------
tki_demo %>%
  filter(dob > ymd("2005-01-01"),
         smoker,
         intervention == "Drug 2") %>%
  head()


## ----select_include, comment = NA----------------------------------------
tki_demo %>%
  select(id, dob, intervention) %>%
  head()


## ----select_exclude, comment = NA----------------------------------------
tki_demo %>%
  select(-dob, -day1) %>%
  head()


## ----mutate_ifelse, eval = F---------------------------------------------
#  tki_demo %>%
#    mutate(
#      age = interval(
#        dob,
#        Sys.Date()) %>%
#          as.duration() %>%
#          as.numeric("years"),
#  
#      teenager = ifelse(age >= 12, T, F)) %>%
#  
#    select(id, dob, age, teenager) %>%
#    head()
#  

## ----mutate_ifelse_2, echo = F, comment = NA-----------------------------
(tmp <- tki_demo %>%
  mutate(
    age = interval(
      dob,
      Sys.Date()) %>%
      as.duration() %>%
      as.numeric("years"),
    teenager = ifelse(age >= 12, T, F)) %>%
  select(id, dob, age, teenager) %>%
  head())


## ----mutate_casewhen, eval = F-------------------------------------------
#  tmp %>%
#    mutate(age_cat = case_when(
#      age < 5 ~ "Younger than 5 years old",
#      age < 10 ~ "5 - 9 years old",
#      age < 15 ~ "10 - 14 years old",
#      age >= 15 ~ "Older than 15 years",
#      T ~ NA_character_
#    )) %>%
#    select(id, dob, age, age_cat)
#  

## ----mutate_casewhen_2, echo = F, comment = NA---------------------------
tmp %>%
  mutate(age_cat = case_when(
    age < 5 ~ "Younger than 5 years old",
    age < 10 ~ "5 - 9 years old",
    age < 15 ~ "10 - 14 years old",
    age >= 15 ~ "Older than 15 years",
    T ~ NA_character_
  )) %>%
  select(id, dob, age, age_cat) %>%
  head()


## ----join, eval = F------------------------------------------------------
#  tki_demo %>%
#    left_join(tki_demo_complications,
#              by = "id") %>%
#    filter(!is.na(complications)) %>%
#    head()
#  

## ----join_2, echo = F, comment = NA--------------------------------------
tki_demo %>%
  left_join(tki_demo_complications,
            by = "id") %>%
  filter(!is.na(complications)) %>%
  head()


## ----summarise, comment = NA---------------------------------------------
tki_demo %>%
  summarise(n = n(),
            day1_mean = mean(day1, na.rm = T),
            day2_median = median(day2, na.rm = T),
            day3_sd = sd(day3, na.rm = T))


## ----summarise_multiple, comment = NA------------------------------------
tki_demo %>%
  summarise_at(c("day1", "day2", "day3"), mean, na.rm = T)


## ----summarise_single_group, comment = NA--------------------------------
tki_demo %>%
  group_by(intervention) %>%
  summarise(mean = mean(day1, na.rm = T),
            sd = sd(day1, na.rm = T))


## ----summarise_multiple_groups, eval = F---------------------------------
#  tki_demo %>%
#    group_by(intervention, smoker) %>%
#    summarise(mean = mean(day1, na.rm = T),
#              sd = sd(day1, na.rm = T))
#  

## ----summarise_multiple_groups_2, echo = F, comment = NA-----------------
tki_demo %>%
  group_by(intervention, smoker) %>%
  summarise(mean = mean(day1, na.rm = T),
            sd = sd(day1, na.rm = T))


## ---- eval = F, comment = NA---------------------------------------------
#  remotes::install_github("tidyverse/tidyr")
#  library(tidyr)
#  

