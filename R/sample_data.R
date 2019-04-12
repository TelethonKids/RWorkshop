#' This script defines sample data to be used in the biometrics package, and
#' R workshops
#' 
#' Created by Paul Stevenson 21 Feb 19
#'

library(lubridate)
library(simstudy)
library(dplyr)
library(tibble)
library(readr)
library(random)

set.seed(1724)

#### simualted data ----
def <- defData(varname = "dob", dist = "uniform", formula = "10;18")
def <- defData(def, varname = "male", dist = "binary", formula = 0.45)
def <- defData(def, varname = "smoker", dist = "binary", formula = 0.32)
def <- defData(def, varname = "intervention", dist = "categorical", formula = "0.33;0.33;0.34")
def <- defData(def, varname = "day1", dist = "normal", formula = "5", variance = 10)
def <- defData(def, varname = "day2", dist = "normal", formula = "7.5*intervention", variance = 10)
def <- defData(def, varname = "day3", dist = "normal", formula = "10*intervention", variance = 10)
tki_demo <- genData(100, def)

##### missing data ----
defM <- defMiss(varname = "day1", formula = 0, logit.link = F)
defM <- defMiss(defM, varname = "day2", formula = 0.05, logit.link = F)
defM <- defMiss(defM, varname = "day3", formula = 0.15, logit.link = F)
missMat <- genMiss(tki_demo, defM, idvars = "id")

##### Data wrangling ----
tki_demo <- genObs(tki_demo, missMat, idvars = "id") %>%
  as_tibble() %>%
  mutate(intervention = factor(intervention, levels = 1:3, labels = c("Placebo", "Drug 1", "Drug 2")),
         dob = ymd(floor_date(as.POSIXct(Sys.Date()) - dob * 365 * 24 * 60 * 60, unit = "day"))) %>%
  mutate_at(c("male", "smoker"), as.logical)

# create some outliers
tki_demo[tki_demo$id == 30,]$day2 <- 143.8187 # 14.38187
tki_demo[tki_demo$id == 60,]$day3 <- 0
tki_demo[tki_demo$id == 90,]$dob <- ymd("1899-02-25")

#### example plot ----
tki_demo %>%
  gather(key = day, value = score, -id, -dob, -intervention, -male, -smoker) %>%
  ggplot(aes(x = day, y = score, color = intervention, group = id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~male)

# second dataset that can be joined in - follow up data/complications
rand <- rnorm(20, mean = 50, sd = 20) %>%
  as.integer() %>%
  unique()

def <- defData(varname = "complications", dist = "categorical", formula = "0.25;0.25;0.25;0.25")
tki_demo_complications <- genData(100, def) %>%
  mutate(complications = factor(complications, levels = 1:4, labels = c("Belly ache", "Yoda speach", "Man flu", "Extreme sarcasm")),
         complications = as.character(complications)) %>%
  as_tibble() %>%
  filter(id %in% rand)

#### Save data ----
save(tki_demo, tki_demo_complications, file = "data/dat.RData")
write_csv(tki_demo, path = "data/demo.csv")
write_csv(tki_demo_complications, path = "data/demo_complications.csv")
