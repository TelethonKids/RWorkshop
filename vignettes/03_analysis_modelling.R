## ----init, include = FALSE, echo = FALSE---------------------------------
library(biometrics)
library(datasets)
library(knitr)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)
library(ggpubr)
library(broom)
library(jtools)
library(tidyr)

data(iris)



## ----echo=FALSE, error=FALSE, message=FALSE, warning=FALSE, out.extra = 'class="centre" style="width: 500px;"', warnings=FALSE----
setwd("/Users/srauschert/Desktop/Work/20.) Git_GitHub/RWorkshop/")
tki_demo <- read_csv("data/demo.csv")

tki_demo %>%
  filter(day2 < 100) %>%
  

ggplot( aes(day2, day3)) +
  labs(title = "TKI Dataset", x = "day1", y = "day2") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()



## ---- echo = FALSE, out.extra = 'class="centre" style="width: 100px;"',warning=FALSE----
#kable(summary(tki_demo[,c(6:8)]))

summary(tki_demo[,c(6:8)])



## ---- echo=TRUE, out.extra = 'class="centre" style="width: 700px;"', warning=FALSE----
tki_demo %>%
  filter(day2 < 100) %>%
  gather(Days, measurement, day1:day3, factor_key=TRUE) %>%
  ggplot( aes(sample=measurement, color=Days)) + stat_qq() + facet_wrap(~Days)



## ----echo = FALSE, out.extra = 'class="centre" style="width: 700px;"',warning=FALSE----

with_out <- tki_demo %>%
  #filter(day2 < 100) %>%
  gather(Days, measurement, day1:day3, factor_key=TRUE) %>%
  ggplot(aes(y=measurement,x=Days, fill=Days)) +
  labs(title = "Days: 1 to 3 with outlier", x = "", y = "Measurment") +
  geom_boxplot() +
  scale_color_telethonkids("light") +
  theme_minimal()

no_out <- tki_demo %>%
  filter(day2 < 100) %>%
  gather(Days, measurement, day1:day3, factor_key=TRUE) %>%
  ggplot(aes(y=measurement,x=Days, fill=Days)) +
  labs(title = "Days: 1 to 3 outlier removed", x = "", y = "Measurment") +
  geom_boxplot() +
  scale_color_telethonkids("light") +
  theme_minimal()


ggarrange(with_out, no_out, ncol=2, common.legend = TRUE, legend=FALSE )



## ---- echo = FALSE, out.extra = 'class="centre" style="width: 700px;"',warning=FALSE----
data(iris)
plot1 <- ggplot(iris, aes(Petal.Width, Petal.Length)) +
  labs(title = "Petal", x = "Petal Width", y = "Petal Length") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()

plot2 <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  labs(title = "Sepal", x = "Sepal Width", y = "Sepal Length") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()

plot3 <- ggplot(iris, aes(Petal.Width, Sepal.Width)) +
  labs(title = "Petal and Sepal", x = "Petal Width", y = "Sepal Width") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()

plot4 <- ggplot(iris, aes(Petal.Length, Sepal.Length)) +
  labs(title = "Petal and Sepal", x = "Petal Length", y = "Sepal Length") +
  geom_point(size = 4) +
  geom_smooth(method='lm')+
  scale_color_telethonkids("light") +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)



## ---- message=FALSE,  warning=FALSE, error=FALSE, echo = FALSE,out.extra = 'class="centre" style="width: 500px;"'----
#summary(lm(Petal.Length~Petal.Width, data=iris))
lm1 <- lm(Petal.Length~Petal.Width, data=iris)
library(sjPlot)
#tab_model(lm1, file="output.html")
summary(lm1)


## ------------------------------------------------------------------------
tidy(lm1)


## ----results = 'asis'----------------------------------------------------
export_summs(lm1)


## ----error=FALSE, message=FALSE, warning=FALSE, include=FALSE------------
library(tidyverse)
library(broom)
library(knitr)
theme_set(theme_classic())

model <- lm(Petal.Length~Petal.Width, data=iris)

model.diag.metrics <- augment(model)
kable(as.matrix(head(model.diag.metrics)), format='markdown')


## ---- echo=TRUE, warning=FALSE, error=FALSE------------------------------
library(ggfortify)
autoplot(model)


## ----eval=FALSE, include=FALSE-------------------------------------------
## ggplot(model.diag.metrics, aes(Petal.Length, Petal.Width)) +
##   geom_point() +
##   stat_smooth(method = lm, se = FALSE) +
##   geom_segment(aes(xend = Petal.Length, yend = .fitted), color = "red", size = 0.3)
## 


## ------------------------------------------------------------------------
summary(lm(day3 ~ day2 + male + intervention, data=tki_demo))


## ------------------------------------------------------------------------
export_summs(lm(day3 ~ day2 + male + smoker, data = tki_demo))


## ------------------------------------------------------------------------
lm1 <- lm(day3 ~ day2, data = tki_demo)
lm2 <- lm(day3 ~ day2 + male + smoker, data = tki_demo)
export_summs(lm1, lm2)


## ------------------------------------------------------------------------
plot_summs(lm1)


## ------------------------------------------------------------------------
plot_summs(lm1, lm2)


## ------------------------------------------------------------------------
plot_summs(lm1, lm2, coefs="day2")


## ------------------------------------------------------------------------
summ(lm2, scale = TRUE, vifs = TRUE, part.corr = TRUE, confint = TRUE, pvals = FALSE)$coeftable

