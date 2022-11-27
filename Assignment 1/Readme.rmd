---
title: "Assignment 1"
author:"
  - Mohammad Ahmed
  - Shahana Ayobi"
date: '2022-11-25'
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# 1. Introduction
We are interested to analyze the gender pay gap among **Financial Managers (CC: 0120)** using the Current Population Survey (CPS) from 2014.

# 2. Data Cleaning
To continue with our analysis, we filtered the data to only Financial Managers occupation and restricted ages from 15-64 since these ages are classified as working age in the U.S.. We then generated a binary variable `female` which takes the value 1 if the observation is female and 0 otherwise where we have 792 observations for women and 607 for men. Other transformations, such as creating `hourly wage`, and `logarithm of hourly wage`, restricting the `hourly wage` to the value 1 or more have been performed.
The education variable (`grade92`) have been categorized into 5 groups: Associate, Bachelor's, Master's, Professional School, and Doctorate degrees.

# 3. Analysis
The unconditional gender pay gap can be observed from `Table 1` that provides a summary of `female` and `hourly wage` variables. On average, women earn USD9.4 equivalent to 29% less than men and this gap is more apparent in the higher hourly wage percentile. The same result can be seen from running simple linear regressions and OLS robust regressions (`Table 2`).

  To determine the wage disparity according to various degrees of education, we ran 5 log-level multivariate regressions accounting for heteroskedastic errors and changed educational base category for each regression. While running the simple regression in `Table 3`, `model 1` shows that women earn 29.3% less than men on average, when conditioned on education levels taking Associate degree as the base category, this gap decreases to 19.9 % less earning than men on average (`Table 3, Model 2`), ceteris paribus. We also went on to investigate the discrepancy in pay between education levels. `Model 2` compares women with various levels of education, using the associate's degree as the base variable. For instance, female employees with bachelor's degree tend to earn 30.3% more on average compared to women with Associate degrees and it is significant at 1% significance level. The rest of the models follow a similar pattern where the coefficients show that higher levels of education result in a higher wage in females when changing the base categories for educational levels. However, `Table 3, Model 2` tends to be a better fit for our analysis when taking associate degree as the base where 19% variation in wage is explained by the explanatory variables in the model.
  
  We now run a regression with interaction terms on the same base variable (`Table 4`). Although the `female` variable is still significant, the interactions are not, meaning that having higher levels of education for women compared to men does not have effect on earnings. However, we will need higher number of observations and more relevant variables in the model to confirm this relationship.
  In conclusion, women earn less than men in the Financial Managers sectors compared to men, higher education levels yield more earnings compared to lower education levels. However, the result of interaction term suggest while having the same education levels men and women's earnings do not differ which is to be investigated further to confirm.

```{r include=FALSE}
rm(list=ls())
library(dplyr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(stargazer)
library(estimatr)
library(huxtable)
library(data.table)
```

```{r warning=FALSE, include=FALSE}
# Loading the data

data_all <- read_csv("https://osf.io/4ay9x/download")
sum(with(data_all, occ2012==0120))

```

```{r include=FALSE}
# Keeping only the "Financial Managers" occupation for ages 15-64

data <- data_all %>% filter(occ2012==0120 & age >= 15 & age<=64)
```

```{r include=FALSE}
# Generating female, hourly wage, and log hourly wage variables

data <- data %>% mutate(female=as.numeric(sex==2)) %>%
  mutate(wage=earnwke/uhours) %>%
  mutate(lnwage=log(wage))

```

```{r include=FALSE}
# Summary of earnwke,uhours, and wage, here the wage and earnwke is less than zero

data %>% dplyr::select(earnwke,uhours,wage) %>% summary()
```

```{r include=FALSE}
# Filtering wage to more than one

data <- data %>% filter(wage>=1)

```

```{r echo=FALSE, fig.show='hold', fig.align='center', fig.pos="H"}
# Summary of wages for each gender

datasummary(as.factor(female)*wage ~ Mean + SD + Min + Max + P25 + P75 + N , data = data, title = "Summary of Wages for Both Genders")
```

```{r echo=FALSE}
# Running simple regressions for wage and log of wage

# plain SE
reg1<-lm(lnwage~female,data) 
# with robust SE
reg2 <- lm_robust(lnwage ~ female, data = data, se_type = "HC1")
reg3<-lm(wage~female,data) 
# with robust SE
reg4 <- lm_robust(wage ~ female, data = data, se_type = "HC1")

msummary(list(reg1, reg2, reg3, reg4),
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         title = "Simple Regressions Result")

```

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Categorizing educational levels

data <- data %>% mutate(ed_Associate=as.numeric(grade92<=42 & grade92 >= 41), ed_BA=as.numeric(grade92==43), ed_MA=as.numeric(grade92==44),
ed_Profess = as.numeric(grade92==45), ed_PhD = as.numeric(grade92==46))

# Graphing the Non-parametric regression: education levels and log wage

graph1 <- ggplot(data = data, aes(x = grade92, y = lnwage)) +
  geom_point(color = "#3a5e8cFF") + 
  geom_smooth(method="loess", color = "#10a53dFF", formual = 'y ~ x') +
  scale_x_continuous(expand=c(0.01, 0.01), limits = c(40.5, 46.5),   breaks=seq(40, 47,   by=1)) + 
  scale_y_continuous(expand=c(0.01, 0.01),limits = c(1.5, 4.5), breaks=seq(1.5, 4.5, by=0.50)) +
  labs(x = "Education levels (Financial managers)",y = "ln(earnings per hour)")+
  theme_light() +
  ggtitle("Figure 1: Non-parametric Regression - Lnwage ~ Education Levels")
graph1

```

```{r include=FALSE}
# Running regressions taking different base categories of education levels

reg1 <- lm_robust(lnwage ~ female, data=data,se_type = "HC1")
reg5 <- lm_robust(lnwage ~ female + ed_BA + ed_MA + ed_Profess + ed_PhD, data=data,se_type = "HC1")
reg6 <- lm_robust(lnwage ~ female + ed_Associate + ed_MA + ed_Profess + ed_PhD, data=data,se_type = "HC1")
reg7 <- lm_robust(lnwage ~ female + ed_Associate + ed_BA + ed_Profess + ed_PhD, data=data,se_type = "HC1")
reg8 <- lm_robust(lnwage ~ female + ed_Associate + ed_BA + ed_MA + ed_PhD, data=data,se_type = "HC1")
reg9 <- lm_robust(lnwage ~ female + ed_Associate + ed_BA + ed_MA + ed_Profess, data=data,se_type = "HC1")
```

```{r echo=FALSE}
# Displaying the models in huxtable

huxreg(reg1, reg5, reg6, reg7, reg8, reg9,statistics = c(N = "nobs", R2 = "r.squared"))
```

```{r echo=FALSE}
# Variable female interaction with education levels

reg11 <- lm_robust(lnwage ~ ed_BA + ed_MA + ed_Profess + ed_PhD, data=data %>% filter(female==1), se_type = "HC1")
reg12 <- lm_robust(lnwage ~ ed_BA + ed_MA + ed_Profess + ed_PhD, data = data %>% filter(female==0), se_type = "HC1")
reg13 <- lm_robust(lnwage ~ ed_BA + ed_MA + ed_Profess + ed_PhD + female*ed_BA + female*ed_MA + female*ed_Profess + female*ed_PhD, data=data, se_type = "HC1")
huxreg(reg11, reg12, reg13,statistics = c(N = "nobs", R2 = "r.squared"))
```

