---
title: "Lab3"
author: "Your Name"
date: "2024-09-19"
output: "github_document"
---

# Load Packages
```{r}
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(bruceR)
library(ggsci)
library(see)
library(car)
library(readr)

```

# Load Datasets
```{r}

dataset <- read_csv("~/Desktop/psy329 datasets/FOMO - Social Media/Data file Einstein.csv")

```

# Unnecessary Data
```{r}

# Deleting Origin data
dataset <- dataset %>% select(-Origin_0)

# I don't need the origins of participants and because most are Australian, it is marked NA. 
# I can't perform listwise deletion otherwise.

```

# Missing Data
```{r}

#Listwise deletion
list_dataset<- drop_na(dataset)

#pair wise deletion will only be used when you run the actual analysis using "na.rm = TRUE"

```

# The dataset is already recoded (see Gender, Australian etc.).
# Scores are calculated as sums of items (see SCAS_TOTAL etc)
# Reverse scales are already corrected (see SCS Reversed)

# Normality
## Normality Plots
```{r}


ggplot(list_dataset, aes(x = FoMO_Total)) + geom_histogram(binwidth = 2) + theme_classic()

ggplot(list_dataset, aes(x = FoMO_Total)) + geom_density(adjust = 2)  + theme_classic()

qq<-ggplot(list_dataset, aes(sample = FoMO_Total)) + geom_qq()  + theme_classic()

qq+ geom_qq_line()


#functions below are base R functions, they can do the job, but not as customizable as ggplot

hist(list_dataset$FoMO_Total)

qqnorm(list_dataset$FoMO_Total, col = "steelblue", lwd = 2)

plot(density(list_dataset$FoMO_Total, na.rm = TRUE, bw = 90),  lwd=2, main = "")

```

### Normality Plots by Anxiety
```{r}

# I couldn't plot a violin graph. Can't figure out how to.

#Find a way to plot the histograms, density, and qq plots by groups using ggplot

ggplot(list_dataset, aes(x = FoMO_Total)) + geom_histogram(binwidth = 2) + facet_wrap(~SCAS_TOTAL_0) + theme_classic()

ggplot(list_dataset, aes(x = FoMO_Total)) + geom_density(adjust = 2)  + facet_wrap(~SCAS_TOTAL_0) + theme_classic()

qq<-ggplot(list_dataset, aes(sample = FoMO_Total)) + geom_qq()  + facet_wrap(~SCAS_TOTAL_0) + theme_classic()

qq+ geom_qq_line()

```

## Normality Tests
```{r}

describe(list_dataset$FoMO_Total)

shapiro.test(list_dataset$FoMO_Total)

```

### Normality Tests by Anxiety
```{r}

#Use the describeBy() function to get skewness and kurtosis by group
?describeBy()
describeBy(FoMO_Total ~ SCAS_TOTAL_0, data = list_dataset)

#Use the group by function to get shapiro test results by group

list_dataset %>%
  group_by(SCAS_TOTAL_0) %>%
  filter(n() >= 3) %>%  # Exclude groups with fewer than 3 observations
  summarize(W = shapiro.test(FoMO_Total)$statistic, 
            p_value = shapiro.test(FoMO_Total)$p.value)

#Use the filter function to get both


```

# Equal Variance between Groups
## Descrptive Variance
```{r}
list_dataset_clean <- drop_na(list_dataset)
var(list_dataset_clean$FoMO_Total)

list_dataset_clean %>%
  group_by(SCAS_TOTAL_0) %>%
  summarize(variance = var(FoMO_Total))

```

## Equal Variance Test
```{r}

#I couldn't figure out how to do this without having a categorical variable as IV

```

# Transformation
```{r}

#if any of the assumption is not met, use transformation 

list_dataset$FoMO_Total_log <- log10(list_dataset$FoMO_Total)

ggplot(list_dataset, aes(x = FoMO_Total_log)) + geom_histogram(binwidth = .2) + theme_classic()

ggplot(list_dataset, aes(x = FoMO_Total_log)) + geom_density(adjust = 2)  + theme_classic()

qq<-ggplot(list_dataset, aes(sample = FoMO_Total_log)) + geom_qq()  + theme_classic()

qq+ geom_qq_line()


ggplot(list_dataset, aes(x = FoMO_Total_log)) + geom_histogram(binwidth = .2) + facet_wrap(~SCAS_TOTAL_0) + theme_classic()

ggplot(list_dataset, aes(x = FoMO_Total_log)) + geom_density(adjust = 2)  + facet_wrap(~SCAS_TOTAL_0) + theme_classic()

qq<-ggplot(list_dataset, aes(sample = FoMO_Total_log)) + geom_qq()  + facet_wrap(~SCAS_TOTAL_0) + theme_classic()

qq+ geom_qq_line()

```

```{r}

#Normality Tests
describe(list_dataset$FoMO_Total_log)
shapiro.test(list_dataset$FoMO_Total_log)

#Use the describeBy() function to get skewness and kurtosis by group
?describeBy()
describeBy(FoMO_Total_log ~ SCAS_TOTAL_0, data = list_dataset)
```
