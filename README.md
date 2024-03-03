---
title: "NYPD Shooting Incident Report"
date: "2024-02-25"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Analysis:

The purpose of this report is to examine data of every shooting incident that occurred in NYC beginning in 2006. With this data, we can analyze the likelihood of shooting incidents to occur based on specific factors and variables provided by this dataset.


## Import the data:
First we will import the data.
```{r cars, message=FALSE}
library(readr)
data <- read_csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
```

## Tidy the data:

Then, we will select only the columns we need to form our plots. Then we will remove any extreme values or unknowns:

```{r load, message=FALSE}
library(tidyverse)
```

```{r process}
data2 <- data %>%
  select(INCIDENT_KEY, OCCUR_DATE, OCCUR_TIME, BORO, STATISTICAL_MURDER_FLAG, 
         PERP_AGE_GROUP, PERP_SEX, PERP_RACE, VIC_AGE_GROUP, VIC_SEX, 
         VIC_RACE, Latitude, Longitude) %>%
  filter(PERP_AGE_GROUP != "1020" & PERP_AGE_GROUP != "224" & PERP_AGE_GROUP != "940" & PERP_AGE_GROUP != "UNKNOWN" & PERP_AGE_GROUP != "(null)" & PERP_SEX != "(null)" & PERP_SEX != "U")
```

We can also extract specific variables from the data set in order to generate models, such as extracting months and years from time stamps.

```{r time}
```

## Visualize the data:
We will be examining the number of incidents based on sex and age. For sex, we will break down the counts based on boroughs.

```{r graph1}
graph1 <- ggplot(data2, aes(x = PERP_SEX)) +
  geom_bar() +
  labs(
    title = "Number of Incidents based on Sex",
    x = "Gender",
    y = "Number of Incidents"
  ) + facet_wrap(~ BORO) + theme_minimal()
graph1
```
We can observe that across all boroughs, men are much more likely to be involved in shooting incidents than women.

```{r graph2}
graph2 <- ggplot(data2, aes(x = PERP_AGE_GROUP)) +
  geom_bar() +  
  labs(
    title = "Number of Incidents based on Age Group",
    x = "Age Group",
    y = "Number of Incidents"
  ) + theme_minimal()
graph2
```

Based on our results, shooting incidents are more likely to occur among younger adults, with individuals ages 18-24 having the highest occurrence. Individuals who are 65+ or older are the least likely to be involved.

We can also break this down by borough to determine likelihood based on age group.

```{r graph4}
graph4 <- ggplot(data2, aes(x = PERP_AGE_GROUP)) +
  geom_bar() +
  labs(
    title = "Number of Incidents based on Age Group",
    x = "Age",
    y = "Number of Incidents"
  ) + facet_wrap(~ BORO) + theme_minimal()
graph4
```

We can see that the distribution is much more concentrated in boroughs like Queens and Staten Island, where perpetrators are more likely to be between ages 18-24 or 25-44. Across all boroughs, individuals who are 65+ years or older are the least likely to be perpetrators. 

We can also take into account what time of day incidences are more likely to occur. Gaining insight on the location and time where incidences are more likely to occur can help to keep residents safe and avoid specific areas during certain times of the day. 

```{r graph3}
g <- ggplot(data2, aes(x = OCCUR_TIME)) +
  geom_line(stat = "count") +
  labs(
    title = "Incidence Occurence rate based on hour",
    x = "Time of Day",
    y = "Count of Incidents"
  ) +
  theme_minimal()
g
``` 

Based on this model, we can observe that shooting incidences are more likely to occur at night time rather than day time, and that shooting incidences are most likely after 8pm.

## Generate a model:

We want to predict the chance of a shooting incident based on month and can generate a model to summarize this prediction.

```{r mode}
library(dplyr)
library(lubridate)
data$OCCUR_DATE <- mdy(data$OCCUR_DATE)
data$Month <- month(data$OCCUR_DATE)
model <- lm(INCIDENT_KEY ~ Month, data = data)
summary(model)
``` 

Our model indicates a positive coefficient for the month variable, meaning that the variable is statistically significant, with a p value of 8.384e-10. We can conclude base on this mode that, on average, the number of incidents tends to increase as the number of months in a year increases.

We can therefore conclude that, shooting incidents are more likely to occur later in the year than earlier.

## Identify bias:
Although our analysis finds that, across all boroughs, the number of incidents is higher for men than for women, there could be potential bias in this report. First, the sample population across these boroughs could have a higher ratio of males to females within the population. Additionally, police may be more likely to target men than women within these boroughs based on stereotyping (wrongly perceiving them as more aggressive). For age, it could also be that within the city, there is a higher population of younger people than older people, which means that incidences are more likely to be reported.

## Conclusion:
In conclusion, we find that men between ages 18-24 are most likely to be involved in shooting incidences based on the demographics observed. There may be potential bias in determining this likelihood, as we must account for other factors such as the demographic makeup of the entire population sample, and the chance of stereotyping from NYPD police. We also have determined that the time of day in which incidences occur are most likely in the evening after 8pm. It is advised that increased security be present in the boroughs where crime is more likely to be committed (such as in Queens and Staten Island as seen in our figure) during the evening time. Taking into account this analysis, NYPD can make more informed decisions on how to protect the community. 
