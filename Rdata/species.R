## 28-09-2020 making the first codes to change the dataframe

# general libraries
library(pander)        # for tables
library(lubridate)     # data and time functions
library(reshape2)      # cast and melt dataframes
library(tidyverse)     # dplyr, tidyr, ggplot, readxl, stringr, etc
library(readxl)        # for reading in R
library(stringr)       # simple consistent wrappers for Common string operations
library(tibble)        # Simple data frames
library(cowplot)       #for ggplot on the same page

# Reset lists
rm(list=ls())

## making the path to load the excelfile
onedrive <- file.path('C:', 'Users', 'jorn', 'OneDrive', 'Uni biologie jaar 7', 'AFI thesis', 'available data species.xlsx')

## upload the data frame in R studio
df <- read_excel(file.path(onedrive),
                 col_names=T,
                 col_types='text',
                 na = '-')


## save data frame
save(df, file='C:/Users/jorn/OneDrive/Uni biologie jaar 7/AFI thesis/Rdata/df.RData')
load(file='C:/Users/jorn/Onedrive/Uni biologie jaar 7/AFI thesis/Rdata/df.RData')

## giving each colomn a specific character / number / date

## This will give the column date
df1 <- df %>% 
    mutate(
    date  = as.numeric(date),
    date  = as.Date(date, origin="1899-12-30")
  ) %>% 
  
separate(date, into=c('year', 'month', 'day'), sep='-', remove=F ) %>%  
 
   ## transform characters of colomn date into date characters and year, months, weeks and dates gets all their own column
  mutate(
    date  = (as.Date(date)),
    month = (as.numeric(month)),
    year  = (as.numeric(year)),
    day   = (as.numeric(day)),
    week  = structure(NA, class= 'character'),
    week  = format(as.Date(date), "%U")
  ) %>%

  ## Every species gets their own column
  separate(species, into=c('ide', 'asp', 'nasus'), sep= '_', remove=F) %>% 
  mutate(
    nasus = ifelse((is.na(nasus)), asp, nasus),
    nasus = gsub('asp', NA, nasus),
    asp   = gsub('nasus', NA, asp),
  ) %>% 
  
  ## changing inlet and outlet in flow
  mutate(
    habitat = gsub('inlet', 'flow', habitat),
    habitat = gsub('outlet' , 'flow', habitat)
  )

## ggplot bars, location, habitat
df1 %>% 
  ggplot()+
  geom_bar(aes(fill=nasus, x=habitat))+
  facet_wrap(~ location, ncol=2)
