## 01-10-2020 making the first codes to change the dataframe

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
onedrive <- file.path('C:', 'Users', 'jorn', 'OneDrive', 'Uni biologie jaar 7', 'AFI thesis', 'Collin_Jorn', 'Collin_Jorn_available data.xlsx')

## upload the data frame in R studio
cj <- read_excel(file.path(onedrive),
                 # sheet= 1,
                 col_names=T,
                 col_types='text',
                 na = '') %>% 
  ## removing special characters
  setNames(tolower(names(.))) %>%         ## removing capital letters in the headings
  setNames(gsub(' ', '', names(.))) %>%   ## removing white spaces in the headings
  setNames(gsub('\\=|\\(|\\)|\\,|\\%|\\/', '', names(.))) %>%    ## removing special characters
  
  ## removing empty rows
  filter(!is.na(sample_nr))
  

## save data frame
save(df, file='C:/Users/jorn/OneDrive/Uni biologie jaar 7/AFI thesis/Rdata/df_Collin_Jorn.RData')
load(file='C:/Users/jorn/Onedrive/Uni biologie jaar 7/AFI thesis/Rdata/df_Collin_Jorn.RData')



## adding columns for the locations, one column for the total catching date and one for length class
cj1 <- cj %>%
  mutate(
    location = tolower(location),
    place = structure(NA, class= 'character'),
    place = ifelse(grepl('k', location), 'katerstede', place),
    place = ifelse(grepl('h', location), 'hurwenen'  , place),
## different habitats
    habitat = structure(NA, class= 'character'),
    habitat = ifelse(grepl('^[a-z]{2}', location), 'groyne field', habitat), ## check this one, it is currently not working good!!
    habitat = ifelse(grepl('^h02$|^h05$|^h02$', location), 'stagnant water', habitat),
    habitat = ifelse(grepl('^k07$|^k12$|^k14$', location), 'stagnant water', habitat),
    habitat = ifelse(grepl('^h08$|^h15$', location), 'flowing water', habitat),
    habitat = ifelse(grepl('^k01$|^k10$', location), 'flowing water', habitat),
  ) %>% 
  mutate_at(vars(matches("per|cm|^day$|^week$|^month$|^year|^control$")),funs(as.numeric)) %>% 
  unite(date, c('day','month','year'), sep='-', remove=F) %>% 
  mutate(
    date = as.Date(date, format = "%d-%m-%Y"),
    length_class = structure(NA, class= 'character'),
    length_class = cut(lt_cm, seq(0, 20, 1)),
  )


## making the columns of the food into rows, easier to make graphs with
cj2 <- cj1 %>% 
# group_by(location ,species, date) %>% 
  gather(value = 'percentage', key= 'food', c(17:55)) %>% 
  mutate(
    percentage = ifelse(is.na(percentage), 0, percentage)
  )




# ===================================================================================================================
# Graphs
# =================================================================================================================== 

# -------------------------------------------------------------------------------------------------------------------
# Nase
# ===================================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# length distribution 
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Sn') %>% 
  group_by(food, species, length_class) %>% 
  dplyr::summarise(mean = mean(percentage))

## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Sn') %>% 
  group_by(length_class) %>% 
  summarise(
    label = sum(!is.na(length_class)),
  )

## percentage of all diet 
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Sn') %>% 
  ggplot()+
  geom_col(
    aes(x=length_class, y=mean, fill=food)
    )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= length_class,
        vjust=0, hjust=0), 
    colour="gray15"
    )+
  theme_bw()

# -------------------------------------------------------------------------------------------------------------------
# different place
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data for the group place
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Sn') %>% 
  group_by(food, species, place) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Sn') %>% 
  group_by(place) %>% 
  summarise(
    label = sum(!is.na(location)),
  )

## percentage of all diet   
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Sn') %>% 
  ggplot()+
  geom_col(
    aes(x=place, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= place,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()

# -------------------------------------------------------------------------------------------------------------------
# different location
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data for the group place
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Sn') %>% 
  group_by(food, species, location) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Sn') %>% 
  group_by(location) %>% 
  summarise(
    label = sum(!is.na(location)),
  )

## percentage of all diet   
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Sn') %>% 
  ggplot()+
  geom_col(
    aes(x=location, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= location,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()


# ===================================================================================================================
# Asp
# ===================================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# length distribution 
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Sn') %>% 
  group_by(food, species, length_class) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Rb') %>% 
  group_by(length_class) %>% 
  summarise(
    label = sum(!is.na(length_class)),
  )

## percentage of all diet 
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Rb') %>% 
  ggplot()+
  geom_col(
    aes(x=length_class, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= length_class,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()

# -------------------------------------------------------------------------------------------------------------------
# different place
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data for the group place
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Rb') %>% 
  group_by(food, species, place) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Rb') %>% 
  group_by(place) %>% 
  summarise(
    label = sum(!is.na(location)),
  )

## percentage of all diet   
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Rb') %>% 
  ggplot()+
  geom_col(
    aes(x=place, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= place,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()

# -------------------------------------------------------------------------------------------------------------------
# different location
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data for the group place
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Rb') %>% 
  group_by(food, species, location) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Rb') %>% 
  group_by(location) %>% 
  summarise(
    label = sum(!is.na(location)),
  )

## percentage of all diet   
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Rb') %>% 
  ggplot()+
  geom_col(
    aes(x=location, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= location,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()


# ===================================================================================================================
# Ide
# ===================================================================================================================

# -------------------------------------------------------------------------------------------------------------------
# length distribution 
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Sn') %>% 
  group_by(food, species, length_class) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Wi') %>% 
  group_by(length_class) %>% 
  summarise(
    label = sum(!is.na(length_class)),
  )

## percentage of all diet 
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Wi') %>% 
  ggplot()+
  geom_col(
    aes(x=length_class, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= length_class,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()

# -------------------------------------------------------------------------------------------------------------------
# different place
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data for the group place
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Wi') %>% 
  group_by(food, species, place) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Wi') %>% 
  group_by(place) %>% 
  summarise(
    label = sum(!is.na(location)),
  )

## percentage of all diet   
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Wi') %>% 
  ggplot()+
  geom_col(
    aes(x=place, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= place,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()

# -------------------------------------------------------------------------------------------------------------------
# different location
# -------------------------------------------------------------------------------------------------------------------
## making a short summary of the data for the group place
cj3 <- cj2 %>%
  # filter(
  # species == 'Wi',
  # place == 'hurwenen',
  # ) %>% 
  # filter(species == 'Wi') %>% 
  group_by(food, species, location) %>%
  dplyr::summarise(mean = mean(percentage))


## making a label for geom_text to know the number of fish per classes 
label <- cj1 %>%
  filter(species == 'Wi') %>% 
  group_by(location) %>% 
  summarise(
    label = sum(!is.na(location)),
  )

## percentage of all diet   
cj3 %>% 
  filter(!mean == 0) %>%
  filter(species == 'Wi') %>% 
  ggplot()+
  geom_col(
    aes(x=location, y=mean, fill=food)
  )+
  geom_text(
    data=label, 
    aes(label = paste("n=", label), 
        y = 80, x= location,
        vjust=0, hjust=0), 
    colour="gray15"
  )+
  theme_bw()

# ===================================================================================================================
# controlling codes
# =================================================================================================================== 

sort(unique(cj1$species))
b <- import[grepl('258\\+', import$lengthavg),]
sort(unique(b$fatpercentageavg)) 
count(import, is.na(weightmax))
grepl('>|\\+',)
a <- filter(import, weightavg < weightmin)
b <- filter(import, weightavg > weightmax)

sort(unique(import$weightavg))
count(import, is.na(weightmin))
a <- filter(import, !is.na(grammin) & !is.na(weightmin))
a <- filter(import, year == 2018)
a <- import[grep('-', import$remarks),]
a <- import[grep('-', import$weightdif),]
b <- import(me)
sort(unique(a$weightdif))
import[22,]
count(import, is.na(fatpercentageavg))
hist(import$voed, xlim=c(0,5))
?hist
hist(c(import$fatpercentageavg), seq(0, 1 ,0.02), xlim=c(0,0.35), col = 'blue')