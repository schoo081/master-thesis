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
onedrive <- file.path('C:', 'Users', 'jorn', 'OneDrive', 'Uni biologie jaar 7', 'AFI thesis', 'Jochem_available data_V2.xlsx')

## upload the data frame in R studio
df <- read_excel(file.path(onedrive),
                 sheet='Ruwe data',
                 col_names=T,
                 col_types='text',
                 na = '') %>% 
  ## removing special characters
  setNames(tolower(names(.))) %>%         ## removing capital letters in the headings
  setNames(gsub(' ', '', names(.))) %>%   ## removing white spaces in the headings
  setNames(gsub('\\=|\\(|\\)|\\,|\\_|\\%|\\/', '', names(.))) %>%  ## removing special characters
  
  ## changing column names
  setNames(gsub('^samplenr$'            , 'sample_nr'              ,names(.))) %>% 
  setNames(gsub('^locatie$'             , 'location'               ,names(.))) %>% 
  setNames(gsub('^jaar$'                , 'year'                   ,names(.))) %>% 
  setNames(gsub('^maand$'               , 'month'                  ,names(.))) %>% 
  setNames(gsub('^dag$'                 , 'day'                    ,names(.))) %>% 
  setNames(gsub('^soortwiwinderbroofbleisnsneep$'      , 'species' ,names(.))) %>% 
  setNames(gsub('^totaallengtevanuiterstetotuiterste$' , 'Lt_cm'       ,names(.))) %>%
  setNames(gsub('^vorklengtetotdevorkvandestraat$'     , 'Lf_cm'       ,names(.))) %>% 
  setNames(gsub('^schublengtetotwaardestraatvenbegint$', 'Ls_cm'       ,names(.))) %>% 
  setNames(gsub('^fulllness'            , 'fullness'                   ,names(.))) %>% 
  setNames(gsub('^draadalg$'            , 'draadalg_per'               ,names(.))) %>%
  setNames(gsub('^alg$'                 , 'algae_per'                  ,names(.))) %>% 
  setNames(gsub('^volwassenvliegmug$'   , 'diptera_adult_per'          ,names(.))) %>% 
  setNames(gsub('^volwassenkever$'      , 'coleoptera_adult_per'       ,names(.))) %>% 
  setNames(gsub('^anorganischmateriaal$', 'inorganic_material_per'     ,names(.))) %>% 
  setNames(gsub('^annelida$'            , 'annelida_per'               ,names(.))) %>% 
  setNames(gsub('^chironomiden$'        , 'chironomidae_per'           ,names(.))) %>% 
  setNames(gsub('^chaoborus$'           , 'chaoborus_per'              ,names(.))) %>% 
  setNames(gsub('^cladocera$'           , 'cladocera_per'              ,names(.))) %>%
  setNames(gsub('^carinogammarusroeseli$'     , 'carino_gammarus_roeseli_per'     ,names(.))) %>%          ## not sure why carino
  setNames(gsub('^acantholeberiscurvirostris$', 'acantholeberis_curvirostris_per' ,names(.))) %>% 
  setNames(gsub('^cyclops_Copopoda$'          , 'cyclops_copopoda_per'            ,names(.))) %>% 
  setNames(gsub('^schelpdier$'          , 'bivalvia_per'        ,names(.))) %>% 
  setNames(gsub('^trichoptera$'         , 'trichoptera_per'     ,names(.))) %>% 
  setNames(gsub('^platyhelminthes$'     , 'platyhelminthes_per' ,names(.))) %>% 
  setNames(gsub('^onbekendinsect$'      , 'insect_unkown_per'   ,names(.))) %>% 
  setNames(gsub('^roeidiertje$'         , 'roeidiertje_per'     ,names(.))) %>%                            ##name need to change
  setNames(gsub('^amphipoda$'           , 'amphipoda_per'       ,names(.))) %>% 
  setNames(gsub('^hairworm$'            , 'nematomorpha_per'    ,names(.))) %>% 
  setNames(gsub('^vlees$'               , 'meat_per'            ,names(.))) %>%
  setNames(gsub('^vis$'                 , 'fish_per'            ,names(.))) %>%
  setNames(gsub('^parasieten$'          , 'parasites'       ,names(.))) %>%
  setNames(gsub('^verwondingen$'        , 'injuries'        ,names(.))) %>%
  setNames(gsub('^controleallevoedselcatagorieenzijninschattingvandetotaledarminhoud$','control',names(.))) 
  
  



## save data frame
save(df, file='C:/Users/jorn/OneDrive/Uni biologie jaar 7/AFI thesis/Rdata/df_Jochem.RData')
load(file='C:/Users/jorn/Onedrive/Uni biologie jaar 7/AFI thesis/Rdata/df_Jochem.RData')


df1 <- df %>%
  mutate(
    place = structure(NA, class= 'character'),
    place = ifelse(grepl('K', location), 'katerstede', place),
    place = ifelse(grepl('H', location), 'hurwenen'  , place),
  ) %>% 
  mutate_at(vars(matches("per|cm|^day$|^week$|^month$|^year$|fullness")),funs(as.numeric)) %>% 
  unite(date, c('day','month','year'), sep='-', remove=F) %>% 
  mutate(
    date = as.Date(date, format = "%d-%m-%Y"),
  )

df1 %>% 
  filter(
    # place == 'hurwenen',
         # month == '7' | month == '8',
         fullness > 25) %>% 

ggplot(aes(x=date, y=Lt_cm))+
geom_point(aes(colour='species'))

sort(unique(df1$year))  

# ===================================================================================================================
# controlling codes
# =================================================================================================================== 

sort(unique(import$lengthavg))
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