library(tidyverse)
library(readxl)
library(janitor)
library(countrycode)
library(cowplot)
library(tidybayes)  
library(modelr)    
library(ggradar)
library(sf)
library(rnaturalearth)
library(tmap)
library(raster)
library(brms)
library(rethinking)
library(conflicted)
library(WDI)
options(dplyr.summarise.inform = FALSE) # suppress warning from dplyr

## conflicts resolve
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(brms::ar)
conflicts_prefer(stats::chisq.test)
conflicts_prefer(tidyr::extract)
conflicts_prefer(stats::fisher.test)
conflicts_prefer(stats::lag)
conflicts_prefer(brms::loo)
conflicts_prefer(rethinking::LOO)
conflicts_prefer(brms::pstudent_t)
conflicts_prefer(brms::qstudent_t)
conflicts_prefer(modelr::resample)
conflicts_prefer(stats::rstudent)
conflicts_prefer(brms::rstudent_t)
conflicts_prefer(lubridate::stamp)
conflicts_prefer(brms::stancode)
conflicts_prefer(brms::WAIC)
conflicts_prefer(base::union)
conflicted::conflicts_prefer(janitor::crosstab)
conflicted::conflicts_prefer(tidybayes::dstudent_t)
conflicted::conflicts_prefer(base::intersect)
conflicted::conflicts_prefer(purrr::map)

# plot cols
source('scripts/theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh','Fried', 'Powder','Smoked', 'Sun-dried')
pcols_named<-c('Fresh' = pcols[1], 'Sun-dried' = pcols[2], 'Smoked' = pcols[3],'Fried' = pcols[4], 'Powder' = pcols[5])

## get RDA reference vals
source('scripts/rda_reader_integrated.R')
# source('scripts/rda_reader_FAO.R')
rda$nutrient<-str_to_title(rda$nutrient)
rda$nutrient[rda$nutrient=='Vitamin_a']<-'Vitamin A'
rda$nutrient[rda$nutrient=='Vitamin_d']<-'Vitamin D'
rda$nutrient[rda$nutrient=='Vitamin_b12']<-'Vitamin B12'
rda$nutrient[rda$nutrient=='Omega_3']<-'Omega-3 (DHA + EPA)'

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Omega-3 (DHA + EPA)', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

# inland / marine
realm_cols<-c('#4daf4a', '#386cb0')
realm_cols_named<-c('Inland' = '#4daf4a','Marine' = '#386cb0')


countries<-c('Senegal', 'Nigeria', 'Malawi', 'Tanzania', 'Uganda', "Cote d'Ivoire")
countries2<-c('Senegal', 'Nigeria', 'Malawi', 'Tanzania', 'Uganda', "Ivory Coast")
