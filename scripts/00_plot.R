library(tidyverse)
library(readxl)
library(janitor)
library(countrycode)
library(cowplot)
library(tidybayes)  
library(bayesplot)
library(modelr)    
library(ggradar)
library(ggExtra)
library(sf)
library(rnaturalearth)
library(tmap)
library(raster)
library(brms)
library(rethinking)
library(conflicted)
library(WDI)
library(FAOSTAT)
library(emmeans)
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
# pcols_named<-c('Fresh' = pcols[1], 'Sun-dried' = pcols[2], 'Smoked' = pcols[3],'Fried' = pcols[4], 'Powder' = pcols[5])

# https://paletton.com/#uid=73b0u0km0n-cfyYhFrXq1juvce8
pcols_named<-c('Sun-dried' = '#ED0606', 'Smoked' = '#048E8E', 
               'Fresh' = '#05BE05', 'Powder' = '#ED6F06', 'Fried' = '#EEB407', 'Dried' = '#ED6F06')

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
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'),
                  type = c('Protein', rep('Mineral', 5), 'Fat', rep('Vitamin', '4')))


nut_cols<-c('Protein' = 'black',
            'Calcium' = '#1f78b4',
            'Iron' = '#1f78b4',
            'Selenium' = '#1f78b4',
            'Zinc' = '#1f78b4',
            'Iodine' = '#1f78b4',
            'Omega-3 (DHA + EPA)' = '#a6cee3',
            'Vitamin A' = '#abdda4',
            'Vitamin D' = '#abdda4',
            'Vitamin B12' = '#abdda4',
            'Folate' = '#abdda4')

nut_cols2<-c('Mineral' = '#1f78b4', 'Fat' = '#a6cee3', 'Vitamin' = '#abdda4')

# inland / marine
realm_cols<-c('#4daf4a', '#386cb0')
realm_cols_named<-c('Inland' = '#4daf4a','Marine' = '#386cb0')


countries<-c('Senegal', 'Nigeria', 'Malawi', 'Tanzania', 'Uganda', "Cote d'Ivoire")
countries2<-c('Senegal', 'Nigeria', 'Malawi', 'Tanzania', 'Uganda', "Ivory Coast")
