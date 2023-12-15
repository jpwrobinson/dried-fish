library(tidyverse)
library(cowplot)
library(ggradar)
options(dplyr.summarise.inform = FALSE) # suppress warning from dplyr

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
