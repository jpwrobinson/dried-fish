# Script reading LSMS data
library(tidyverse)
library(readxl)

source('scripts/lsms_fish_codes.R')

## load in food consumption tables and name
files<-list.files('data/lsms_subset') %>% str_subset('meta|gps|household|urban-rural|lsms_all.csv', negate=TRUE)
for(i in 1:length(files)){
    df<-read.csv(paste0('data/lsms_subset/', files[i]))
    assign(str_split_fixed(files[i], '_', n = 2)[1], df)
}

# ------------------------ #
#### 1. IVORY COAST ####
# ------------------------ #
# Ivory Coast food consumption table is 's07b_me_civ2018'. This has surveys for 7 days and 30 days.
# Food item column is s07bq01
# Column s07bq03a is "What is the total quantity of [PRODUCT] consumed by the household in the last 7 days?
# Column s07bq03b is the unit for "What is the total quantity of [PRODUCT] consumed by the household in the last 7 days? 
# Column s07bq03c is ???? (takes integers 0 - 6 suggesting it is a code)

## hh dataset: s00q04 is the rural (2) / urban (1) Q

civ<-cotedivoire %>% 
    mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
           hh_id2 = paste(vague, grappe, sep = '_'),
           tot_hh = n_distinct(hh_id)) %>% 
    left_join(read.csv('data/lsms_subset/gps/cotedivoire_grappe_gps_civ2018.csv') %>% 
                  mutate(hh_id2 = paste(vague, grappe, sep = '_'),
                         lat = coordonnes_gps__Latitude, lon = coordonnes_gps__Longitude) %>% 
                  select(hh_id2, lat, lon),
              by = 'hh_id2') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/cotedivoire_s00_me_civ2018.csv') %>% 
                  mutate(hh_id = paste(vague, grappe, menage, sep = '_'), urban_rural = ifelse(s00q04 == 2, 'rural', 'urban')) %>% 
                  select(hh_id, urban_rural), by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/household/cotedivoire_s01_me_civ2018.csv') %>% 
        mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
               age = ifelse(is.na(s01q04a), 2019-s01q03c, s01q04a)) %>% 
        group_by(hh_id) %>% 
        summarise(n_hh = length(s01q00a), n_adult = length(s01q00a[age>15]), n_children = length(s01q00a[age<16])) %>%
        select(hh_id, n_hh, n_adult, n_children), by ='hh_id') %>% 
    mutate(country = 'CIV') 

civ_hh<-civ %>% distinct(hh_id, tot_hh, lat, lon, urban_rural, n_hh, n_adult, n_children, country)

civ_fish<-civ %>%
    filter(s07bq01 %in% c(35:40)) %>% 
    select(country, tot_hh, hh_id, hh_id2, s07bq01, s07bq03a, s07bq03b, lat, lon, urban_rural, n_hh) %>% 
    left_join(civ_fish_code) %>% 
    rename('fish_item' = s07bq01, 'quantity' = s07bq03a, 'code' = s07bq03b) %>% 
    left_join(read_excel('data/lsms_subset/meta/civ_unit_codes.xlsx'), by = 'code') %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural, n_hh, fish, form, quantity, unit, country)

# ------------------------ #
#### 2. SENEGAL ####
# ------------------------ #
# Enquête Harmonisée sur le Conditions de Vie des Ménages 2018-2019 
# https://microdata.worldbank.org/index.php/catalog/4297

sen<-senegal %>% 
    mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
           hh_id2 = paste(vague, grappe, sep = '_'),
           tot_hh = n_distinct(hh_id)) %>% 
    left_join(read.csv('data/lsms_subset/gps/senegal_grappe_gps_sen2018.csv') %>% 
                  mutate(hh_id2 = paste(vague, grappe, sep = '_'),
                         lat = coordonnes_gps__Latitude, lon = coordonnes_gps__Longitude) %>% 
                  select(hh_id2, lat, lon),
              by = 'hh_id2') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/senegal_s00_me_sen2018.csv') %>% 
                  mutate(hh_id = paste(vague, grappe, menage, sep = '_'), urban_rural = ifelse(s00q04 == 2, 'rural', 'urban')) %>% 
                  select(hh_id, urban_rural), by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/household/senegal_s01_me_sen2018.csv') %>% 
        mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
               age = ifelse(is.na(s01q04a), 2019-s01q03c, s01q04a)) %>% 
        group_by(hh_id) %>% 
        summarise(n_hh = length(s01q00a), n_adult = length(s01q00a[age>15]), n_children = length(s01q00a[age<16])) %>%
        select(hh_id, n_hh, n_adult, n_children), by ='hh_id') %>% 
    mutate(country = 'SEN')

sen_hh<-sen %>% distinct(hh_id, tot_hh, lat, lon, urban_rural, n_hh, n_adult, n_children, country)

sen_fish<-sen %>%
    filter(s07bq01 %in% c(35:43)) %>%
    select(tot_hh, hh_id, hh_id2, s07bq01, s07bq03a, s07bq03b, lat, lon, urban_rural, n_hh, country) %>% 
    left_join(sen_fish_code) %>% 
    rename('fish_item' = s07bq01, 'quantity' = s07bq03a, 'code' = s07bq03b) %>% 
    left_join(read_excel('data/lsms_subset/meta/civ_unit_codes.xlsx'), by = 'code') %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural, n_hh, fish, form, quantity, unit, country)

# ------------------------ #
#### 3. NIGERIA ####
# General Household Survey Wave 4 2018-2019 
# ------------------------ #
# https://microdata.worldbank.org/index.php/catalog/3557

## IF WE WANT QUANTIFIED, NEED TO PULL UNIT CODES WHICH ARE IN PDF IN /meta FOLDER
## Note there is a Living Standards Survey in same year but this did not have GPS data (with 22000 households, compared to 5000 in the GHS)
nga<-nigeria %>% 
    mutate(hh_id = as.character(hhid),
           tot_hh = n_distinct(hh_id)) %>% 
    left_join(read.csv('data/lsms_subset/gps/nigeria_nga_householdgeovars_y4.csv') %>% 
                  mutate(hh_id = as.character(hhid), lat = lat_dd_mod, lon = lon_dd_mod) %>% 
                  select(hh_id, lat, lon),
              by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/nigeria_secta_cover.csv') %>% 
                  mutate(hh_id = as.character(hhid), urban_rural = ifelse(sector == 2, 'rural', 'urban')) %>% 
                  select(hh_id, urban_rural), by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/household/nigeria_sect1_harvestw4.csv') %>% 
        mutate(hh_id = as.character(hhid)) %>% 
        filter(s1q4a == 1) %>% # only current HH members
        group_by(hh_id) %>% 
        summarise(n_hh = length(indiv), n_adult = length(indiv[s1q4>15]), n_children = length(indiv[s1q4<16])) %>%
        select(hh_id, n_hh, n_adult, n_children), by ='hh_id') %>% 
    mutate(country = 'NGA')
    
nga_hh<-nga %>% distinct(hh_id, tot_hh, lat, lon, urban_rural, n_hh, n_adult, n_children, country)

nga_fish<-nga %>% 
    filter(item_cd %in% c(100:107) & s10bq1 == 1) %>% ## select fish only, and YES consumed (1)
    select(tot_hh, hh_id, item_cd, s10bq2a, lat, lon, urban_rural, n_hh, country) %>% 
    left_join(nga_fish_code) %>% 
    rename('quantity' = s10bq2a) %>% 
    mutate(unit = NA) %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural, n_hh, fish, form, quantity, unit, country)


# ------------------------ #
#### 4. MALAWI IHS4 16/17 ####
# ------------------------ #
# Fourth Integrated Household Survey 2016-2017
# https://microdata.worldbank.org/index.php/catalog/2936/data-dictionary

unit<-read.csv('data/lsms_subset/meta/malawi_ihs_foodconversion_factor_2020.csv')

## IF WE WANT QUANTIFIED, NEED TO PULL UNIT CODES WHICH ARE IN PDF IN /meta FOLDER (hh_g_03b)
mal<-malawi %>% 
    mutate(hh_id = as.character(case_id),
           tot_hh = n_distinct(hh_id)) %>% 
    left_join(read.csv('data/lsms_subset/gps/malawi_householdgeovariables_ihs4.csv') %>% 
                  mutate(hh_id = as.character(case_id), lat = lat_modified, lon = lon_modified) %>% 
                  select(hh_id, lat, lon),
              by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/malawi_hh_mod_a_filt.csv') %>%
                  mutate(hh_id = as.character(case_id), urban_rural = ifelse(reside == 'RURAL', 'rural', 'urban')) %>%
                  select(hh_id, urban_rural), by = 'hh_id') %>%
    left_join(read.csv('data/lsms_subset/household/malawi_hh_mod_b.csv') %>% 
        mutate(hh_id = as.character(case_id)) %>% 
        group_by(hh_id) %>% 
        summarise(n_hh = length(PID), n_adult = length(PID[hh_b05a>15]), n_children = length(PID[hh_b05a<16])) %>%
        select(hh_id, n_hh, n_adult, n_children), by ='hh_id') %>% 
    mutate(country = 'MWI') 

mal_hh<-mal %>% distinct(hh_id, tot_hh, lat, lon, urban_rural,n_hh, n_adult, n_children, country)

mal_fish<-mal %>% 
    filter(hh_g02 %in% mal_fish_code$hh_g02 & hh_g01 == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, hh_g02, hh_g03a, hh_g03b, lat, lon, urban_rural,n_hh, country) %>% 
    left_join(mal_fish_code) %>% 
    rename('quantity' = hh_g03a, 'unit_code' = hh_g03b, 'item_code' = hh_g02) %>% 
    left_join(unit %>% distinct(item_code, unit_code, unit_name, Otherunit)) %>%
    rename('unit' = unit_name) %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural,n_hh, fish, form, quantity, unit, country)

# ------------------------ #
#### 5. UGANDA ####
# ------------------------ #
# National Panel Survey 2010-2011 

## Unit codes are https://microdata.worldbank.org/index.php/catalog/2166/variable/F82/V1230?name=untcd
uga<-uganda %>% 
    mutate(hh_id = as.character(hh),
           tot_hh = n_distinct(hh_id)) %>% 
    left_join(read.csv('data/lsms_subset/gps/uganda_UNPS_Geovars_1011.csv') %>%
                  mutate(hh_id = as.character(HHID), lat = lat_mod, lon = lon_mod) %>%
                  select(hh_id, lat, lon),
              by = 'hh_id') %>%
    left_join(read.csv('data/lsms_subset/urban-rural/uganda_GSEC1.csv') %>%
                  mutate(hh_id = as.character(HHID), urban_rural = ifelse(urban == 0, 'rural', 'urban')) %>%
                  select(hh_id, urban_rural), by = 'hh_id') %>%
    left_join(read.csv('data/lsms_subset/household/uganda_GSEC2.csv') %>% 
        mutate(hh_id = as.character(HHID)) %>% 
        group_by(hh_id) %>% 
        summarise(n_hh = length(h2q1), n_adult = length(h2q1[h2q8>15]), n_children = length(h2q1[h2q8<16])) %>%
        select(hh_id, n_hh, n_adult, n_children), by ='hh_id') %>% 
    mutate(country = 'UGA')
    
uga_hh<-uga %>% distinct(hh_id, tot_hh, lat, lon, urban_rural, n_hh, n_adult, n_children, country)


uga_fish<-uga %>% 
    filter(itmcd %in% uga_fish_1011_code$itmcd & h15bq3a == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, itmcd, untcd, h15bq14, lat, lon, urban_rural, n_hh, country) %>%
    left_join(uga_fish_1011_code) %>%
    rename('unit' = untcd, 'quantity' = h15bq14) %>%
    select(tot_hh, hh_id, lat, lon, urban_rural, n_hh, fish, form, quantity, unit, country)


# ------------------------ #
#### 6. TANZANIA EXTENDED (2013-16) ####
# ------------------------ #
# https://microdata.worldbank.org/index.php/catalog/3455
 
# tza<-tanzaniaExt %>% 
#     mutate(hh_id = as.character(y4_hhid),
#            tot_hh = n_distinct(hh_id)) %>% 
#     filter(itemcode %in% tza_fish$itemcode & hh_j01 == 1) %>% ## select fish only, and YES consumed
#     select(tot_hh, hh_id, itemcode, hh_j02_1, hh_j02_2) %>%
#     left_join(tza_fish) %>%
#     rename('quantity' = hh_j02_2) %>%
#     left_join(tza_unit) %>% 
#     mutate(country = 'TZA') %>% 
#     select(tot_hh, hh_id, fish, form, quantity, unit, country)

# ------------------------ #
#### 6. TANZANIA (2014-15) ####
# ------------------------ #
# National Panel Survey 2014-2015, Wave 4 
# https://microdata.worldbank.org/index.php/catalog/2862/get-microdata

tza<-tanzania %>% 
    mutate(hh_id = as.character(y4_hhid),
           tot_hh = n_distinct(hh_id)) %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/tanzania_consumptionNPS4.csv') %>%
                  mutate(hh_id = as.character(y4_hhid), urban_rural = ifelse(urban == 1, 'rural', 'urban')) %>%
                  select(hh_id, urban_rural), by = 'hh_id') %>%
    left_join(read.csv('data/lsms_subset/urban-rural/tanzania_hh_sec_a.csv') %>% 
                  mutate(hh_id = as.character(y4_hhid)) %>% select(hh_id, clusterid), by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/gps/tanzania_npsy4.ea.offset.csv') %>% 
                  mutate(lat = lat_modified, lon = lon_modified) %>% 
                  select(clusterid, lat, lon),
              by = 'clusterid') %>%
    left_join(read.csv('data/lsms_subset/household/tanzania_ag_sec_01.csv') %>% 
        mutate(hh_id = as.character(y4_hhid)) %>% 
        filter(!is.na(indidy4)) %>% 
        group_by(hh_id) %>% 
        summarise(n_hh = length(indidy4), n_adult = length(indidy4[ag01_02>15]), n_children = length(indidy4[ag01_02<16])) %>% 
        select(hh_id, n_hh, n_adult, n_children), by ='hh_id') %>% 
    mutate(country = 'TZA')

tza_hh<-tza %>% distinct(hh_id, tot_hh, lat, lon, urban_rural, n_hh, n_adult, n_children, country)

tza_fish<-tza %>% 
    filter(itemcode %in% tza_fish_code$itemcode & hh_j01 == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, itemcode, hh_j02_1, hh_j02_2, lat, lon, urban_rural, n_hh, country) %>%
    left_join(tza_fish_code) %>%
    rename('quantity' = hh_j02_2) %>%
    left_join(tza_unit_code) %>% 
    select(tot_hh, hh_id, lat, lon,urban_rural, n_hh, fish, form, quantity, unit, country)

## combine datasets
lsms_hh<-rbind(
    civ_hh, sen_hh, nga_hh,
    mal_hh, uga_hh, tza_hh
)

lsms_fish<-rbind(
    civ_fish, sen_fish, nga_fish,
    mal_fish, uga_fish, tza_fish
)

lsms_hh %>% group_by(country, tot_hh) %>% 
    filter(!is.na(n_hh)) %>% 
    summarise(n_hh = mean(n_hh), n_adult = mean(n_adult), n_children = mean(n_children))

lsms_fish %>% group_by(country, tot_hh) %>% 
    summarise(n_dried = n_distinct(hh_id[form %in% c('dried', 'smoked', 'dry/smoked')]),
              n_fish = n_distinct(hh_id)) %>% 
    mutate(prop_dried_pop = n_dried / tot_hh,
              prop_fish_pop = n_fish / tot_hh,
           prop_dried_of_fish = n_dried / n_fish)

## Simmance stats for national total fish (dried within fish) consumption:
# Malawi = 73% (71%)
# Tanzania = 71% (46%)
# Uganda = 33% (64%)

## My stats for national total fish (dried within fish) [total dried] consumption:
# Malawi = 73% (86%) [63%]
# Tanzania = 75% (39%) [29%]
# Uganda = 36% (66%) [23%] 
# Cote D'Ivoire = 87% (67%) [59%]
# Senegal = 91% (74%) [67%]
# Nigeria = 71% (58%) [42%]


write.csv(lsms_hh, file = 'data/lsms_subset/lsms_all_hh.csv', row.names=FALSE)
write.csv(lsms_fish, file = 'data/lsms_subset/lsms_fish.csv', row.names=FALSE)
