# Script reading LSMS data
library(tidyverse)
library(readxl)

## load in food consumption tables and name
files<-list.files('data/lsms_subset') %>% str_subset('meta|gps|urban-rural|lsms_all.csv', negate=TRUE)
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

# Fish items are:
civ_fish<-data.frame(
    fish = c('Tilapia frais ( carpe grise importée)', 'Appolo frais (Chinchards)', 'Sardinelles fraiches', 'Autres poissons frais', 
             'Poisson fumé mangni', 'Autres Poissons fumés'),
    form = c('fresh', 'fresh', 'fresh', 'fresh', 'smoked', 'smoked'),
    s07bq01 = 35:40)

## hh dataset: s00q04 is the rural (2) / urban (1) Q

civ<-cotedivoire %>% 
    mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
           hh_id2 = paste(vague, grappe, sep = '_'),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(s07bq01 %in% c(35:40)) %>% 
    select(tot_hh, hh_id, hh_id2, s07bq01, s07bq03a, s07bq03b) %>% 
    left_join(civ_fish) %>% 
    rename('fish_item' = s07bq01, 'quantity' = s07bq03a, 'code' = s07bq03b) %>% 
    left_join(read_excel('data/lsms_subset/meta/civ_unit_codes.xlsx'), by = 'code') %>% 
    left_join(read.csv('data/lsms_subset/gps/cotedivoire_grappe_gps_civ2018.csv') %>% 
                  mutate(hh_id2 = paste(vague, grappe, sep = '_'),
                         lat = coordonnes_gps__Latitude, lon = coordonnes_gps__Longitude) %>% 
                  select(hh_id2, lat, lon),
              by = 'hh_id2') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/cotedivoire_s00_me_civ2018.csv') %>% 
                  mutate(hh_id = paste(vague, grappe, menage, sep = '_'), urban_rural = ifelse(s00q04 == 2, 'rural', 'urban')) %>% 
                  select(hh_id, urban_rural), by = 'hh_id') %>% 
    mutate(country = 'CIV') %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural, fish, form, quantity, unit, country)
    
    read.csv('data/lsms_subset/urban-rural/cotedivoire_s00_me_civ2018.csv') %>% head
# ------------------------ #
#### 2. SENEGAL ####
# ------------------------ #

## This is same format as CIV, but different fish groups
## Fish items are:
sen_fish<-data.frame(
    fish = c('Poisson frais yaboye ou obo (sardinelle)','Poisson frais thiof/ seudeu (baracouda)','Poisson frais wass ',
             'Autre Poisson frais (dorade, youfouf, rouget, siket [capitaine], thiarumbekh [mollette], …..)',
             'Poisson fumé Kethiakh (sardinelle)','Autre Poisson fumé (Con fumé, yaboye ou obo fumé, …)',
             'Poisson séché','Crabes, crevettes et autres fruits de mer','Conserves de poisson '),
    form = c('fresh', 'fresh', 'fresh', 'fresh', 'smoked', 'smoked', 'dried', 'invertebrates', 'canned'),
    s07bq01 = 35:43)

sen<-senegal %>% 
    mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
           hh_id2 = paste(vague, grappe, sep = '_'),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(s07bq01 %in% c(35:43)) %>%
    select(tot_hh, hh_id, hh_id2, s07bq01, s07bq03a, s07bq03b) %>% 
    left_join(sen_fish) %>% 
    rename('fish_item' = s07bq01, 'quantity' = s07bq03a, 'code' = s07bq03b) %>% 
    left_join(read_excel('data/lsms_subset/meta/civ_unit_codes.xlsx'), by = 'code') %>% 
    left_join(read.csv('data/lsms_subset/gps/senegal_grappe_gps_sen2018.csv') %>% 
                  mutate(hh_id2 = paste(vague, grappe, sep = '_'),
                         lat = coordonnes_gps__Latitude, lon = coordonnes_gps__Longitude) %>% 
                  select(hh_id2, lat, lon),
              by = 'hh_id2') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/senegal_s00_me_sen2018.csv') %>% 
                  mutate(hh_id = paste(vague, grappe, menage, sep = '_'), urban_rural = ifelse(s00q04 == 2, 'rural', 'urban')) %>% 
                  select(hh_id, urban_rural), by = 'hh_id') %>% 
    mutate(country = 'SEN') %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural, fish, form, quantity, unit, country)

# ------------------------ #
#### 3. NIGERIA ####
# ------------------------ #
nga_fish<-data.frame(
    fish = c('Fish - fresh', 'Fish - frozen', 'Fish - smoked', 'Fish - dried', 'Snails', 
      'Seafood (lobster, crab, prawns, etc)', 'Canned fish/seafood', 'Other fish or seafood (specify)'),
    form = c('fresh', 'frozen', 'smoked', 'dried', 'invertebrates', 'invertebrates', 'canned', 'other'),
    item_cd = 100:107
)

## IF WE WANT QUANTIFIED, NEED TO PULL UNIT CODES WHICH ARE IN PDF IN /meta FOLDER
## GPS was not available
nga<-nigeria %>% 
    mutate(hh_id = as.character(hhid),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(item_cd %in% c(100:107) & s06bq01 == 1) %>% ## select fish only, and YES consumed (1)
    select(tot_hh, hh_id, item_cd, s06bq02a) %>% 
    left_join(nga_fish) %>% 
    left_join(read.csv('data/lsms_subset/gps/nigeria_nga_householdgeovars_y4.csv') %>% 
                  mutate(hh_id = as.character(hhid), lat = lat_dd_mod, lon = lon_dd_mod) %>% 
                  select(hh_id, lat, lon),
              by = 'hh_id') %>% 
    left_join(read.csv('data/lsms_subset/urban-rural/nigeria_secta_cover.csv') %>% 
                  mutate(hh_id = as.character(hhid), urban_rural = ifelse(sector == 2, 'rural', 'urban')) %>% 
                  select(hh_id, urban_rural), by = 'hh_id') %>% 
    rename('quantity' = s06bq02a) %>% 
    mutate(unit = NA, country = 'NGA') %>% 
    select(tot_hh, hh_id, lat, lon, urban_rural, fish, form, quantity, unit, country)

# ------------------------ #
#### 4. MALAWI ####
# ------------------------ #
mal_fish<-data.frame(
    fish = c('Dried fish small','Dried fish medium', 'Dried fish large', 
             'Fresh fish small', 'Fresh fish medium', 'Fresh fish large',
             'Smoked fish small', 'Smoked fish medium', 'Smoked fish large'
             ),
    form = rep(c('dried', 'fresh', 'smoked'), each=3),
    hh_g02 = c(5021, 5022, 5023, 5031,5032, 5033, 5121, 5122, 5123)
)

unit<-read.csv('data/lsms_subset/meta/malawi_ihs_foodconversion_factor_2020.csv')

## IF WE WANT QUANTIFIED, NEED TO PULL UNIT CODES WHICH ARE IN PDF IN /meta FOLDER (hh_g_03b)
mal<-malawi %>% 
    mutate(hh_id = as.character(case_id),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(hh_g02 %in% mal_fish$hh_g02 & hh_g01 == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, hh_g02, hh_g03a, hh_g03b) %>% 
    left_join(mal_fish) %>% 
    rename('quantity' = hh_g03a, 'unit_code' = hh_g03b, 'item_code' = hh_g02) %>% 
    left_join(unit %>% distinct(item_code, unit_code, unit_name, Otherunit)) %>% 
    left_join(read.csv('data/lsms_subset/gps/malawi_householdgeovariables_ihs5.csv') %>% 
                  mutate(hh_id = as.character(case_id), lat = ea_lat_mod, lon = ea_lon_mod) %>% 
                  select(hh_id, lat, lon),
              by = 'hh_id') %>% 
    # left_join(read.csv('data/lsms_subset/urban-rural/nigeria_secta_cover.csv') %>% 
    #               mutate(hh_id = as.character(hhid), urban_rural = ifelse(sector == 2, 'rural', 'urban')) %>% 
    #               select(hh_id, urban_rural), by = 'hh_id') %>% 
    mutate(country = 'MAL') %>% 
    rename('unit' = unit_name) %>% 
    select(tot_hh, hh_id, lat, lon, fish, form, quantity, unit, country)

# ------------------------ #
#### 5. UGANDA ####
# ------------------------ #
uga_fish<-data.frame(
    fish = c('Fresh tilapia', 'Fresh Nile perch ', 'Dry/ Smoked Tilapia', 'Dry/Smoked Nile perch ',
             'Dried Nkejje ', 'Silver Fish (Mukene) ', 'Other fresh fish ', 'Other dry/smoked fish '),
    form = c('fresh','fresh', 'dry/smoked','dry/smoked', 'dried', 'dried', 'fresh', 'dry/smoked'),
    CEB01 = c(1221, 1222, 1231, 1232, 1234, 1237, 1235, 1236) # note I have dropped underscores that are in PDF but not in dataset
)

## There were no unit codes provided
uga<-uganda %>% 
    mutate(hh_id = as.character(hhid),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(CEB01 %in% uga_fish$CEB01 & CEB03 == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, CEB01, CEB03C) %>%
    left_join(uga_fish) %>%
    left_join(read.csv('data/lsms_subset/gps/uganda_UNPS_Geovars_1011.csv') %>%
                  mutate(hh_id = as.character(HHID), lat = lat_mod, lon = lon_mod) %>%
                  select(hh_id, lat, lon),
              by = 'hh_id') %>%
    # left_join(read.csv('data/lsms_subset/urban-rural/uganda_GSEC1.csv') %>%
    #               mutate(hh_id = as.character(hhid), urban_rural = ifelse(urban == ??, 'rural', 'urban')) %>%
    #               select(hh_id, urban_rural), by = 'hh_id') %>%
    rename('quantity' = CEB03C) %>%
    mutate(unit = NA, country = 'UGA') %>% select(-CEB01) %>% 
    select(tot_hh, hh_id, lat, lon, fish, form, quantity, unit, country)


# ------------------------ #
#### 6. TANZANIA EXTENDED (2013-16) ####
# ------------------------ #
# https://microdata.worldbank.org/index.php/catalog/3455

tza_fish<-data.frame(
    fish = c('Fresh fish and seafood', 'Dried/salted fish and seafood'),
    form = c('fresh','dried'),
    itemcode = c(0808, 0809)
)

tza_unit<-data.frame(
    hh_j02_1 = 1:5,
    unit = c('kg', 'g', 'l', 'ml', 'pieces')
)

tza<-tanzaniaExt %>% 
    mutate(hh_id = as.character(y4_hhid),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(itemcode %in% tza_fish$itemcode & hh_j01 == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, itemcode, hh_j02_1, hh_j02_2) %>%
    left_join(tza_fish) %>%
    left_join(read.csv('data/lsms_subset/gps/tanzania_npsy4.ea.offset.csv') %>% 
                  mutate(hh_id = as.character(clusterid), lat = lat_modified, lon = lon_modified) %>% 
                  select(hh_id, lat, lon),
              by = 'hh_id') %>%
    rename('quantity' = hh_j02_2) %>%
    left_join(tza_unit) %>% 
    mutate(country = 'TZA') %>% 
    select(tot_hh, hh_id, fish, form, quantity, unit, country)

# ------------------------ #
#### 6. TANZANIA (2014-15) ####
# ------------------------ #
# https://microdata.worldbank.org/index.php/catalog/2862/get-microdata

tza_fish<-data.frame(
    fish = c('Fresh fish and seafood', 'Dried/salted fish and seafood'),
    form = c('fresh','dried'),
    itemcode = c(0808, 0809)
)

tza_unit<-data.frame(
    hh_j02_1 = 1:5,
    unit = c('kg', 'g', 'l', 'ml', 'pieces')
)

tza2<-tanzania %>% 
    mutate(hh_id = as.character(y4_hhid),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(itemcode %in% tza_fish$itemcode & hh_j01 == 1) %>% ## select fish only, and YES consumed
    select(tot_hh, hh_id, itemcode, hh_j02_1, hh_j02_2) %>%
    left_join(tza_fish) %>%
    rename('quantity' = hh_j02_2) %>%
    left_join(tza_unit) %>% 
    mutate(country = 'TZA') %>% 
    select(tot_hh, hh_id, fish, form, quantity, unit, country)

# combine tza sample size so these are one country sample
tza<-rbind(tza, tza2) %>% 
    mutate(tot_hh = sum(unique(tot_hh)), .before=hh_id)

## combine datasets
lsms<-rbind(
    civ, sen, #nga,
    mal, uga, tza
)

lsms %>% group_by(country, tot_hh) %>% 
    summarise(n_dried = n_distinct(hh_id[form %in% c('dried', 'smoked', 'dry/smoked')]),
              n_fish = n_distinct(hh_id)) %>% 
    mutate(prop_dried_pop = n_dried / tot_hh,
              prop_fish_pop = n_fish / tot_hh,
           prop_dried_of_fish = n_dried / n_fish)

## Simmance stats for national total fish (dried within fish) consumption:
# Malawi = 73% (71%)
# Tanzania = 71% (46%)
# Uganda = 33% (64%)

## My stats for national total fish (dried within fish) consumption:
# Malawi = 73% (86%)
# Tanzania = 75% (39%)
# Uganda = 55% (88%) 

write.csv(lsms, file = 'data/lsms_subset/lsms_all.csv', row.names=FALSE)
