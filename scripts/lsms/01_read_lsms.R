# Script reading LSMS data
library(tidyverse)
library(readxl)

## load in food consumption tables and name
files<-list.files('data/lsms_subset') %>% str_subset('meta', negate=TRUE)
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
    fish = c('Tilapia frais ( carpe grise importée)', 'Appolo frais (Chinchards)', 'Sardinelles fraiches', 'Autres poissons frais', 'Poisson fumé mangni', 'Autres Poissons fumés'),
    s07bq01 = 35:40)

## hh dataset: s00q04 is the rural (2) / urban (1) Q

civ<-cotedivoire %>% 
    mutate(hh_id = paste(vague, grappe, menage, sep = '_'),
           tot_hh = n_distinct(hh_id)) %>% 
    filter(s07bq01 %in% c(35:40)) %>% 
    select(tot_hh, hh_id, s07bq01, s07bq03a, s07bq03b) %>% 
    left_join(civ_fish) %>% 
    rename('fish_item' = s07bq01, 'quantity' = s07bq03a, 'code' = s07bq03b) %>% 
    left_join(read_excel('data/lsms_subset/meta/civ_unit_codes.xlsx'), by = 'code') %>% 
    mutate(country = 'CIV')

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
           tot_hh = n_distinct(hh_id)) %>% 
    filter(s07bq01 %in% c(35:43)) %>%
    select(tot_hh, hh_id, s07bq01, s07bq03a, s07bq03b) %>% 
    left_join(sen_fish) %>% 
    rename('fish_item' = s07bq01, 'quantity' = s07bq03a, 'code' = s07bq03b) %>% 
    left_join(read_excel('data/lsms_subset/meta/civ_unit_codes.xlsx'), by = 'code') %>% 
    mutate(country = 'SEN')

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
nga<-nigeria %>% 
    mutate(hh_id = hhid,
           tot_hh = n_distinct(hh_id)) %>% 
    filter(item_cd %in% c(100:107) & s06bq01 == 1) %>% ## select fish only, and YES consumed (1)
    select(tot_hh, hh_id, item_cd, s06bq02a) %>% 
    left_join(nga_fish) %>% 
    rename('quantity' = s06bq02a) %>% 
    mutate(country = 'NGA')

# ------------------------ #
#### 4. MALAWI ####
# ------------------------ #
mal_fish<-data.frame(
    fish = c('Dried fish', 'Fresh fish', 'Smoked fish','Fish Soup/Sauce'),
    form = c('dried', 'fresh', 'smoked', 'other'),
    hh_g02 = c(502, 503, 513, 514)
)

## IF WE WANT QUANTIFIED, NEED TO PULL UNIT CODES WHICH ARE IN PDF IN /meta FOLDER (hh_g_03b)
mal<-malawi %>% 
    mutate(hh_id = case_id,
           tot_hh = n_distinct(hh_id)) %>% 
    filter(hh_g02 %in% mal_fish$hh_g02 & hh_g01 == 'YES') %>% ## select fish only, and YES consumed (1)
    select(tot_hh, hh_id, hh_g02, hh_g03a) %>% 
    left_join(mal_fish) %>% 
    rename('quantity' = hh_g03a) %>% 
    mutate(country = 'MAL')



# ------------------------ #
#### 5. UGANDA ####
# ------------------------ #
uga_fish<-data.frame(
    fish = c('Dried fish', 'Fresh fish', 'Smoked fish','Fish Soup/Sauce'),
    form = c('dried', 'fresh', 'smoked', 'other'),
    hh_g02 = c(502, 503, 513, 514)
)

## IF WE WANT QUANTIFIED, NEED TO PULL UNIT CODES WHICH ARE IN PDF IN /meta FOLDER (hh_g_03b)
uga<-uganda %>% 
    mutate(hh_id = hh,
           tot_hh = n_distinct(hh_id)) %>% 
    # filter(hh_g02 %in% mal_fish$hh_g02 & hh_g01 == 'YES') %>% ## select fish only, and YES consumed (1)
    # select(tot_hh, hh_id, hh_g02, hh_g03a) %>% 
    left_join(uga_fish) %>%
    # rename('quantity' = hh_g03a) %>% 
    mutate(country = 'UGA')


