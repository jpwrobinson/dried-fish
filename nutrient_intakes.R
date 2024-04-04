
targets::tar_load(nut_data)

nut_data %>% 
    filter(!form %in% c('Fresh', 'Fresh, gutted'),
           nutrient %in% nuts) %>% 
    mutate(nutrient = str_to_title(nutrient)) %>% 
    mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                              'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Epa_dha'))) %>%
    mutate(nutrient = recode(nutrient,  Epa_dha = 'Omega-3 (DHA + EPA)', 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) %>% 
    group_by(nutrient) %>% 
    summarise(value = mean(value)) %>%
    left_join(rda) %>% 
    mutate(prop_rni_kids = value*portionK/100 / rni_kids*100,
           prop_rni_women = value*portionW/100 / rni_women*100)


## read proportion of dried fish eaters (observed and modelled)
targets::tar_load(lsms_proximity)

prop_dried<-lsms_proximity %>% 
    group_by(country) %>% 
    filter(!is.na(n_hh)) %>% 
    mutate(tot_hh2 = n_distinct(hh_id)) %>% 
    group_by(country, tot_hh2) %>% 
    count(dried) %>% 
    mutate(prop = n / tot_hh2) %>% 
    filter(dried=='yes')

load(file = 'data/mod/lsms_mod.rds')
post<-m1 %>%
    spread_draws(b_Intercept, r_country[state,]) %>% 
    mutate(country_mean = inv_logit(b_Intercept + r_country)) %>%
    summarise_draws() %>% 
    filter(variable == 'country_mean')
# check why posterior probs are higher than the observed proportions

# post<-posterior_epred(m1)

# join populations
class<-WDI(extra=TRUE)
# WDIsearch('total.*female.*population')[1:20,]

pop <- getWDItoSYB(name = "total_population", indicator = "SP.POP.TOTL")$entity %>%
    clean_names() %>% filter(year == 2020) %>%
    left_join(class %>% distinct(country, capital, longitude, latitude, income), by = 'country') %>%
    select(-year) %>% 
    mutate(iso3 = countrycode(iso2_wb_code, origin = 'iso2c', destination = 'iso3c'),
           income = ifelse(iso2_wb_code=='VN', 'Lower middle income', income)) %>% 
    left_join(read.csv('data/FAOSTAT_data_en_6-26-2023.csv') %>%  ## from EAT data
                  clean_names() %>% filter(year == 2020 & item %in% c('Cost of a healthy diet (PPP dollar per person per day)')) %>% 
                  mutate(iso3 = countrycode(area, origin = 'country.name.en', destination = 'iso3c'),
                         cost_healthy_diet_ppp_person_day = value) %>% 
                  select(year, cost_healthy_diet_ppp_person_day, iso3), by = 'iso3') %>% 
    filter(iso3 %in% lsms_proximity$country) %>% 
    left_join(post %>% mutate(iso3 = state) %>% select(iso3, median,  q5, q95), by = 'iso3') %>% 
    mutate(people_eating_dried = total_population * median,
           people_eating_dried_lo = total_population * q5,
           people_eating_dried_hi = total_population * q95)
