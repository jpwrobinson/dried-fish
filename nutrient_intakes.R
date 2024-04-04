
targets::tar_load(nut_data)

nut_data %>% 
    filter(!form %in% c('Fresh', 'Fresh, gutted'),
           nutrient %in% nuts) %>% 
    group_by(nutrient) %>% 
    summarise(value = mean(value)) %>%
    left_join(rda) %>% 
    mutate(prop_rni_kids = value*portionK/100 / rni_kids*100,
           prop_rni_women = value*portionW/100 / rni_women*100)


## read proportion of dried fish eaters
targets::tar_load(lsms_proximity)

prop_dried<-lsms_proximity %>% 
    group_by(country, dried, tot_hh) %>% 
    summarise(prop_dried = length(dried) / tot_hh)

# join populations
class<-WDI(extra=TRUE)
# WDIsearch('total.*female.*population')[1:20,]

pop.df <- getWDItoSYB(name = "total_population", indicator = "SP.POP.TOTL")$entity %>%
    clean_names() %>% filter(year == 2020) %>%
    left_join(class %>% distinct(country, region, capital, longitude, latitude, income), by = 'country') %>%
    select(-year) %>% 
    mutate(iso3 = countrycode(iso2_wb_code, origin = 'iso2c', destination = 'iso3c'),
           income = ifelse(iso2_wb_code=='VN', 'Lower middle income', income)) %>% 
    left_join(read.csv('data/FAOSTAT_data_en_6-26-2023.csv') %>%  ## from EAT data
                  clean_names() %>% filter(year == 2020 & item %in% c('Cost of a healthy diet (PPP dollar per person per day)')) %>% 
                  mutate(iso3 = countrycode(area, origin = 'country.name.en', destination = 'iso3c'),
                         cost_healthy_diet_ppp_person_day = value) %>% 
                  select(year, cost_healthy_diet_ppp_person_day, iso3), by = 'iso3') %>% 
    filter(iso3 %in% lsms_proximity$country) %>% 
    left_join(prop_dried %>% mutate(iso3 = country), by = 'iso3') %>% 
    mutate(people_eating_dried = total_population * prop_dried)
