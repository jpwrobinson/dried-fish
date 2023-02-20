
## Script to read in all norway results by sheet, combining ww and dw (which were incorrectly separated by technicians)

## combine mineral + vitamin sheets
sheets<-c(1,2,3,4,5,6,7,9,12)

drop_vars<-c('project', 'customer', 'jnr_analysis_replicate', 'batch', 
             'product', 'subproduct', 'project_comments', 'sample_comments', 'variation', 'test_comments', 'test_status', 'reviewed_by')

# read most sheets, as these have just one sample type (_ww)
minerals<-c(5,7) # skip these sheets
for(i in 1:length(sheets)){
    if(i %in% minerals) next
    df<-read_excel(path, sheet = sheets[i]) %>% clean_names() 
    if(i == 1){dat <- df %>% select(-any_of(drop_vars))} 
    if(i != 1){dat <- dat %>% left_join(df, by = 'customer_marking') %>% select(-any_of(drop_vars))}
}

dat<-dat %>% 
    mutate(customer_marking = recode(customer_marking, A_001 = 'A_005')) %>% 
    rename(sample_id = customer_marking) %>% 
    select(-folat_mg_100_g_ww, -ends_with('dw')) %>% 
    rename_all(~sub('_ww', '', .x))

metat<-dat %>% left_join(metat)
rm(dat)

# dried / wet weight fuck ups
for(i in minerals){
    df<-read_excel(path, sheet = sheets[i]) %>% clean_names() %>%
        select(-any_of(drop_vars)) %>% 
        mutate_if(is.numeric, as.character) %>% 
        pivot_longer(-customer_marking, names_to = 'nut', values_to = 'value') %>% 
        separate(nut, into=c("nut", "type"), sep="kg_") %>% 
        filter(value != "'") %>% 
        select(-type) %>% 
        pivot_wider(names_from = 'nut', values_from = 'value', names_glue = paste0("{nut}","kg_ww"))
    if(i == 5){dat<-df}
    if(i == 7){dat<-dat %>% left_join(df, by = 'customer_marking')}
}

dat<-dat %>% mutate(customer_marking = recode(customer_marking, A_001 = 'A_005')) %>% 
    rename(sample_id = customer_marking) %>% 
    rename_all(~sub('_ww', '', .x))

metat<-dat %>% left_join(metat, by ='sample_id')

## long version with trace values removed
datl<-metat %>% mutate_if(is.numeric, as.character) %>% 
    pivot_longer(-c(sample_id,date:latin_name, dry_matter_g_100g), values_to = 'value', names_to = 'nutrient') %>% 
    mutate(unit = ifelse(str_detect(nutrient, 'dry|protein|torrst'),'g_100g','mg_kg'),
           nutrient = str_replace_all(nutrient, '_mg_kg_mg_kg', '_mg_kg'),
           nutrient = str_replace_all(nutrient, '_mg_kg', ''),
           nutrient = str_replace_all(nutrient, '_g_100g', ''),
           nutrient = str_replace_all(nutrient, '_mg_100_g', ''),
           nutrient = str_replace_all(nutrient, '_mg_kg', ''),
           nutrient = str_replace_all(nutrient, '_percent_g_100g', '')) %>% 
    mutate(value = ifelse(str_detect(value, '<'), NA, as.numeric(value)),
           nutrient = recode(nutrient, v = 'vanadium', cr = 'chromium', mn = 'manganese',
                             fe = 'iron', co = 'cobalt', ni = 'nickel', cu = 'copper', zn = 'zinc',
                             as = 'arsenic', se = 'selenium', mo = 'molybdenum', ag = 'silver', cd = 'cadmium',
                             hg = 'mercury', pb = 'lead', jod = 'iodine', ca = 'calcium', na = 'sodium', k = 'potassium', mg = 'magnesium',
                             p = 'phosphorus', folat = 'folate', cobalamin = 'vitamin_b12')) %>% 
    mutate(value = ifelse(str_detect(nutrient, 'vitamin|selenium|folat|iodi'), value*1000, value)) %>% 
    mutate(value = ifelse(unit =='mg_kg', value/10, value)) %>% 
    mutate(unit = ifelse(str_detect(nutrient, 'vitamin|selenium|folat|iodi'), 'mug_100g', 'mg_100g')) %>% 
    mutate(unit = ifelse(str_detect(nutrient, 'protein'), 'g_100g', unit)) 

write.csv(metat, file = paste0('data/clean/', filesave, '_wide.csv'))
write.csv(datl, file = paste0('data/clean/', filesave, '_long.csv'))