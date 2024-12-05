

targets::tar_load(lsms_proximity)
lsms_proximity %>% 
    filter(country == 'SEN') %>% 
    select(hh_id:any_fish) %>% 
    write.csv('data/senegal_from_lsms.csv', row.names=FALSE)
