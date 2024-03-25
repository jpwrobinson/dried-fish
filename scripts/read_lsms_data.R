lsms_read<-function(path){
    
    lsms<-read.csv(path) %>% 
        mutate(form2 = ifelse(form %in% c('dried', 'dry/smoked', 'smoked'), 'dried', form)) %>% 
        group_by(hh_id, form2, lat, lon, country) %>% 
        summarise(n = length(unique(form2))) %>% 
        dplyr::select(-n) %>% 
        filter(form2 == 'dried' & !is.na(lat))
}

lsms_to_csv<-function(dat){
    write.csv(dat, file = 'data/lsms_dried_fish_with_covariates.csv', row.names=FALSE)
}