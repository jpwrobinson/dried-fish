lsms_read<-function(path){
    
    lsms<-read.csv(path) %>% 
        mutate(form2 = ifelse(form %in% c('dried', 'dry/smoked', 'smoked'), 'dried', form)) %>% 
        group_by(hh_id, form2, lat, lon, country) %>% 
        summarise(n = length(unique(form2))) 
}
