
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
        filter(nceas_group=='small pelagics') 

sp<-read.csv('data/trade/sciname.csv') %>% 
    filter(isscaap %in% c('Herrings, sardines, anchovies'))

foc<-c(unique(sp$sciname),
      'rastrineobola argentea')

files<-list.files('data/trade/artis_data')
# what about inland species
artis<-numeric()
for(i in 1:length(files)){
    a<-read.csv(paste0('data/trade/artis_data/', files[i])) %>% 
        filter(!is.na(live_weight_t) & !is.na(product_weight_t) & 
                   exporter_iso3c != 'NEI' &
                   method=='capture') %>%
        mutate(export_region = countrycode(exporter_iso3c, origin = 'iso3c', destination = 'continent'),
               import_region = countrycode(importer_iso3c, origin = 'iso3c', destination = 'continent'),
               source_region = countrycode(source_country_iso3c, origin = 'iso3c', destination = 'continent')) %>% 
        filter(source_region == 'Africa' & sciname %in% foc)
    
    artis<-rbind(artis, a)
}
