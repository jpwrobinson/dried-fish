
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
        filter(    !is.na(live_weight_t) & 
                   !is.na(product_weight_t) & 
                   exporter_iso3c != 'NEI' &
                   method=='capture' & 
                   source_region == 'Africa' & 
                   sciname %in% foc
                   ) %>%
        mutate(export_region = countrycode(exporter_iso3c, origin = 'iso3c', destination = 'continent'),
               import_region = countrycode(importer_iso3c, origin = 'iso3c', destination = 'continent'),
               source_region = countrycode(source_country_iso3c, origin = 'iso3c', destination = 'continent')) 
    
    print(files[i])
    artis<-rbind(artis, a)
}

# write.csv(artis, file = 'data/trade/artis_small_pelagics.csv', row.names=FALSE)



# Now with artis R package - note this is a subset of data
library(exploreARTIS)
# save small pelagic species from trade csv
sp<-unique(trade$sciname)



# Filter ARTIS data to small pelagic fishes
spf<-mini_artis %>% filter(
    region == 'Africa',
    sciname %in% sp) %>% 
    mutate(export_region = countrycode(exporter_iso3c, origin = 'iso3c', destination = 'continent'),
           import_region = countrycode(importer_iso3c, origin = 'iso3c', destination = 'continent'),
           source_region = countrycode(source_country_iso3c, origin = 'iso3c', destination = 'continent'),
           trade_direction = ifelse(
               import_region == source_region & export_region == source_region, 'Regional\n[within Africa]', glob),
           trade_direction = ifelse(
               export_region != source_region & import_region == source_region, 'Double imported\n[Global to Africa]', trade_direction),
           trade_direction = ifelse(
               export_region != source_region & import_region != source_region, glob, trade_direction))

# aggregate by year 
source_agg_artis<-spf %>%
    group_by(trade_direction, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) 

# plot total import by trade direction
ggplot(source_agg_artis, 
       aes(year, product_weight_t, group=trade_direction, col=trade_direction)) + 
    geom_line() +
    geom_label(data = source_agg_artis %>% filter(year ==2019),
               aes(label=trade_direction), size=3) +
    guides(col='none') + 
    scale_y_continuous(labels=scales::comma) +
    coord_cartesian(clip='off') +
    labs(x= '', y = 'Trade of small pelagic catch, t')
