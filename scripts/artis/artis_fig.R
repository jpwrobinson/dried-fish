# devtools::install_github("Seafood-Globalization-Lab/exploreARTIS@v1.0.0", dependencies = TRUE)
# note I had to install ggsankey manually

# loading data
# add region codes for export/import/source
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    mutate(export_region = countrycode(exporter_iso3c, origin = 'iso3c', destination = 'continent'),
           import_region = countrycode(importer_iso3c, origin = 'iso3c', destination = 'continent'),
           source_region = countrycode(source_country_iso3c, origin = 'iso3c', destination = 'continent'))

# define trade directions
trade<-trade %>% filter(nceas_group=='small pelagics') %>% 
    mutate(
        trade_direction = ifelse(
            import_region == source_region & export_region == source_region, 'Regional\n[within Africa]', glob),
        trade_direction = ifelse(
            export_region != source_region & import_region == source_region, 'Double imported\n[Global to Africa]', trade_direction),
        trade_direction = ifelse(
            export_region != source_region & import_region != source_region, glob, trade_direction))

# summarise total trade by source region
source_agg<-trade %>% 
    group_by(source_region, trade_direction, nceas_group, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(source_region) %>% 
    mutate(maxer = max(product_weight_t)) %>% 
    filter(!is.na(trade_direction)) # these are NEI / SCG values

# plot total import by trade direction
ggplot(source_agg, 
               aes(year, product_weight_t, group=trade_direction, col=trade_direction)) + 
    geom_line() +
    geom_label(data = source_agg %>% filter(year ==2019),
               aes(label=trade_direction), size=3) +
    guides(col='none') + 
    scale_y_continuous(labels=scales::comma) +
    coord_cartesian(clip='off') +
    labs(x= '', y = 'Trade of small pelagic catch, t')

# Now with artis R package
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
