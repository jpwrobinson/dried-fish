library(tidyverse)
library(ggrepel)

trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv')
wac<-unique(trade$source_country_iso3c)

trade<-trade %>% filter(nceas_group=='small pelagics') %>% 
    mutate(exporter_region = ifelse(exporter_iso3c %in% wac, 'West Africa', 'Foreign'),
           trade_direction = ifelse(!(exporter_iso3c %in% wac) & importer_iso3c %in% wac, 'Foreign to\nWest Africa', 'XX'),
           trade_direction = ifelse((exporter_iso3c %in% wac) & importer_iso3c %in% wac, 'West Africa to\nWest Africa', trade_direction),
           trade_direction = ifelse((exporter_iso3c %in% wac) & !(importer_iso3c %in% wac), 'West Africa to\nForeign', trade_direction),
           trade_direction = ifelse(!(exporter_iso3c %in% wac) & !(importer_iso3c %in% wac), 'Foreign to\nForeign', trade_direction))

# Aggregate for plots

source_agg<-trade %>% 
    group_by(source_country_iso3c, nceas_group, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(source_country_iso3c) %>% 
    mutate(maxer = max(product_weight_t)) #%>% 
    # mutate(importer_iso3c = fct_lump_n(importer_iso3c, 10, w = product_weight_t))

imp_agg<-trade %>% 
        group_by(year, importer_iso3c) %>% 
        summarise(product_weight_t = sum(product_weight_t),
                    live_weight_t = sum(live_weight_t)) %>% 
        group_by(importer_iso3c) %>% 
        mutate(maxer = max(product_weight_t))

exp_agg<-trade %>% 
    group_by(year, exporter_iso3c) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(exporter_iso3c) %>% 
    mutate(maxer = max(product_weight_t))

exp_region<-trade %>% 
    group_by(year, exporter_region) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(exporter_region) %>% 
    mutate(maxer = max(product_weight_t))

trade_dir<-trade %>% 
    group_by(year, trade_direction) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(trade_direction) %>% 
    mutate(maxer = max(product_weight_t))

sp_agg<-trade %>% 
    group_by(year, sciname) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(sciname) %>% 
    mutate(maxer = max(product_weight_t))

alls<-rbind(
    source_agg %>% group_by(year) %>% summarise(product_weight_t = sum(product_weight_t)) %>% mutate(type = 'Source/production'),
    imp_agg %>% group_by(year) %>% summarise(product_weight_t = sum(product_weight_t)) %>% mutate(type = 'Imported'),
    exp_agg %>% group_by(year) %>% summarise(product_weight_t = sum(product_weight_t)) %>% mutate(type = 'Exported')
)

label_lim<-10000

## circular trade so total production = total imports = total exports
ggplot(alls %>% filter(type=='Source/production'), aes(year, product_weight_t, col=type)) + 
    geom_line() + 
    labs(subtitle = 'West Africa: small pelagic fish trade') +
    theme(legend.position = c(0.2, 0.6), legend.title = element_blank())


ggplot(source_agg, aes(year, product_weight_t, group=source_country_iso3c)) + 
    geom_line() +
    geom_label(data = source_agg %>% filter(year ==2019 & maxer>label_lim), 
               aes(label=source_country_iso3c), size=3) +
    labs(subtitle = 'Small pelagics: source countries')

ggplot(exp_agg, 
       aes(year, product_weight_t, group=exporter_iso3c)) + 
    geom_line(data=exp_agg %>% filter(maxer<label_lim), col='grey') +
    geom_line(data=exp_agg %>% filter(maxer>label_lim), col='black') +
    geom_label(data = exp_agg %>% filter(year ==2019 & maxer>label_lim),
               aes(label=exporter_iso3c), size=3) +
    labs(subtitle = 'Small pelagics: exporter countries')

ggplot(exp_region, 
       aes(year, product_weight_t, group=exporter_region)) + 
    geom_line(data=exp_region, col='black') +
    geom_label(data = exp_region %>% filter(year ==2019 & maxer>label_lim),
               aes(label=exporter_region), size=3) +
    labs(subtitle = 'Small pelagics: exporter regions')

ggplot(trade_dir, 
       aes(year, product_weight_t, group=trade_direction, col=trade_direction)) + 
    geom_line(data=trade_dir) +
    geom_label(data = trade_dir %>% filter(year ==2019 & maxer>label_lim) %>% mutate(year=2021),
               aes(label=trade_direction), size=2) +
    labs(subtitle = 'Small pelagics: trade direction') +
    guides(col='none')

ggplot(imp_agg, 
       aes(year, product_weight_t, group=importer_iso3c)) + 
    geom_line(data=imp_agg %>% filter(maxer<label_lim), col='grey90') +
    geom_line(data=imp_agg %>% filter(maxer>label_lim), col='black') +
    geom_label(data = imp_agg %>% filter(year == 2019 & maxer>label_lim),
               aes(label=importer_iso3c), size=2) +
    labs(subtitle = 'Small pelagics: importer countries')

ggplot(sp_agg, aes(year, product_weight_t, group=sciname)) + 
    geom_line() +
    lims(x = c(1995, 2024)) +
    geom_label(data = sp_agg %>% filter(year ==2019 & maxer>label_lim) %>% mutate(year=2022), 
               aes(label=sciname), size=3) +
    labs(subtitle = 'Small pelagic species')

ggplot(imp_agg %>% group_by(year) %>% summarise(n = n_distinct(importer_iso3c)),
       aes(year, n)) + geom_line() +
    labs(subtitle = 'Number of small pelagic importer countries', y = 'n countries')
