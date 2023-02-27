library(tidyverse)
library(ggrepel)
theme_set(theme_bw())

wac<-unique(trade$source_country_iso3c)

trade<-trade %>% filter(nceas_group=='small pelagics') %>% 
    mutate(exporter_region = ifelse(exporter_iso3c %in% wac, 'West Africa', 'Foreign'),
           trade_direction = ifelse(!(exporter_iso3c %in% wac) & importer_iso3c %in% wac, 'Foreign to\nWest Africa', 'XX'),
           trade_direction = ifelse((exporter_iso3c %in% wac) & importer_iso3c %in% wac, 'West Africa to\nWest Africa', trade_direction),
           trade_direction = ifelse((exporter_iso3c %in% wac) & !(importer_iso3c %in% wac), 'West Africa to\nForeign', trade_direction),
           trade_direction = ifelse(!(exporter_iso3c %in% wac) & !(importer_iso3c %in% wac), 'Foreign to\nForeign', trade_direction),
           reimport = ifelse(exporter_iso3c != source_country_iso3c & importer_iso3c != source_country_iso3c, 'Double import', 'Single import'))

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

reimp<-trade %>% 
    group_by(year, reimport) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(reimport) %>% 
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



## estimate small pelagic trade as proportion of total trade
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    group_by(source_country_iso3c, year) %>% 
    mutate(total_traded = sum(live_weight_t)) 

# save top 50% of trading countries
top50_iso <- trade %>% group_by(source_country_iso3c) %>% 
    summarise(total_traded = sum(live_weight_t)) %>% slice_max(total_traded, prop = .5)

trade_small_prop<- trade %>% 
    group_by(source_country_iso3c, year, total_traded, nceas_group) %>% 
    summarise(traded_catch = sum(live_weight_t)) %>% 
    mutate(prop_catch = traded_catch / total_traded,
           top50 = ifelse(source_country_iso3c %in% top50_iso$source_country_iso3c, 'Top-50%', 'Bot-50%'))

# sanity check
# trade_small_prop %>% group_by(source_country_iso3c, year) %>% summarise(p = sum(prop_catch)) %>% ungroup() %>% distinct(p)


## values start and end of time-series
# ts_plot<-trade_small_prop %>% filter(nceas_group=='small pelagics') %>% 
#     group_by(source_country_iso3c, traded_catch, top50) %>% 
#     summarise(start = head(prop_catch, 1), end = tail(prop_catch,1)) %>% 
#     pivot_longer(-c(source_country_iso3c, top50, traded_catch), names_to = 'time', values_to = 'p') %>% 
#     mutate(p = round(p*100, 0))
# 
# library(CGPfunctions)
# 
# newggslopegraph(dataframe = ts_plot %>% filter(top50 == 'Top-50%'),
#                 Times = time,
#                 Measurement = p,
#                 Grouping = source_country_iso3c)

## circular trade so total production = total imports = total exports
print(
    ggplot(alls %>% filter(type=='Source/production'), aes(year, product_weight_t, col=type)) + 
    geom_line() + 
    labs(subtitle = 'West Africa: small pelagic fish trade') +
    theme(legend.position = c(0.2, 0.6), legend.title = element_blank())
)
    
print(
    ggplot(source_agg, aes(year, product_weight_t, group=source_country_iso3c, col=source_country_iso3c)) + 
    geom_line() +
    geom_label(data = source_agg %>% filter(year ==2019 & maxer>label_lim), 
               aes(label=source_country_iso3c), size=3) +
    labs(subtitle = 'Small pelagics: source countries') +
guides(col='none')
)
    

print(
    ggplot(exp_agg, 
       aes(year, product_weight_t, group=exporter_iso3c, col=exporter_iso3c)) + 
    geom_line(data=exp_agg %>% filter(maxer<label_lim), col='grey') +
    geom_line(data=exp_agg %>% filter(maxer>label_lim)) +
    geom_label(data = exp_agg %>% filter(year ==2019 & maxer>label_lim),
               aes(label=exporter_iso3c), size=3) +
    guides(col='none') + 
    labs(subtitle = 'Small pelagics: exporter countries')
)

print(
    ggplot(exp_region, 
       aes(year, product_weight_t, group=exporter_region)) + 
    geom_line(data=exp_region, col='black') +
    geom_label(data = exp_region %>% filter(year ==2019 & maxer>label_lim),
               aes(label=exporter_region), size=3) +
    labs(subtitle = 'Small pelagics: exporter regions')
)

print(
    ggplot(trade_dir, 
       aes(year, product_weight_t, group=trade_direction, col=trade_direction)) + 
    geom_line(data=trade_dir) +
    geom_label(data = trade_dir %>% filter(year ==2019 & maxer>label_lim) %>% mutate(year=2021),
               aes(label=trade_direction), size=2) +
    labs(subtitle = 'Small pelagics: trade direction') +
    guides(col='none')
)

print(
    ggplot(reimp, 
       aes(year, product_weight_t, group=reimport, col=reimport)) + 
    geom_line(data=reimp) +
    geom_label(data = reimp %>% filter(year ==2019 & maxer>label_lim) %>% mutate(year=2021),
               aes(label=reimport), size=2) +
    labs(subtitle = 'Small pelagics: catch is imported once (single) or twice (double)') +
    guides(col='none')
)

print(
    ggplot(imp_agg, 
       aes(year, product_weight_t, group=importer_iso3c, col=importer_iso3c)) + 
    geom_line(data=imp_agg %>% filter(maxer<label_lim), col='grey90') +
    geom_line(data=imp_agg %>% filter(maxer>label_lim)) +
    geom_label(data = imp_agg %>% filter(year == 2019 & maxer>label_lim),
               aes(label=importer_iso3c), size=2) +
    guides(colour='none') +
    labs(subtitle = 'Small pelagics: importer countries')
)

print(
    ggplot(sp_agg, aes(year, product_weight_t, group=sciname)) + 
    geom_line() +
    lims(x = c(1995, 2024)) +
    geom_label(data = sp_agg %>% filter(year ==2019 & maxer>label_lim) %>% mutate(year=2022), 
               aes(label=sciname), size=3) +
    labs(subtitle = 'Small pelagic species')
)

print(
    ggplot(imp_agg %>% group_by(year) %>% summarise(n = n_distinct(importer_iso3c)),
       aes(year, n)) + geom_line() +
    labs(subtitle = 'Number of small pelagic importer countries', y = 'n countries')
)

print(
ggplot(trade_small_prop %>% filter(nceas_group=='small pelagics'), 
       aes(year, prop_catch*100, colour=source_country_iso3c)) + geom_line() +
    facet_wrap(~top50) +
    labs(subtitle = 'Proportion of small pelagic catch over time', y = 'proportion trade catch, %', x= '')
)
