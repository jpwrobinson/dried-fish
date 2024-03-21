source('scripts/00_plot.R')

## SAU from https://www.seaaroundus.org/data/#/eez/686?chart=catch-chart&dimension=taxon&measure=tonnage&limit=10

sp<-c('Small pelagics (<30 cm)')

## Ghana + Senegal catches
## Note that Deme et al. 2023 include Horse mackerel (Trachurus) as small pelagic, but this is medium pelagic in SAU
catch<-rbind(
    read.csv('data/SAU EEZ 288 v50-1/SAU EEZ 288 v50-1.csv'),
    read.csv('data/SAU EEZ 686 v50-1/SAU EEZ 686 v50-1.csv')) %>% 
    filter(catch_type == 'Landings' & functional_group %in% sp & fishing_sector != 'Subsistence') %>% 
    # filter(end_use_type=='Direct human consumption') %>% 
    group_by(area_name, area_type, year, fishing_sector) %>% 
    summarise(tonnes = sum(tonnes), landed_value = sum(landed_value))

catch_all<-catch %>% group_by(area_name, area_type, year) %>% 
            summarise(tonnes = sum(tonnes), landed_value = sum(landed_value))


gcat<-ggplot(catch, aes(year, tonnes, colour=fishing_sector)) +
    geom_line() + 
    geom_line(data=catch_all, col='black') + 
    geom_label(data = catch %>% filter(year ==2019 & area_name =='Senegal') %>% mutate(year=2028), 
               aes(label=fishing_sector), size=3) +
    geom_label(data = catch_all %>% filter(year ==2019 & area_name =='Senegal') %>% mutate(year=2028, fishing_sector='Total'), 
               aes(label=fishing_sector), size=3, colour='black') +
    facet_wrap(~area_name) +
    coord_cartesian(clip='off') +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(limits=c(1950, 2030)) +
    labs(x = '', title = 'Reconstructed landings of small pelagic fishes') +
    theme(legend.position ='none',  legend.title=element_blank())

# ggplot(catch %>% filter(end_use_type=='Fishmeal and fish oil'), aes(year, tonnes)) +
#     geom_line() + 
#     facet_wrap(~area_name) +
#     scale_y_continuous(labels=scales::comma) +
#     labs(x = '')


# trade
cts<-c('GHA', 'SEN')
export<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    filter(source_country_iso3c %in% cts) %>% 
    filter(nceas_group=='small pelagics') %>% 
    mutate(country = ifelse(source_country_iso3c == 'GHA', 'Ghana', 'Senegal')) %>% 
    filter(dom_source == 'domestic export') %>% 
    group_by(source_country_iso3c, country, year) %>% 
    summarise(live_weight_t = sum(live_weight_t)) %>% mutate(type = 'Export')

import<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    filter(importer_iso3c %in% cts) %>% 
    filter(nceas_group=='small pelagics') %>% 
    mutate(country = ifelse(importer_iso3c == 'GHA', 'Ghana', 'Senegal')) %>% 
    filter(dom_source == 'domestic export') %>% 
    group_by(importer_iso3c, country, year) %>% 
    summarise(live_weight_t = sum(live_weight_t)) %>% mutate(type = 'Import')

trade<-rbind(export, import)

gex<-ggplot(trade, aes(year, live_weight_t, col=country, linetype=type)) + geom_line() +
    geom_label(data = export %>% filter(year ==2019) %>% mutate(year=2023), 
               aes(label=country), size=3) +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(limits=c(1995, 2024)) +
    guides(colour='none') +
    coord_cartesian(clip='off') +
    labs(x = '', title = 'Trade of small pelagic fishes', y = 'tonnes') +
    theme(legend.position =c(0.9, 0.5),  legend.title=element_blank())


## supply exported to what countries?
importers<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    filter(source_country_iso3c %in% cts) %>% 
    filter(nceas_group=='small pelagics') %>% 
    mutate(country = ifelse(source_country_iso3c == 'GHA', 'Ghana', 'Senegal')) %>% 
    filter(dom_source == 'domestic export') %>% 
    group_by(country, importer_iso3c, year) %>% 
    summarise(live_weight_t = sum(live_weight_t)) %>% mutate(type = 'Exporters') %>% 
    mutate(country2 = countrycode::countrycode(importer_iso3c, origin = 'iso3c', destination = 'country.name'))

top5 <- importers %>% group_by(importer_iso3c, country) %>% 
    summarise(total_traded = max(live_weight_t)) %>% 
    group_by(country) %>% 
    slice_max(total_traded, n = 5)
    
importers$top<-ifelse(importers$importer_iso3c %in% top5$importer_iso3c, 'Top', 'Not')

gimp<-ggplot(importers %>% filter(top =='Not'), aes(year, live_weight_t, group=importer_iso3c)) + 
    geom_line(col='grey', alpha=0.8) + 
    geom_line(data=importers %>% filter(top == 'Top'), aes(col=importer_iso3c)) +
    ggrepel::geom_label_repel(data = importers %>% filter(year ==2019 & importer_iso3c %in% top5$importer_iso3c) %>% mutate(year=2024),
          aes(label=country2, col=importer_iso3c), size=3, force=0.2, force_pull=4) +
    scale_x_continuous(limits=c(1995, 2025)) +
    facet_wrap(~country) +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.position = 'none') +
    labs(x = '', y = 'tonnes', title = 'Exports of small pelagic fish from Ghana and Senegal')


pdf(file = 'fig/FigX_small_pelagics_ghana_senegal.pdf', height=3, width=12)
plot_grid(gcat, gex, nrow = 1, labels=c('a', 'b'), rel_widths=c(1, 0.5))
print(gimp)
dev.off()






## Ghana + Senegal catches
fmfo_catch<-rbind(
    read.csv('data/SAU EEZ 288 v50-1/SAU EEZ 288 v50-1.csv'),
    read.csv('data/SAU EEZ 686 v50-1/SAU EEZ 686 v50-1.csv')) %>% 
    filter(catch_type == 'Landings' & functional_group %in% sp & fishing_sector != 'Subsistence') %>% 
    filter(end_use_type=='Fishmeal and fish oil') %>% 
    group_by(year, area_name) %>% 
    summarise(tonnes = sum(tonnes))


gcat<-ggplot(fmfo_catch, aes(year, tonnes, colour=fishing_sector)) +
    geom_line() + 
    geom_label(data = fmfo_catch %>% filter(year ==2019 & area_name =='Senegal') %>% mutate(year=2028), 
               aes(label=fishing_sector), size=3) +
    geom_label(data = catch_all %>% filter(year ==2019 & area_name =='Senegal') %>% mutate(year=2028, fishing_sector='Total'), 
               aes(label=fishing_sector), size=3, colour='black') +
    facet_wrap(~area_name) +
    coord_cartesian(clip='off') +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(limits=c(1950, 2030)) +
    labs(x = '', title = 'Reconstructed landings of small pelagic fishes used in FMFO') +
    theme(legend.position ='none',  legend.title=element_blank())
