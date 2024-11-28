# devtools::install_github("Seafood-Globalization-Lab/exploreARTIS@v1.0.0", dependencies = TRUE)
# note I had to install ggsankey manually

# loading data
# add region codes for export/import/source
artis<-#read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    read.csv('data/trade/artis_small_pelagics.csv') %>% 
    mutate(subregion = countrycode(source_country_iso3c, 'iso3c', 'un.regionintermediate.name'),
           subregion = ifelse(is.na(subregion), 'Northern Africa', subregion))
    # mutate(export_region = countrycode(exporter_iso3c, origin = 'iso3c', destination = 'continent'),
    #        import_region = countrycode(importer_iso3c, origin = 'iso3c', destination = 'continent'),
    #        source_region = countrycode(source_country_iso3c, origin = 'iso3c', destination = 'continent'))

# define trade directions
tdir<-data.frame(trade_direction = c('global', 'region', 'double'),
                 lab = c('Global\n[outside Africa]', 'Regional\n[within Africa]', 'Double imported\n[Global to Africa]')
)
                 

trade<-artis %>% 
    # filter(nceas_group=='small pelagics') %>% 
    mutate(
        trade_direction = ifelse(
            import_region == source_region & export_region == source_region, 'region', 'global'),
        trade_direction = ifelse(
            export_region != source_region & import_region == source_region, 'double', trade_direction),
        trade_direction = ifelse(
            export_region != source_region & import_region != source_region, 'global', trade_direction)) %>% 
    left_join(tdir)

# summarise total trade by source region
source_agg<-trade %>% 
    group_by(source_region, trade_direction, lab, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(source_region) %>% 
    mutate(maxer = max(product_weight_t)) %>% 
    filter(!is.na(trade_direction)) # these are NEI / SCG values

source_iso<-trade %>% 
    filter(!is.na(trade_direction) & trade_direction != 'Double imported\n[Global to Africa]')  %>% 
    group_by(subregion, source_country_iso3c, source_region, trade_direction, lab, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(source_country_iso3c, source_region, trade_direction) %>% 
    mutate(maxer = max(product_weight_t), scaler = rescale(product_weight_t, to = c(0,1))) 

source_iso_prop<-source_iso %>% 
    pivot_wider(id_cols = c(-lab, -scaler, -maxer, -product_weight_t), 
                names_from = 'trade_direction', values_from = 'live_weight_t', values_fill = 0) %>% 
    mutate(tot = global + region + double, 
                  prop = global / tot) %>% 
    group_by(source_country_iso3c) %>% 
    mutate(nyears = n_distinct(year), meaner = mean(tot))

pdf(file = 'fig/artis_explore.pdf')

# plot total import by trade direction
ggplot(source_agg, 
               aes(year, product_weight_t, group=trade_direction, col=trade_direction)) + 
    geom_line() +
    geom_label(data = source_agg %>% filter(year ==2020) %>% mutate(year=2022),
               aes(label=trade_direction), size=3) +
    guides(col='none') + 
    scale_y_continuous(labels=scales::comma) +
    coord_cartesian(clip='off') +
    labs(x= '', y = 'Trade of small pelagic catch, t')

ggplot(source_iso, 
       aes(year, scaler, group=source_country_iso3c, fill=source_country_iso3c)) + 
    geom_line() +
    geom_label(data = source_iso %>% filter(year ==2020) %>% mutate(year=2022),
               aes(label=source_country_iso3c), size=3) +
    facet_wrap(~trade_direction) +
    guides(col='none', fill='none') + 
    scale_y_continuous(labels=scales::comma) +
    coord_cartesian(clip='off') +
    labs(x= '', y = 'Trade of small pelagic catch, t')

ggplot(source_iso_prop %>% filter(nyears > 20 & meaner > 1000 & prop > 0), 
       aes(year, prop, group=source_country_iso3c, col=log10(meaner), alpha=log10(meaner))) + 
       geom_line() +
    facet_wrap(source_country_iso3c~subregion) +
    # geom_label(data = source_iso_prop %>% filter(year ==2020) %>% mutate(year=2022),
    #            aes(label=source_country_iso3c), size=3) +
    scale_y_continuous(labels=label_percent()) +
    coord_cartesian(clip='off') +
    stat_smooth() +
    labs(x= '', y = 'Small pelagic catch exported globally')


subs<-unique(artis$subregion)
for(i in 1:length(subs)){
    print(
        plot_sankey(artis %>% filter(subregion == subs[i]),
                cols = c("sciname", "exporter_iso3c", "importer_iso3c")))
}
dev.off()

