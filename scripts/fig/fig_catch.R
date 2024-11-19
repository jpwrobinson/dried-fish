library(tidyverse)
library(ggrepel)
# theme_set(theme_bw())

glob<-'Global\n[outside Africa]'

trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    mutate(export_region = countrycode(exporter_iso3c, origin = 'iso3c', destination = 'region'),
           import_region = countrycode(importer_iso3c, origin = 'iso3c', destination = 'region'),
           source_region = countrycode(source_country_iso3c, origin = 'iso3c', destination = 'region'))

trade<-trade %>% filter(nceas_group=='small pelagics') %>% 
    mutate(
        trade_direction = ifelse(
            import_region == source_region & export_region == source_region, 'Regional\n[within Africa]', glob),
        trade_direction = ifelse(
               export_region != source_region & import_region == source_region, 'Double imported\n[Global to Africa]', trade_direction),
        trade_direction = ifelse(
            export_region != source_region & import_region != source_region, glob, trade_direction))

# Aggregate for plots
# trade_agg<-trade %>% mutate(period = cut(year, breaks = 3)) %>% 
#     group_by(period, exporter_iso3c, importer_iso3c, dom_source) %>% 
#     summarise(product_weight_t = sum(product_weight_t),
#               live_weight_t = sum(live_weight_t))
# 
# plot_chord(trade_agg %>% filter(period == '(1996,2004]'),
#            region_colors = region7_palette)
# 
# plot_chord(trade_agg %>% filter(period == '(2011,2019]'),
#            region_colors = region7_palette)
# 
# plot_map(trade_agg %>% filter(period == '(1996,2004]'),
#          country_fill = "importer_iso3c",
#          flow_arrows = TRUE,
#          arrow_label = "Trade (live t)",
#          fill_label = "Import (live t)")

source_agg<-trade %>% 
    group_by(source_region, trade_direction, nceas_group, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(source_region) %>% 
    mutate(maxer = max(product_weight_t)) %>% 
    filter(!is.na(trade_direction)) # these are NEI / SCG values

gartis<-ggplot(source_agg, 
           aes(year, product_weight_t, group=trade_direction, col=trade_direction)) + 
        geom_line(data=source_agg %>% filter(maxer<10000), col='grey') +
        geom_line(data=source_agg %>% filter(maxer>10000)) +
        geom_label(data = source_agg %>% filter(year ==2019 & maxer>10000),
                   aes(label=trade_direction), size=3) +
        guides(col='none') + 
        scale_y_continuous(labels=scales::comma) +
        coord_cartesian(clip='off') +
        labs(x= '', y = 'Trade of small pelagic catch, t')


source('scripts/artis/faostat_data.R')


pdf(file = 'fig/fig_artis.pdf', height=5, width=7)

# plot_grid(gfao, gartis, nrow=2, labels=c('', 'c'), rel_heights=c(1, 0.5))\
gartis + theme(plot.margin = unit(c(1,1,1,1), 'cm'))

dev.off()
