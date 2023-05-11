library(tidyverse)

## summary figs of small pelagics trade
pdf(file = 'fig/small_pelagic_trade_summary.pdf', height=6, width=10)
label_lim<-10000
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv')
source('scripts/00_source_trade_figs.R')
dev.off()

pdf(file = 'fig/small_pelagic_trade_summary_withoutMauritania.pdf', height=6, width=10)
label_lim<-1000
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% filter(source_country_iso3c != 'MRT')
source('scripts/00_source_trade_figs.R')
dev.off()