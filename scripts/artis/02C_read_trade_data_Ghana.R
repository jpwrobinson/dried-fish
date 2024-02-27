

label_lim<-1000
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    filter(source_country_iso3c == 'GHA' | importer_iso3c == 'GHA') %>% 
    filter(nceas_group=='small pelagics')

catch<-read.csv('data/SAU EEZ 288 v50-1/SAU EEZ 288 v50-1.csv')

sp<-c('Small pelagics (<30 cm)')
catch_sp<-catch %>% filter(functional_group %in% sp & catch_type == 'Landings') %>% 
    group_by(year, reporting_status) %>% 
    summarise(tonnes = sum(tonnes))

catch_sp_sp<-catch %>% filter(functional_group %in% sp & catch_type == 'Landings' & reporting_status == 'Reported') %>% 
    group_by(year, scientific_name, reporting_status) %>% 
    summarise(tonnes = sum(tonnes))

# catch use data just show >99% for direct human consumption
# dhc<-catch %>% filter(functional_group %in% sp & catch_type == 'Landings') %>% 
#     group_by(year, end_use_type) %>% 
#     summarise(tonnes = sum(tonnes))

source_agg<-trade %>% 
    group_by(source_country_iso3c, nceas_group, year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(source_country_iso3c) %>% 
    mutate(maxer = max(product_weight_t)) #%>% 
# mutate(importer_iso3c = fct_lump_n(importer_iso3c, 10, w = product_weight_t))

imp_agg<-trade %>% 
    filter(importer_iso3c=='GHA') %>% 
    group_by(year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    mutate(maxer = max(product_weight_t))

exp_agg<-trade %>% 
    filter(source_country_iso3c=='GHA') %>% 
    group_by(year) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>%
    mutate(maxer = max(product_weight_t))


sp_agg<-trade %>% 
    group_by(year, sciname) %>% 
    mutate(loc = ifelse(source_country_iso3c == "GHA", "Exported", "Imported")) %>% 
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>% 
    group_by(sciname) %>% 
    mutate(maxer = max(product_weight_t))

total<-rbind(
    catch_sp %>% filter(reporting_status == 'Reported') %>% mutate(dat = 'Catch') %>% select(year, dat, tonnes),
    imp_agg %>% mutate(tonnes = live_weight_t, dat = 'Imports') %>% select(year, dat, tonnes),
    exp_agg %>% mutate(tonnes = live_weight_t, dat = 'Exports') %>% select(year, dat, tonnes)
)

pdf(file = 'fig/small_pelagic_trade_summary_Ghana.pdf', height=6, width=10)

# ## production + imports - exports
# print(
#     ggplot(source_agg %>% mutate(loc = ifelse(source_country_iso3c == "GHA", "Exported", "Imported")) %>% 
#                group_by(year, loc) %>% 
#                summarise(product_weight_t = sum(product_weight_t)),
#                                  aes(year, product_weight_t, fill = loc)) +
#         geom_bar(stat='identity') + 
#         labs(subtitle = 'Ghana: total small pelagic fish landed, imported + exported, t') +
#         theme(legend.position = c(0.2, 0.6), legend.title = element_blank())
# )

print(
    ggplot(catch_sp,
           aes(year, tonnes, col = reporting_status)) +
        geom_line() + 
        labs(subtitle = 'Ghana: small pelagic fish landings, t') +
        theme(legend.position = c(0.2, 0.6), legend.title = element_blank())
)

print(
    ggplot(catch_sp_sp,
           aes(year, tonnes, col = scientific_name)) +
        geom_line() + 
        labs(subtitle = 'Ghana: two species dominate landings (Engraulis encrasicolus & Sardinella aurita)') +
        theme(legend.position = c(0.15, 0.6), legend.title = element_blank())
)


print(
    ggplot(source_agg %>% mutate(loc = ifelse(source_country_iso3c == "GHA", "Exported", "Imported")) %>% 
               group_by(year, loc) %>% 
               summarise(product_weight_t = sum(product_weight_t)),
           aes(year, product_weight_t, col = loc)) +
        geom_line() + 
        labs(subtitle = 'Ghana: small pelagic fish imported + exported, t') +
        theme(legend.position = c(0.2, 0.6), legend.title = element_blank())
)

print(
    ggplot(sp_agg, aes(year, product_weight_t, group=sciname)) + 
        geom_line() +
        lims(x = c(1995, 2024)) +
        geom_label(data = sp_agg %>% filter(year ==2019 & maxer>label_lim) %>% mutate(year=2022), 
                   aes(label=sciname), size=3) +
        labs(subtitle = 'Small pelagic fish traded are mostly sardinella sp.')
)


print(
    ggplot(total,
           aes(year, tonnes, col=dat)) +
        geom_line() + 
        lims(x = c(1990, 2020)) +
        labs(subtitle = 'Ghana: total small pelagic fish production') +
        theme(legend.position = c(0.8, 0.8), legend.title = element_blank())
)


dev.off()

