library(tidyverse)

source('scripts/theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh', 'Sun-dried', 'Smoked','Fried', 'Powder')

## get RDA reference vals
source('scripts/rda_reader.R')
rda$nutrient<-str_to_title(rda$nutrient)
rda$nutrient[rda$nutrient=='Vitamin_a']<-'Vitamin A'
rda$nutrient[rda$nutrient=='Vitamin_d']<-'Vitamin D'
rda$nutrient[rda$nutrient=='Vitamin_b12']<-'Vitamin B12'

units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Omega3', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
## Dried nutrients
nutl<-read.csv('data/clean/dried_nutrient_estimates_long.csv') %>% 
    filter(location %in% c('Mombasa', 'Kisumu')) %>% 
    filter(nutrient %in% nuts) %>%
    filter(! latin_name %in% c('Encrasicholina punctifer')) %>% 
    mutate(nutrient = str_to_title(nutrient)) %>% 
    rename(species = latin_name, fbname = local_name, form = type, mu = value) %>% 
    mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                              #'Omega3', 
                                              'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Folate'))) %>%
    mutate(nutrient = recode(nutrient, #Omega3 = 'Omega-3\nfatty acids', 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) %>% 
    mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
    mutate(fbname = ifelse(species == 'Encrasicholina punctifer', 'Omena (marine)', fbname),
           fbname = ifelse(species == 'Rastrineobola argenteus', 'Omena (freshwater)', fbname))

## units in labels
nutl$lab<-nutl$nutrient
levels(nutl$lab)<-c("'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'),
                    "'Zinc, mg'",expression('Iodine, '*mu*'g'),# "'Omega-3, g'", 
                    expression('Vitamin A, '*mu*'g'),expression('Vitamin B12, '*mu*'g'),
                    expression('Vitamin D, '*mu*'g'),expression('Folate, '*mu*'g'))

nutl_agg<-nutl %>% 
    group_by(species, fbname, nutrient, lab) %>% 
    mutate(n = length(mu)) %>% 
    group_by(form, species, fbname, n, nutrient, lab) %>% 
    summarise(mu = median(mu)) %>% 
    ungroup() %>% droplevels() %>% 
    ## add RDA and units
    left_join(rda) %>% 
    left_join(units) %>% 
    mutate(rni_women = mu/rni_women*100,
           rni_kids = mu/rni_kids*100,
           rni_men = mu/rni_men*100,
           rni_pregnant = mu/rni_pregnant*100) %>% 
    mutate(rni = rni_kids/100/(100/40)) %>% ## 40 g portion for kids intake
    mutate(rni2 = rni_pregnant/100) %>% ## 100 g portion for pregnant women intake
    ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
    mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni),
           rni2 = case_when(rni2 > 1 ~ 1, TRUE ~ rni2)) %>% 
    rowwise() %>% mutate(fbname_long= paste0(unique(species), '\n', unique(fbname), ''))


g1<-ggplot(nutl_agg, aes(fbname_long, mu, fill=form)) + 
    geom_bar(stat='identity', alpha=0.7, position = position_dodge(width=0.9, preserve = "single")) +
    # geom_jitter(data=dat2, alpha=0.8, pch=21, col='black') +
    facet_grid(~lab, scales='free', labeller=label_parsed) +
    labs(x = '', y = 'concentration per 100 g') +
    scale_fill_manual(values = pcols) +
    scale_y_continuous(labels=scales::comma, expand=c(0.01,0.01)) +
    # scale_colour_manual(values = pcols) +
    theme(#axis.ticks.x = element_blank(),
          #axis.text.x = element_blank(),
          #axis.line.x = element_blank(),
          strip.text.x = element_text(size = 12),
          legend.title=element_blank(),
          plot.caption = element_text(size=12, colour='#636363', face=3),
          plot.title = element_text(size=14, colour='black', face=2),
          legend.text = element_text(size = 11)) +
    coord_flip()

pdf(file = 'figures/GLOW10-naftaly/mean_nutrient_concentration_kenya.pdf', height = 4, width=16)
print(g1)
dev.off()


g2<-ggplot(nutl_agg, aes(fbname_long, rni*100, fill=form)) + 
    geom_bar(stat='identity', alpha=0.7, position = position_dodge(width=0.9, preserve = "single")) +
    # geom_jitter(data=dat2, alpha=0.8, pch=21, col='black') +
    facet_grid(~lab, scales='fixed', labeller=label_parsed) +
    labs(x = '', y = 'contribution of 40g portion to daily nutrient requirement, %') +
    scale_fill_manual(values = pcols) +
    scale_y_continuous(labels=scales::label_percent(scale=1), expand=c(0.01,0.01), breaks=seq(25, 100, by =25)) +
    # scale_colour_manual(values = pcols) +
    theme(#axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.line.x = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.title=element_blank(),
        plot.caption = element_text(size=12, colour='#636363', face=3),
        plot.title = element_text(size=14, colour='black', face=2),
        legend.text = element_text(size = 11)) +
    coord_flip()

pdf(file = 'figures/GLOW10-naftaly/mean_nutrient_intake__children_under5_40g_kenya.pdf', height = 4, width=22)
print(g2)
dev.off()


g3<-ggplot(nutl_agg, aes(fbname_long, rni2*100, fill=form)) + 
    geom_bar(stat='identity', alpha=0.7, position = position_dodge(width=0.9, preserve = "single")) +
    # geom_jitter(data=dat2, alpha=0.8, pch=21, col='black') +
    facet_grid(~lab, scales='fixed', labeller=label_parsed) +
    labs(x = '', y = 'contribution of 100g portion to daily nutrient requirement, %') +
    scale_fill_manual(values = pcols) +
    scale_y_continuous(labels=scales::label_percent(scale=1), expand=c(0.01,0.01), breaks=seq(25, 100, by =25)) +
    # scale_colour_manual(values = pcols) +
    theme(#axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.line.x = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.title=element_blank(),
        plot.caption = element_text(size=12, colour='#636363', face=3),
        plot.title = element_text(size=14, colour='black', face=2),
        legend.text = element_text(size = 11)) +
    coord_flip()

pdf(file = 'figures/GLOW10-naftaly/mean_nutrient_intake_pregnant_women_100g_kenya.pdf', height = 4, width=22)
print(g3)
dev.off()



nutSelect<-c('Iron', 'Zinc', 'Vitamin A', 'Folate')
g2<-ggplot(nutl_agg %>% filter(nutrient %in% nutSelect), aes(fbname_long, rni*100, fill=form)) + 
    geom_bar(stat='identity', alpha=0.7, position = position_dodge(width=0.9, preserve = "single")) +
    # geom_jitter(data=dat2, alpha=0.8, pch=21, col='black') +
    facet_grid(~lab, scales='fixed', labeller=label_parsed) +
    labs(x = '', y = 'contribution of 40g portion to daily nutrient requirement, %') +
    scale_fill_manual(values = pcols) +
    scale_y_continuous(labels=scales::label_percent(scale=1), expand=c(0.01,0.01), breaks=seq(25, 100, by =25)) +
    # scale_colour_manual(values = pcols) +
    theme(#axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.line.x = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.title=element_blank(),
        plot.caption = element_text(size=12, colour='#636363', face=3),
        plot.title = element_text(size=14, colour='black', face=2),
        legend.text = element_text(size = 11)) +
    coord_flip()

pdf(file = 'figures/GLOW10-naftaly/mean_nutrient_intake__children_under5_40g_kenya_4nutrients.pdf', height = 4, width=14)
print(g2)
dev.off()


g3<-ggplot(nutl_agg %>% filter(nutrient %in% nutSelect), aes(fbname_long, rni2*100, fill=form)) + 
    geom_bar(stat='identity', alpha=0.7, position = position_dodge(width=0.9, preserve = "single")) +
    # geom_jitter(data=dat2, alpha=0.8, pch=21, col='black') +
    facet_grid(~lab, scales='fixed', labeller=label_parsed) +
    labs(x = '', y = 'contribution of 100g portion to daily nutrient requirement, %') +
    scale_fill_manual(values = pcols) +
    scale_y_continuous(labels=scales::label_percent(scale=1), expand=c(0.01,0.01), breaks=seq(25, 100, by =25)) +
    # scale_colour_manual(values = pcols) +
    theme(#axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.line.x = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.title=element_blank(),
        plot.caption = element_text(size=12, colour='#636363', face=3),
        plot.title = element_text(size=14, colour='black', face=2),
        legend.text = element_text(size = 11)) +
    coord_flip()

pdf(file = 'figures/GLOW10-naftaly/mean_nutrient_intake_pregnant_women_100g_kenya_4nutrients.pdf', height = 4, width=14)
print(g3)
dev.off()

