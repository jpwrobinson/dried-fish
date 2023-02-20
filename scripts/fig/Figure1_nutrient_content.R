library(tidyverse)
library(cowplot)
library(ggradar)

## plotting args
source('scripts/theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh', 'Sun-dried', 'Smoked','Fried', 'Powder')
pcols_named<-c('Fresh' = pcols[1], 'Sun-dried' = pcols[2], 'Smoked' = pcols[3],'Fried' = pcols[4], 'Powder' = pcols[5])

## food settings
portion = 40
pop = 'Children'

## get RDA reference vals
source('scripts/rda_reader.R')
rda$nutrient<-str_to_title(rda$nutrient)
rda$nutrient[rda$nutrient=='Vitamin_a']<-'Vitamin A'
rda$nutrient[rda$nutrient=='Vitamin_d']<-'Vitamin D'
rda$nutrient[rda$nutrient=='Vitamin_b12']<-'Vitamin B12'
# rda$nutrient[rda$nutrient=='Omega_3']<-'Omega3'

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Omega3', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

## load data
nut<-read.csv('data/clean/dried_nutrient_estimates_long.csv') 
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
## tidy names
nutl<-nut %>% 
    filter(nutrient %in% nuts) %>%
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
           rni_pregnant = mu/rni_pregnant*100)


## arrange data
dat<-nutl_agg %>% 
    mutate(rni = case_when(str_detect(pop, 'Children') ~ rni_kids, 
                           str_detect(pop, 'Adult women')~rni_women,
                           str_detect(pop, 'Adult men')~rni_men,
                           str_detect(pop, 'Pregnant')~rni_pregnant)) %>% 
    mutate(rni = rni/portion/100) %>%
    ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
    mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni))

sp<-unique(dat$species)
for(i in 1:length(sp)){
    
    plotter<-dat[,c('form', 'nutrient', 'rni', 'species')] %>% 
        filter(species == sp[i]) %>% select(-species) %>% 
        pivot_wider(names_from = nutrient, values_from = rni) %>% select_if(~ !any(is.na(.)))
    
    if(i != 1){names(plotter)<-c('form', 'I', 'Vit-A', 'Vit-B12', 'Vit-D', 'Vit-B9', )}
    gg<-ggradar(plotter, 
            group.colours = pcols,
            base.size = 1,
            group.point.size = 2,
            group.line.width = 1,
            background.circle.colour = "white",
            axis.label.size = 4,
            fill=TRUE,
            gridline.mid.colour = "grey") + 
        labs(
            # title = tit,
            subtitle = sp[i],
            # caption = cap
        ) +
        coord_equal(clip='off') +
        theme(
            plot.title = element_text(size=14, colour='black', face=2, hjust=1),
            plot.subtitle = element_text(size=11, colour='black', face=3, hjust=1),
            plot.caption = element_text(size=12, colour='#636363', face=3),
            legend.position = 'none') + 
        scale_color_manual(values=pcols_named) + ## second col/fill scales to assign specific sample forms
        scale_fill_manual(values=pcols_named) 
    
    assign(paste('gg', i, sep = '_'), gg)
}
pl<-list(gg_1, gg_2, gg_3, gg_4, gg_5, gg_6, gg_7, gg_8, gg_9, gg_10, gg_11, gg_12)
plot_grid(plotlist=pl)