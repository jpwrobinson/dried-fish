## plotting args
source('scripts/00_plot.R')

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Omega3', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

## load data
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
nut<-read.csv('data/clean/dried_nutrient_estimates_long.csv') %>% 
    filter(nutrient %in% nuts) %>% 
    mutate(id = paste(nutrient, latin_name),
           type = recode(type, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh'),
           nutrient = str_to_title(nutrient),
            nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                              'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Folate')),
           nutrient = recode(nutrient, 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) 

fresh<-nut %>% filter(type == 'Fresh')
nut$fresh<-fresh$value[match(nut$id, fresh$id)]

nut<-nut %>% filter(!is.na(fresh) & type != 'Fresh') %>% 
    select(sample_id, latin_name, nutrient, unit, fresh, type, value) %>%
    mutate(contrast = (value - fresh) / fresh * 100)

nut_mean<-nut %>% group_by(nutrient) %>% 
    summarise(se = funk::se(contrast), contrast=mean(contrast)) %>% 
    mutate(lwr = contrast - 2*se, upr = contrast + 2*se)

g1A<-ggplot(nut_mean, aes(contrast, fct_reorder(nutrient, contrast))) +
    geom_jitter(data = nut, aes( col = type), height=0.1, size=1, alpha=0.5) +
    geom_pointrange(aes(xmin = lwr, xmax = upr)) +
    scale_colour_manual(values=pcols_named[-1]) +
    labs(x = 'Change in nutrient content after processing', y = '') +
    scale_x_continuous(limits=c(0, 1100), breaks = seq(0, 2500, 250), labels=paste0(seq(0, 2500, 250), '%')) +
    theme(legend.title = element_blank(), legend.position = c(0.8, 0.4))