## plotting args
source('scripts/00_plot.R')

## food settings
portion = 12.75
pop = 'Children'


## load data
nut<-read.csv('data/clean/dried_nutrient_estimates_long.csv') 
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
## tidy names
nutl<-nut %>% 
    filter(nutrient %in% nuts) %>%
    mutate(nutrient = str_to_title(nutrient)) %>% 
    rename(species = latin_name, fbname = local_name, mu = value) %>% 
    mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                              'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Folate', 'Epa_dha'))) %>%
    mutate(nutrient = recode(nutrient,  Epa_dha = 'Omega-3 (DHA + EPA)', 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) %>% 
    mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
    mutate(fbname = ifelse(species == 'Encrasicholina punctifer', 'Omena (marine)', fbname),
           fbname = ifelse(species == 'Rastrineobola argenteus', 'Omena (freshwater)', fbname))

## units in labels
nutl$lab<-nutl$nutrient
levels(nutl$lab)<-c("'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'),
                    "'Zinc, mg'",expression('Iodine, '*mu*'g'),# "'Omega-3, g'", 
                    expression('Vitamin A, '*mu*'g'),expression('Vitamin B12, '*mu*'g'),
                    expression('Vitamin D, '*mu*'g'),expression('Folate, '*mu*'g'), "'Omega-3 (DHA+EPA), g'")

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
    mutate(rni = rni/100 * portion/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
    ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
    mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni)) %>% 
    group_by(lab, nutrient, form) %>% 
    summarise(rni = mean(rni))

forms<-unique(dat$form)

th<-theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=0),
          legend.position = 'none') 

for(i in 1:length(forms)){
    
    plotter<-dat[,c('form', 'nutrient', 'rni')] %>% 
        filter(form == forms[i]) %>% #select(-form) %>% 
        mutate(rni = ifelse(is.na(rni), 0, rni)) %>% 
        group_by(form) %>% 
        pivot_wider(names_from = nutrient, values_from = rni) 
    
    coll<-as.character(pcols_named[forms[i]])

    if(i != 1){
        ## All panels without top-left guide
        names(plotter)<-c('form', 'Ca', 'Fe', 'Se', 'Zn', 'I', 'v-A', 'v-B12','v-D', 'v-B9', 'O-3')
        gg<-ggradar(plotter, 
                    group.colours = pcols,
                    base.size = 1,
                    # values.radar = '',
                    grid.label.size = 3,
                    group.point.size = 2,
                    group.line.width = 1,
                    background.circle.colour = "white",
                    axis.label.size = 3,
                    fill=TRUE,
                    gridline.mid.colour = "grey") +
            th + labs(subtitle = forms[i]) +coord_equal(clip='off') +
            scale_color_manual(values=coll) + scale_fill_manual(values=coll) 
        
    } else {
        ## Top-left guide
        names(plotter)[10]<-'Vitamin B9'
        gg<-ggradar(plotter, 
                    # group.colours = pcols,
                    base.size = 1,
                    group.point.size = 2,
                    grid.label.size  = 3,
                    group.line.width = 1,
                    background.circle.colour = "white",
                    axis.label.size = 3,
                    fill=TRUE,
                    gridline.mid.colour = "grey") +
            th + labs(subtitle = forms[i]) +coord_equal(clip='off') +
            scale_color_manual(values=coll) + scale_fill_manual(values=coll) 
    }
    
    assign(paste('gg', i, sep = '_'), gg)
}

bot<-plot_grid(gg_4, 
               gg_2 + theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=1)), 
               nrow=1)
mid<-plot_grid(gg_5, 
               gg_3 + theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=1)), 
               nrow=1)
g1B<-plot_grid(gg_1, mid, bot, nrow=3)
