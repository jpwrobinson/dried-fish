figRNI<-function(dat, portion){
    ## tidy names
    nutl<-dat %>% 
        filter(nutrient %in% nuts) %>%
        mutate(nutrient = str_to_title(nutrient)) %>% 
        rename(species = latin_name, fbname = local_name, mu = value) %>% 
        mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                                  #'Omega3', 
                                                  'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Epa_dha'))) %>%
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
                        expression('Vitamin D, '*mu*'g'), "'Omega-3 (DHA+EPA), g'")

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
    datter<-nutl_agg %>% 
        mutate(rni = case_when(str_detect(pop, 'Children') ~ rni_kids, 
                               str_detect(pop, 'Adult women')~rni_women,
                               str_detect(pop, 'Adult men')~rni_men,
                               str_detect(pop, 'Pregnant')~rni_pregnant)) %>% 
        mutate(rni=rni/100 * portion/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
        ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
        mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni))

    sp<-rev(sort(unique(datter$species)))
    sp<-sp[!sp == '']

    th<-theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=0),
              legend.position = 'none') 

    for(i in 1:length(sp)){
        
        plotter<-datter[,c('form', 'nutrient', 'rni', 'species')] %>% 
            filter(species == sp[i]) %>% select(-species) %>% 
            filter(form != 'Fresh') %>% # drop fresh
            mutate(rni = ifelse(is.na(rni), 0, rni)) %>% 
            pivot_wider(names_from = nutrient, values_from = rni) 
        # select_if(~ !any(is.na(.)))
        
        
        if(i != 1){
            ## All panels without top-left guide
            names(plotter)<-c('form','Ca', 'Fe', 'Se', 'Zn', 'I', 'v-A', 'v-B12','v-D', 'O-3')
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
                th + labs(subtitle = sp[i]) +coord_equal(clip='off') +
                scale_color_manual(values=pcols_named) + scale_fill_manual(values=pcols_named) 
            
        } else {
            ## Top-left guide
            gg<-ggradar(plotter, 
                        group.colours = pcols,
                        base.size = 1,
                        group.point.size = 2,
                        grid.label.size  = 3,
                        group.line.width = 1,
                        background.circle.colour = "white",
                        axis.label.size = 3,
                        fill=TRUE,
                        gridline.mid.colour = "grey") +
                th + labs(subtitle = sp[i]) +coord_equal(clip='off') +
                scale_color_manual(values=pcols_named) + scale_fill_manual(values=pcols_named) 
        }
        
        assign(paste('gg', i, sep = '_'), gg)
    }

    gg_leg<-ggradar(datter %>% filter(form %in% unique(datter$form)) %>% distinct(form, nutrient) %>% 
                        mutate(rni = 0) %>% 
                        pivot_wider(names_from = nutrient, values_from = rni), 
                    fill=TRUE) + guides(color='none') + scale_fill_manual(values=pcols_named[-3]) 

    pl<-list(gg_1, gg_2, gg_3, gg_4, gg_5, gg_6, gg_7, gg_8, gg_9, gg_10, gg_11, get_legend(gg_leg))
    gSX<-plot_grid(plotlist=pl)
    print(gSX)
}