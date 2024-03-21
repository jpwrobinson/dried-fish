
fig_dried_rni<-function(dat){
    ## tidy names
    nutl<-dat %>% 
        filter(nutrient %in% nuts) %>%
        mutate(nutrient = str_to_title(nutrient)) %>% 
        rename(species = latin_name, fbname = local_name, mu = value) %>% 
        mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
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
               rni_pregnant = mu/rni_pregnant*100
               )

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


    th<-theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=0),
              legend.position = 'none') 

    ## main fig with smoked + sun-dried
    forms<-c('Smoked', 'Sun-dried')
        
    plotter<-dat[,c('form', 'nutrient', 'rni')] %>% 
            filter(form %in% forms) %>% #select(-form) %>% 
            mutate(rni = ifelse(is.na(rni), 0, rni)) %>% 
            group_by(form) %>% 
            pivot_wider(names_from = nutrient, values_from = rni) 
        
    coll<-as.character(pcols_named[forms])
        
    # names(plotter)<-c('form', 'Ca', 'Fe', 'Se', 'Zn', 'I', 'v-A', 'v-B12','v-D', 'v-B9', 'O-3')
    # names(plotter)[10]<-'Vitamin B9'

    g1C<-ggradar(plotter, 
                    group.colours = pcols,
                    base.size = 1,
                    # values.radar = c('15%', "50%", "100%"),
                    # grid.min = .0, grid.mid = .15, grid.max = 1,
                    grid.label.size = 3,
                    group.point.size = 2,
                    group.line.width = 1,
                    background.circle.colour = "white",
                    axis.label.size = 3,
                    fill=TRUE,
                    gridline.mid.colour = "grey") +
            th + coord_equal(clip='off') +
            theme( legend.position = 'bottom', legend.text = element_text(size=9))+# legend.key.width = unit(1, 'cm')) +
            guides(fill='none') +
            scale_color_manual(values=coll) + 
            scale_fill_manual(values=coll) +
            annotate("path", col='darkred', linetype=5,
                 x=c(0,0)+.26*cos(seq(0,2*pi,length.out=100)),
                 y=c(0,0)+.26*sin(seq(0,2*pi,length.out=100))) +
            annotate('text', x = -.85, y = 0.1, label = 'Source of nutrient --------', col='darkred', size=3)


    ## sup fig with all forms
    forms<-unique(dat$form)
    for(i in 1:length(forms)){
        
        plotter<-dat[,c('form', 'nutrient', 'rni')] %>% 
            filter(form == forms[i]) %>% #select(-form) %>% 
            mutate(rni = ifelse(is.na(rni), 0, rni)) %>% 
            group_by(form) %>% 
            pivot_wider(names_from = nutrient, values_from = rni) 
        
        coll<-as.character(pcols_named[forms[i]])

        # if(i != 1){
            ## All panels without top-left guide
            names(plotter)<-c('form', 'Ca', 'Fe', 'Se', 'Zn', 'I', 'v-A', 'v-B12','v-D', 'O-3')
            gg<-ggradar(plotter, 
                        group.colours = pcols,
                        base.size = 1,
                        values.radar = c('', "50%", "100%"),
                        # grid.min = 0, grid.mid = .15, grid.max = 1,
                        grid.label.size = 3,
                        group.point.size = 2,
                        group.line.width = 1,
                        background.circle.colour = "white",
                        axis.label.size = 3,
                        fill=TRUE,
                        gridline.mid.colour = "grey") +
                th + labs(subtitle = forms[i]) +coord_equal(clip='off') +
                scale_color_manual(values=coll) + scale_fill_manual(values=coll) +
                # add source food = 15% line
                annotate("path", col='darkred', linetype=5,
                         x=c(0,0)+.26*cos(seq(0,2*pi,length.out=100)),
                         y=c(0,0)+.26*sin(seq(0,2*pi,length.out=100))) +
                annotate('text', x = -.9, y = 0.1, label = 'Source of nutrient --------', col='darkred', size=3)
            
        assign(paste('gg', i, sep = '_'), gg)
    }

    gS1B<-plot_grid(gg_1, gg_2, gg_3, gg_4, gg_5, nrow=1)
    print(gS1B)

}