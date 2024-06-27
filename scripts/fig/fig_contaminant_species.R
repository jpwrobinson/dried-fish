figContaminant_Species<-function(dat, portion){

    ## tidy names
    nutl<-dat %>% 
        filter(nutrient %in% cons) %>%
        mutate(nutrient = str_to_title(nutrient)) %>% 
        rename(species = latin_name, fbname = local_name, mu = value) %>% 
        mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
        mutate(fbname = ifelse(species == 'Encrasicholina punctifer', 'Omena (marine)', fbname),
               fbname = ifelse(species == 'Rastrineobola argenteus', 'Omena (freshwater)', fbname))

    ## units in labels
    nutl$lab<-as.factor(nutl$nutrient)
    levels(nutl$lab)<-c("'Cadmium, mg'","Lead, mg", "'Mercury, mg'")

    nutl_agg<-nutl %>% 
        group_by(species, fbname, nutrient, lab) %>% 
        mutate(n = length(mu)) %>% 
        group_by(form, species, fbname, n, nutrient, lab) %>% 
        summarise(mu = median(mu)) %>% 
        ungroup() %>% droplevels() %>% 
        ## add RDA and units
        left_join(cont %>% mutate(nutrient = str_to_title(nutrient))) %>% 
        left_join(units %>% mutate(nutrient = str_to_title(nutrient))) %>% 
        mutate(exposure = mu  / limit_100g * 100)


    ## arrange data
    datter<-nutl_agg %>% 
        mutate(exposure = exposure/100 * portion/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
        mutate(exposure = case_when(exposure > 1 ~ 1, TRUE ~ exposure)) ## cap limits for plot - but note some forms are more than 100% limit

    sp<-unique(datter$species)
    sp<-sp[!sp == '']

    th<-theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=0),
              legend.position = 'none') 

    for(i in 1:length(sp)){
        
        plotter<-datter[,c('form', 'nutrient', 'exposure', 'species')] %>% 
            filter(species == sp[i]) %>% select(-species) %>% 
            filter(form != 'Fresh') %>% 
            mutate(exposure = ifelse(is.na(exposure), 0, exposure)) %>% 
            pivot_wider(names_from = nutrient, values_from = exposure, values_fill = 0) 

        
        if(i != 1){
            ## All panels without top-left guide
            names(plotter)<-c('form','Cd', 'Pb', 'Hg')
            gg<-ggradar(plotter, 
                        # group.colours = pcols,
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
                        # group.colours = pcols,
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
                        mutate(exposure = 0) %>% 
                        pivot_wider(names_from = nutrient, values_from = exposure), 
                    fill=TRUE) + 
            guides(color='none') + 
            scale_fill_manual(values=pcols_named[-1]) 

    pl<-list(gg_1, gg_2, gg_3, gg_4, gg_5, gg_6, gg_7, gg_8, gg_9, gg_10, gg_11,gg_12, gg_13, gg_14, gg_15, gg_16, gg_17, gg_18, get_legend(gg_leg))
    gSX<-plot_grid(plotlist=pl)
    print(gSX)
}