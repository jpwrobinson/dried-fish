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
    levels(nutl$lab)<-c('Cadmium\nmonthly limit','Lead\ndaily limit', 'Mercury\nweekly limit')
    
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
        filter(species != '') %>% 
        mutate(exposure = exposure * portion/100, ## correct portion size (portion * 100) then rescale between 0-1
                lab2 = paste(species, form))
    # sp<-unique(datter$species)
    # sp<-sp[!sp == '']

    th<-theme(plot.margin=unit(c(1,3,1,2), 'cm'),
              axis.text.y = element_blank(),
              legend.position = 'none') 
    
    for(i in 1:length(cons)){
        
        gg<-ggplot(datter %>% filter(nutrient==str_to_title(cons[i])), 
                   aes(fct_reorder2(lab2, nutrient, -exposure),exposure, fill = form)) + 
            geom_col(position = position_dodge(width = 0.8)) + 
            geom_text(aes(label = species), size=2, hjust=-.1, fontface=3) +
            facet_wrap(~lab, scales='free_x') +
            coord_flip(clip='off') + 
            labs(x = '', y = '% of exposure limit') +
            scale_fill_manual(values = pcols_named) +
            scale_y_continuous(expand=c(0,0)) +
            th
        
        assign(paste('gg', i, sep = '_'), gg)
        
    }
    
    # 3 samples above Lead limit (0.3 mg / kg)
    leader<-nutl %>% filter(nutrient=='Lead') %>%
        mutate(limit_mg_100g = cont$max_limit_100g[cont$nutrient=='lead']) %>%
        slice_max(mu, n = 20) %>% 
        mutate(lab2 = paste(species, form, sep='\n'),
               lab = 'Lead\nMaximum regulatory limit')
    
    gg_1 <- ggplot(leader, aes(fct_reorder(sample_id, mu), mu, fill = form)) + 
        geom_hline(yintercept = cont$max_limit_100g[cont$nutrient=='lead'], linetype=5, col='grey') +
        geom_col(position = position_dodge(width = 0.8)) + 
        geom_text(aes(label = lab2), size=2, hjust=-.1, fontface=3) +
        facet_wrap(~lab, scales='free_x') +
        coord_flip(clip='off') + 
        labs(x = '', y = 'mg per 100 g') +
        scale_fill_manual(values = pcols_named) +
        scale_y_continuous(expand=c(0,0)) +
        th
    
    pl<-list(gg_1, gg_2, gg_3)
    gSX<-plot_grid(plotlist=pl, nrow=1, labels=c('A', 'B', 'C'), label_fontface = "plain")
    print(gSX)
}