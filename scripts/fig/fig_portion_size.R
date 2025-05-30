
figPortion<-function(dat, pop){
    ## tidy names
    nutl<-dat %>% 
        filter(nutrient %in% nuts) %>%
        mutate(nutrient = str_to_title(nutrient)) %>% 
        rename(species = latin_name, fbname = local_name, mu = value) %>% 
        mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                                  'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Epa_dha'))) %>%
        mutate(nutrient = recode(nutrient,  Epa_dha = 'Omega-3', 
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

    ## arrange data, add a portion seq
    portions<-seq(1, 100, by = .1)
    porter<-numeric()
    for(i in 1:length(portions)){
        datter<-nutl_agg %>% ungroup() %>% 
            mutate(form2 = ifelse(form=='Fresh', 'Fresh', 'Dried'),
                rni = case_when(str_detect(pop, 'Children') ~ rni_kids, 
                                   str_detect(pop, 'Adult women')~rni_women,
                                   str_detect(pop, 'Adult men')~rni_men,
                                   str_detect(pop, 'Pregnant')~rni_pregnant)) %>% 
            mutate(rni = rni/100 * portions[i]/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
            ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
            mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni)) %>% 
            group_by(lab, nutrient, form2) %>% 
            summarise(rni = mean(rni))
        
        porter<-rbind(porter, datter %>% mutate(portion = portions[i]) %>% as.data.frame())
    }

    # where is source achieved along portion size
    intersect<-porter %>% 
        group_by(nutrient, form2) %>% 
        reframe(portion=min(portion[which(rni>=.15)])) %>% 
        mutate(rni = .15)
    
    # fresh vs dried, facet by nutrient for sup fig
    gg_port<-ggplot(porter, aes(portion, rni, col=form2)) + geom_line() + 
        facet_grid(~nutrient) +
        labs(x = 'Portion size, g', y = 'Nutrient Reference Value', col='') +
        geom_hline(yintercept = 0.15, col='grey', linetype=5) +
        geom_text(data = data.frame(nutrient = 'Calcium', rni = 0.2, portion = 20, lab='Source'), 
                  aes(label = lab), col='grey40', size=3) +
        geom_point(data = intersect, aes(portion, rni, fill=form2), size=2, pch=21, col='black') + 
        scale_y_continuous(limits=c(0, 1), labels=scales::percent) +
        scale_colour_manual(values = pcols_named) +
        scale_fill_manual(values = pcols_named) 
    
    if(pop == 'Children') {
        gg_port<-gg_port + 
            scale_x_continuous(limits =c(1, 30), expand=c(0,0), breaks=c(1, 5, 15, 25)) +
            geom_vline(xintercept = 9, col='#CF0000', linetype=5, alpha=0.5) }
    
    if(pop == 'Adult women') {
        gg_port<-gg_port + 
            scale_x_continuous(limits =c(1, 50), expand=c(0,0), breaks=c(1, 15, 30, 45)) +
            geom_vline(xintercept = 41, col='#CF0000', linetype=5, alpha=0.5) }
            
    # dried RNI only, for main fig 1b
    labber<-porter %>% filter(form2 != 'Fresh') %>% 
        group_by(nutrient) %>% 
        reframe(rni = rni[which(portion==25)])
    
    intersect2<-porter %>% filter(form2 != 'Fresh') %>% 
        group_by(nutrient) %>% 
        reframe(portion=min(portion[which(rni>=.15)])) %>% 
        mutate(rni = .15)
    
    gg_portMain<-ggplot(porter %>% filter(form2 != 'Fresh' & portion <=26), aes(portion, rni, col=nutrient)) +
        geom_line() + 
        labs(x = 'Dried fish portion, g', y = 'Nutrient Reference Value', col='') +
        geom_hline(yintercept = 0.15, col='grey', linetype=5) +
        geom_vline(xintercept = 9, col='#CF0000', linetype=5, alpha=0.5) +
        geom_text(data = labber, aes(26.5, y = rni, label = nutrient), vjust=0, hjust=0, size=2.5) +
        geom_text(data = data.frame(rni = 0.18, portion = 0, lab='Source of nutrient'), 
                  aes(label = lab), col='grey40', size=2) +
        geom_point(data = intersect2, aes(portion, rni, fill=nutrient), size=3, pch=21, col='black') + 
        scale_y_continuous(labels=scales::percent) +
        scale_x_continuous(limits =c(1, 30), expand=c(0,0), breaks=c(1, 5, 10, 15, 20, 25)) +
        scale_colour_manual(values = nut_cols) +
        scale_fill_manual(values = nut_cols) +
        coord_cartesian(clip='off') +
        theme(legend.position = 'none')

    ## now portion size at which sun-dried and smoked are sources of nutrients
    source<-porter %>% 
        group_by(nutrient, form2) %>% 
        reframe(portion=portion[which(rni>=0.15)]) %>% 
        group_by(nutrient, form2) %>% 
        summarise(portion=min(portion)) 

    gg_source<-ggplot(source, aes(portion, fct_reorder(nutrient,portion), portion, col=form2)) + 
        geom_point(size=2.5) +
        labs(x = 'portion size for 15% NRV, g', y = '', col='') +
        # geom_vline(xintercept = 12.75, linetype=5, col='grey') +
        geom_vline(xintercept = 6, linetype=5, col='grey') +
        # annotate('text', x = 24, y = 10, size=3, label = 'Daily intake for 6-23 months (Kimere et al.)', col='grey30') +
        scale_colour_manual(values = pcols_named) +
        theme(legend.position = c(0.8, 0.2)) + 
        scale_x_continuous(expand=c(0.01,0.01), breaks=c(1,5,10,seq(20,80,by=20)))
        


    gg_source_hist<-ggplot(source, aes(portion)) + 
        geom_histogram() + 
        labs(x = 'portion size for 15% NRV, g', y = 'Number of source foods') +
        # geom_vline(xintercept = 12.75, linetype=5, col='grey') +
        geom_vline(xintercept = 6, linetype=5, col='grey') +
        scale_x_continuous(expand=c(0.01,0.01), breaks=c(1,5,10,seq(20,80,by=20))) +
        scale_y_continuous(expand=c(0,0))

    list(
        plot_grid(gg_source, gg_source_hist, nrow =1, labels = c('a', 'b')),
        gg_port,
        gg_portMain,
        porter
        )

}