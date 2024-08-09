fig_fresh_contrast<-function(dat){
    

    nut<-dat %>% 
        filter(nutrient %in% c(nuts, cons)) %>% 
        mutate(id = paste(nutrient, latin_name),
               form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh'),
               type = ifelse(nutrient %in% nuts, 'Nutrient', 'Contaminant'),
               nutrient = str_to_title(nutrient),
               nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                                  'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Epa_dha', 'Cadmium', 'Lead', 'Mercury')),
               nutrient = recode(nutrient,  Epa_dha = 'Omega-3', 
                                 Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) 

    # save fresh samples, convert fresh values to dry weight equivalents
    fresh<-nut %>% filter(form == 'Fresh') %>%
        group_by(id, latin_name, local_name, nutrient) %>% 
        summarise(value = mean(value), dry_matter_g_100g = mean(dry_matter_g_100g)) %>% 
        ungroup() %>% rowwise() %>% 
        mutate(value_dw = value * 100/dry_matter_g_100g) ## convert 100g fresh to 100g dried, based on estimated dry matter content

    nut$fresh_value_dw<-fresh$value_dw[match(nut$id, fresh$id)]

    # how many samples do we have fresh and processed?
    nut %>% filter(!is.na(fresh_value_dw)) %>% distinct(latin_name, local_name) # 9 species

    nut<-nut %>% filter(!is.na(fresh_value_dw) & form != 'Fresh') %>% 
        select(sample_id, latin_name, type, nutrient, unit, fresh_value_dw, form, value) %>%
        mutate(contrast = (value - fresh_value_dw) / fresh_value_dw) %>% 
        group_by(nutrient, type) %>% 
        mutate(meaner = mean(contrast))

    nut_mean2<-nut %>% group_by(nutrient, type) %>% 
        summarise(se = funk::se(contrast), meaner=mean(contrast)) %>% 
        mutate(lwr = meaner - 2*se, upr = meaner + 2*se)

    g1A<-ggplot(nut_mean2, aes(meaner, fct_reorder2(nutrient, type, -meaner))) +
        geom_vline(xintercept = 0, linetype=5, col='grey80')+
        geom_jitter(data = nut, aes(x = contrast, col = form), height=0.1, size=1, alpha=0.5) +
        geom_pointrange(aes(xmin = lwr, xmax = upr)) +
        scale_colour_manual(values=pcols_named) +
        facet_grid(type~., scales='free', space = 'free_y') +
        labs(x = 'Dried fish relative to fresh', y = '') +
        scale_x_continuous(limits=c(-1, 2), breaks=seq(-1, 2, by = 0.5), labels=scales::percent) +
        theme(legend.title = element_blank(), legend.position = c(0.8, 0.2), strip.text.y = element_blank(),
              axis.text = element_text(size=10))


    print(g1A)
}

