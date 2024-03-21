fig_fresh_contrast<-function(dat){
    nut<-dat
        filter(nutrient %in% c(nuts, cons)) %>% 
        mutate(id = paste(nutrient, latin_name),
               form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh'),
               nutrient = str_to_title(nutrient),
                nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                                  'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Epa_dha', 'Cadmium', 'Lead', 'Mercury')),
               nutrient = recode(nutrient,  Epa_dha = 'Omega-3 (DHA + EPA)', 
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
        select(sample_id, latin_name, nutrient, unit, fresh_value_dw, form, value) %>%
        mutate(contrast = (value - fresh_value_dw) / fresh_value_dw)

    nut_mean<-nut %>% group_by(nutrient, form) %>% 
        summarise(se = funk::se(contrast), contrast=mean(contrast)) %>% 
        mutate(lwr = contrast - 2*se, upr = contrast + 2*se)

    nut_mean2<-nut %>% group_by(nutrient) %>% 
        summarise(se = funk::se(contrast), contrast=mean(contrast)) %>% 
        mutate(lwr = contrast - 2*se, upr = contrast + 2*se)

    g1A<-ggplot(nut_mean2, aes(contrast, fct_reorder(nutrient, contrast))) +
        geom_rect(xmin = -Inf, xmax = Inf, ymin = 10.5, ymax = 11.5, fill = 'grey90', alpha=0.5) +
        geom_rect(xmin = -Inf, xmax = Inf, ymin = 12.5, ymax = Inf, fill = 'grey90', alpha=0.5) +
        geom_rect(xmin = -Inf, xmax = Inf, ymin = 3.5, ymax = 4.5, fill = 'grey90', alpha=0.5) +
        geom_vline(xintercept = 0, linetype=5, col='grey80')+
        geom_jitter(data = nut, aes( col = form), height=0.1, size=1, alpha=0.5) +
        geom_pointrange(aes(xmin = lwr, xmax = upr)) +
        # geom_pointrange(aes(xmin = lwr, xmax = upr, fill=form), size=0.5, pch=21, col='black') +
        scale_colour_manual(values=pcols_named[-1]) +
        scale_fill_manual(values=pcols_named[-1]) +
        labs(x = 'Change in nutrient / contaminant content after processing', y = '') +
        scale_x_continuous(limits=c(-1, 2), breaks=seq(-1, 2, by = 0.5), labels=scales::percent) +
        theme(legend.title = element_blank(), legend.position = c(0.8, 0.4))
    print(g1A)
}

