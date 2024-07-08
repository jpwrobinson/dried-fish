
fig_ndensity<-function(dat, portion){
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
        group_by(species, fbname, nutrient, lab, location) %>% 
        mutate(n = length(mu)) %>% 
        group_by(form, species, fbname, n, nutrient, lab, location) %>% 
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
        mutate(rni = rni/100 * portion/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
        ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
        mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni)) %>% 
        group_by(lab, species, nutrient, form, location) %>% 
        summarise(rni = mean(rni)) %>% 
        pivot_wider(id_cols = -lab,  names_from = nutrient, values_from = rni) %>% 
        filter(form != 'Fresh') %>% 
        mutate(density = rowSums(across(where(is.numeric)), na.rm=TRUE)*100,
               id = paste(species, form, sep='\n'),
               env = ifelse(location %in% c('Mombasa', "Accra"), 'Marine', 'Freshwater'))

    # form aggs
    datter2<-nutl_agg %>% 
        mutate(rni = case_when(str_detect(pop, 'Children') ~ rni_kids, 
                               str_detect(pop, 'Adult women')~rni_women,
                               str_detect(pop, 'Adult men')~rni_men,
                               str_detect(pop, 'Pregnant')~rni_pregnant)) %>% 
        mutate(rni = rni/100 * portion/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
        ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
        mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni),
               env = ifelse(location %in% c('Mombasa', "Accra"), 'Marine', 'Freshwater')) %>% 
        group_by(lab, nutrient, form, env) %>% 
        summarise(rni = mean(rni)) %>% 
        pivot_wider(id_cols = -lab,  names_from = nutrient, values_from = rni) %>% 
        mutate(density = rowSums(across(where(is.numeric)), na.rm=TRUE)*100,
               id = paste(env, form, sep='\n'))

    g1B<-ggplot(datter2, aes(fct_reorder(id, density), density/100, fill=form)) + 
            geom_bar(stat='identity', position = 'dodge', alpha=0.7) + 
            geom_text(aes(label = paste0(round(density, 0), '%')), nudge_y = .05, size = 2, hjust=0) +
            scale_fill_manual(values=pcols_named)  +
            scale_y_continuous(expand=c(0,0), labels=scales::percent) +
            theme(legend.position = 'none', 
                  axis.text.y = element_text(size=7, vjust=0.5),
                  axis.line.y = element_blank()) +
            labs(x = '', y = 'Nutrient density in 9 g portion') +
            coord_flip(clip='off') 

        print(g1B)
}
