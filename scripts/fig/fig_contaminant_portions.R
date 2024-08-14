figContaminantPortion<-function(dat, portion){
    
    nutl<-dat %>% 
        filter(nutrient %in% cons) %>%
        mutate(nutrient = str_to_title(nutrient)) %>% 
        rename(species = latin_name, fbname = local_name,  mu = value) %>% 
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
        mutate(exposure = mu  / limit_100g * 100,
               nportions = limit_100g / (mu * portion/100)) # get number of portions in limit
    
    
    ## arrange data
    datter<-nutl_agg %>% 
        mutate(exposure = exposure/100 * portion/100) %>% ## correct portion size (portion * 100) then rescale between 0-1
        mutate(exposure = case_when(exposure > 1 ~ 1, TRUE ~ exposure)) %>% ## cap limits for plot - but note some forms are more than 100% limit
        group_by(form, nutrient, lab) %>% 
        summarise(exposure = mean(exposure, na.rm = TRUE),
                  nportions = mean(nportions, na.rm = TRUE)) %>% 
        # convert limits to number of daily portions
        mutate(nportions = ifelse(nutrient == 'Cadmium', nportions/30, nportions),
               nportions = ifelse(nutrient == 'Mercury', nportions/7, nportions))
    
    forms<-unique(datter$form)
    
    th<-theme(plot.subtitle = element_text(size=9, colour='black', face=3, hjust=0),
              legend.position = 'none') 
    
    gg<-ggplot(datter, aes(form, nportions, fill = form)) +
        geom_col() + 
        facet_wrap(~lab, nrow=1) +
        labs(x = '', y = 'Limit of daily portions') +
        scale_fill_manual(values = pcols_named) +
        th
    
    print(gg)
    
}