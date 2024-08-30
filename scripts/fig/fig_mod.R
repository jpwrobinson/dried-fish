
fig_mod<-function(dat, model = 'dried'){
    
    if(model == 'dried'){
        load(file = 'data/mod/lsms_mod.rds')
        ylab = 'P(dried fish consumption)'
    } else {
        load(file = 'data/mod/lsms_mod_fresh.rds')
        m2<-m3
        ylab = 'P(fresh fish consumption)'
    }
    
    scales<-list(
        scale_y_continuous(labels = scales::label_percent()), 
        scale_fill_manual(values = realm_cols_named),
        scale_colour_manual(values = realm_cols_named))
    
    # proximity to water (marine)
    da1<-mod_post(m2, dat, 'Sproximity_to_marine_km', 'distance_to_marine') %>% 
        mutate(nearest_water = 'Marine') %>% select(-1)
    # proximity to water (inland)
    da2<-mod_post(m2, dat, 'Sproximity_to_inland_km', 'distance_to_inland') %>% 
        mutate(nearest_water = 'Inland') %>% select(-1)
    
    # proximity to city
    db<-mod_post(m2, dat, 'Sproximity_to_city_mins', 'proximity_to_city_mins') 
    
    # household wealth
    dc<-mod_post(m2, dat, 'Swealth', 'Swealth') 
    
    # household size
    dd<-mod_post(m2, dat, 'Sn_hh', 'n_hh') 
    
    # water panel
    ga<-ggplot(rbind(da1, da2) %>% mutate(raw = raw/1000) , aes(x = raw)) +
        geom_lineribbon(aes(y = estimate__, ymin = lower50, ymax = upper50,fill=nearest_water), alpha = 0.5) +
        geom_lineribbon(aes(y = estimate__, ymin = lower95, ymax = upper95,fill=nearest_water), alpha = 0.3) +
        scales +
        theme(legend.position = 'inside', legend.position.inside = c(0.9, 0.9), legend.title = element_blank()) +
        labs(x = 'Proximity to water, km', y = ylab)
    
    
    # proximity to urban centre
    gb<-ggplot(db , aes(x = raw)) +
        geom_lineribbon(aes(y = estimate__,ymin = lower50, ymax = upper50), alpha = 0.5) +
        geom_lineribbon(aes(y = estimate__,ymin = lower95, ymax = upper95), alpha = 0.3) +
        scales +
        theme(legend.position = 'none') +
        labs(x = 'Proximity to urban centre, mins', y = ylab)
    
    gc<-gb %+% dc +
        labs(x = 'Household wealth', y = ylab)
    
    gd<-gc %+% dd +
        labs(x = 'Household size, people', y = ylab)
        
    
    lhs<-plot_grid(ga, gb, gc, gd, nrow=1)
    return(lhs)
    
}
