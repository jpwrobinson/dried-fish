fig_water<-function(dat,){
    
    basesize = 9
    
    scales<-list(
        scale_y_continuous(labels = scales::label_percent(), limits=c(0,1)), 
        scale_fill_manual(values = pcols_named),
        scale_colour_manual(values = pcols_named))
    
    load(file = 'data/mod/lsms_mod.rds')
    dried<-m2
    load(file = 'data/mod/lsms_mod_fresh.rds')
    fresh<-m3
    ylab = 'Probability'
    
    # proximity to water (marine)
    da1<-mod_post(dried, mod_dat, 'Sproximity_to_marine_km', 'distance_to_marine') %>% 
        mutate(mod = 'Dried', nearest_water = 'Marine') %>% select(-1)
    # proximity to water (inland)
    da2<-mod_post(dried, mod_dat, 'Sproximity_to_inland_km', 'distance_to_inland') %>% 
        mutate(mod = 'Dried', nearest_water = 'Inland') %>% select(-1)
    
    # proximity to water (marine)
    fa1<-mod_post(fresh, mod_dat, 'Sproximity_to_marine_km', 'distance_to_marine') %>% 
        mutate(mod = 'Fresh', nearest_water = 'Marine') %>% select(-1)
    # proximity to water (inland)
    fa2<-mod_post(fresh, mod_dat, 'Sproximity_to_inland_km', 'distance_to_inland') %>% 
        mutate(mod = 'Fresh', nearest_water = 'Inland') %>% select(-1)
    
    mar<-rbind(fa1, da1)
    inl<-rbind(fa2, da2)

    gmar<-ggplot(mar, aes(x = raw)) +
        geom_lineribbon(aes(y = estimate__, ymin = lower50, ymax = upper50,fill=mod), alpha = 0.5) +
        geom_lineribbon(aes(y = estimate__, ymin = lower95, ymax = upper95,fill=mod), alpha = 0.1) +
        scales +
        theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.75), legend.title = element_blank(),
              plot.margin = unit(c(.05, .01, .05, .05), 'cm'),
              axis.text = element_text(size = basesize), 
              axis.title = element_text(size = basesize)) +
        labs(x = 'Distance to marine, km', y = ylab)  
    
    ginl<-ggplot(inl, aes(x = raw)) +
        geom_lineribbon(aes(y = estimate__, ymin = lower50, ymax = upper50,fill=mod), alpha = 0.5) +
        geom_lineribbon(aes(y = estimate__, ymin = lower95, ymax = upper95,fill=mod), alpha = 0.1) +
        scales +
        theme(legend.position = 'none', 
              plot.margin = unit(c(.05, .01, .05, .05), 'cm'),
              axis.text = element_text(size = basesize), 
              axis.title = element_text(size = basesize)) +
        labs(x = 'Distance to inland, km', y = ylab)  
    
    
    # Create the inset histogram
    inset_hist <- ggplot(mod_dat, aes(distance_to_marine)) +
        geom_histogram(bins = 20, fill = "steelblue", color = "black") +
        scale_x_continuous(expand=c(0,0)) +
        theme_void() 
    
    inset_hist2 <- ggplot(mod_dat, aes(distance_to_inland)) +
        geom_histogram(bins = 20, fill = "steelblue", color = "black") +
        scale_x_continuous(expand=c(0,0)) +
        theme_void() 
    
    # Convert the inset plot into a grob
    inset_grob <- ggplotGrob(inset_hist)
    inset_grob2 <- ggplotGrob(inset_hist2)
    
    # Add the inset histogram to the main plot
    gmar<-gmar +
        annotation_custom(
            grob = inset_grob,
            xmin = 0, xmax = max(mod_dat$distance_to_marine),  # Adjust the x-axis placement of the inset
            ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
        )
    
    ginl<-ginl +
        annotation_custom(
            grob = inset_grob2,
            xmin = 0, xmax = max(mod_dat$distance_to_inland),  # Adjust the x-axis placement of the inset
            ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
        )
    
    plot_grid(gmar, ginl, nrow=1, labels=c('a', 'b'))
}