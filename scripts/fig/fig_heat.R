fig_heat<-function(dat){
    
    basesize = 9
    
    load(file = 'data/mod/lsms_mod.rds')
    load(file = 'data/mod/lsms_mod_fresh.rds')
    
    # resolution of predictions along proximity range
    res = 100
    
    dat$distance_to_marine<-dat$distance_to_marine/1000
    dat$distance_to_inland<-dat$distance_to_inland/1000
    
    raw_r<-expand_grid(distance_to_marine = seq_range(dat$distance_to_marine, n = res),
                       distance_to_inland = seq_range(dat$distance_to_inland, n = res)) 
    
    # proximity to water
    dri<-expand_grid(Sproximity_to_marine_km = seq_range(dat$Sproximity_to_marine_km, n = res),
                  Sproximity_to_inland_km = seq_range(dat$Sproximity_to_inland_km, n = res)) %>% 
        mutate(Sproximity_to_city_mins = 0, 
               country = 'marginal',
                  Swealth = 0,
                  Sn_hh = 0) %>% 
        add_epred_draws(m2, ndraws = 1000, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "gaussian") %>% 
        group_by(Sproximity_to_marine_km, Sproximity_to_inland_km) %>% 
        summarise(mu = median(.epred)) 
    
    dri$distance_to_marine<-raw_r$distance_to_marine
    dri$distance_to_inland<-raw_r$distance_to_inland
    
    fres<-expand_grid(Sproximity_to_marine_km = seq_range(dat$Sproximity_to_marine_km, n = res),
                     Sproximity_to_inland_km = seq_range(dat$Sproximity_to_inland_km, n = res)) %>% 
        mutate(Sproximity_to_city_mins = 0, 
               country = 'marginal',
               Swealth = 0,
               Sn_hh = 0) %>% 
        add_epred_draws(m3, ndraws = 1000, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "gaussian") %>% 
        group_by(Sproximity_to_marine_km, Sproximity_to_inland_km) %>% 
        summarise(mu = median(.epred))
    
    fres$distance_to_marine<-raw_r$distance_to_marine
    fres$distance_to_inland<-raw_r$distance_to_inland
    
    g1<-ggplot(dri, aes(distance_to_marine, distance_to_inland)) + 
        geom_tile(aes(fill=mu)) +
        scale_y_continuous(expand=c(0,0)) +
        scale_x_continuous(expand=c(0,0)) +
        scale_fill_gradientn(labels=scales::percent, 
                             # midpoint = median(dri$mu),
                             colors = rev(hcl.colors(20, "RdYlBu")),
                             limits=c(0, max(dri$mu))) +
        geom_point(data = dat, alpha=0.1, size=.005, col='black') +
        theme(legend.position = 'inside', legend.position.inside = c(0.98, 0.7),
              legend.text=element_text(color='white'),
              text = element_text(size = basesize)) +
        labs(fill = '',x = 'Distance to marine, km', y = 'Distance to inland, km') 
    
    # ggMarginal(g1, type='histogram')
    
    
    g2<-g1 %+% fres +
        # annotate('text', x = 1000, y = 330, label = 'Fresh fish', colour='white') +
        guides(fill = 'none') 
    
    lhs<-plot_grid(g1 ,#+ annotate('text', x = 1000, y = 330, label = 'Dried fish', colour='white'), 
                   g2, nrow=2)#, labels=c('a', 'b'))
    # top<-plot_grid(gc, gd, nrow=1, labels=c('a', 'b'))
    # pp<-plot_grid(top, lhs, nrow=2, rel_heights=c(0.4, 1))
    
    
    return(lhs)
    
    }