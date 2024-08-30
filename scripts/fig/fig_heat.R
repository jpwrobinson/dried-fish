fig_heat<-function(dat){
    
    load(file = 'data/mod/lsms_mod.rds')
    load(file = 'data/mod/lsms_mod_fresh.rds')
    
    # resolution of predictions along proximity range
    res = 100
    
    # proximity to water
    dri<-expand_grid(Sproximity_to_marine_km = seq_range(dat$Sproximity_to_marine_km, n = res),
                  Sproximity_to_inland_km = seq_range(dat$Sproximity_to_inland_km, n = res)) %>% 
        mutate(Sproximity_to_city_mins = 0, 
                  Swealth = 0,
                  Sn_hh = 0) %>% 
        add_epred_draws(m2, ndraws = 100, re_formula = NA) %>% 
        group_by(Sproximity_to_marine_km, Sproximity_to_inland_km) %>% 
        summarise(mu = median(.epred))
    
    fres<-expand_grid(Sproximity_to_marine_km = seq_range(dat$Sproximity_to_marine_km, n = res),
                     Sproximity_to_inland_km = seq_range(dat$Sproximity_to_inland_km, n = res)) %>% 
        mutate(Sproximity_to_city_mins = 0, 
               Swealth = 0,
               Sn_hh = 0) %>% 
        add_epred_draws(m3, ndraws = 100, re_formula = NA) %>% 
        group_by(Sproximity_to_marine_km, Sproximity_to_inland_km) %>% 
        summarise(mu = median(.epred))
    
    g1<-ggplot(dri, aes(Sproximity_to_marine_km, Sproximity_to_inland_km)) + 
        geom_tile(aes(fill=mu)) +
        scale_y_continuous(expand=c(0,0)) +
        scale_x_continuous(expand=c(0,0)) +
        scale_fill_gradientn(labels=scales::percent, 
                             # midpoint = median(dri$mu),
                             colors = rev(hcl.colors(20, "RdYlBu")),
                             limits=c(0, max(dri$mu))) +
        geom_point(data = dat, alpha=0.2, size=.5) +
        labs(fill = 'P(dried consumption)') +
        coord_fixed()
    
    # ggMarginal(g1, type='histogram')
    
    g1
    g2<-g1 %+% fres + labs(fill = 'P(fresh consumption)') 
    
    lhs<-plot_grid(g1, g2, nrow=1, labels=c('a', 'b'))
    # top<-plot_grid(gc, gd, nrow=1, labels=c('a', 'b'))
    # pp<-plot_grid(top, lhs, nrow=2, rel_heights=c(0.4, 1))
    
    
    return(lhs)
    
    }