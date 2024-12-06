fig_heat<-function(dat){
    
    basesize = 9
    
    load(file = 'data/mod/lsms_mod.rds')
    load(file = 'data/mod/lsms_mod_fresh.rds')
    
    # resolution of predictions along proximity range
    res = 100
    
    raw_r<-expand_grid(distance_to_marine = seq_range(dat$distance_to_marine, n = res),
                       distance_to_inland = seq_range(dat$distance_to_inland, n = res)) 
    
    # proximity to water
    dri<-expand_grid(Sproximity_to_marine_km = seq_range(dat$Sproximity_to_marine_km, n = res),
                  Sproximity_to_inland_km = seq_range(dat$Sproximity_to_inland_km, n = res)) %>% 
        mutate(Sproximity_to_city_mins = 0, 
               # country = 'marginal',
               urban_rural = 'Urban',
               # urban = 0, rural = 0,
                  Swealth = 0,
                  Sn_hh = 0) %>% 
        add_epred_draws(m2, ndraws = 100, re_formula = NA) %>% 
        group_by(Sproximity_to_marine_km, Sproximity_to_inland_km) %>% 
        summarise(y = median(.epred), lo = HPDI(.epred)[1], hi = HPDI(.epred)[2]) 
    
    dri$distance_to_marine<-raw_r$distance_to_marine
    dri$distance_to_inland<-raw_r$distance_to_inland
    
    fres<-expand_grid(Sproximity_to_marine_km = seq_range(dat$Sproximity_to_marine_km, n = res),
                     Sproximity_to_inland_km = seq_range(dat$Sproximity_to_inland_km, n = res)) %>% 
        mutate(Sproximity_to_city_mins = 0, 
               # country = 'marginal',
               urban_rural = 'Urban',
               # urban = 0, rural = 0,
               Swealth = 0,
               Sn_hh = 0) %>% 
        add_epred_draws(m3, ndraws = 100, re_formula = NA) %>% 
        group_by(Sproximity_to_marine_km, Sproximity_to_inland_km) %>% 
        summarise(y = median(.epred) , lo = HPDI(.epred)[1], hi = HPDI(.epred)[2])
    
    fres$distance_to_marine<-raw_r$distance_to_marine
    fres$distance_to_inland<-raw_r$distance_to_inland
    
    hh_clusters<-dat %>% group_by(hh_cluster) %>% summarise(distance_to_marine = mean(distance_to_marine), distance_to_inland = mean(distance_to_inland))
    
    g1<-ggplot(dri, aes(distance_to_marine, distance_to_inland)) + 
        geom_tile(aes(fill=y)) +
        scale_y_continuous(expand=c(0,0)) +
        scale_x_continuous(expand=c(0,0)) +
        scale_fill_gradientn(labels=scales::percent, 
                             # midpoint = median(dri$mu),
                             colors = rev(hcl.colors(20, "RdYlBu")),
                             limits=c(0, max(c(dri$y, fres$y)))) +
        geom_point(data = hh_clusters, alpha=0.1, size=.005, col='black') +
        theme(plot.margin = unit(c(.05, .5, .05, .05), 'cm'),
              legend.position = 'inside', legend.position.inside = c(0.85, 0.8),
              legend.text=element_text(color='white'),
              axis.text = element_text(size = basesize), 
              axis.title = element_text(size = basesize)) +
        labs(fill = '',x = 'Distance to marine, km', y = 'Distance to inland, km') 

    
    g2<-g1 %+% fres +
        guides(fill = 'none') 
    
    lhs<-plot_grid(g1 ,#+ annotate('text', x = 1000, y = 330, label = 'Dried fish', colour='white'), 
                   g2, nrow=2, labels=c('a', 'b'))
    
    return(lhs)
    
}


## summary stats
# dri %>% ungroup() %>%  filter(distance_to_marine < 250 & distance_to_inland < 350) %>% summarise(mean(y))
# fres %>% ungroup() %>%  filter(distance_to_marine < 250 & distance_to_inland < 350) %>% summarise(mean(y))
