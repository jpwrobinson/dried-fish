
fig_post<-function(dat){
    
        load(file = 'data/mod/lsms_mod.rds')
        load(file = 'data/mod/lsms_mod_fresh.rds')
        ylab = 'Probability fish consumption'
        
    
    # posterior effect sizes
    posterior <- mcmc_intervals_data(m2)
    posterior2 <- mcmc_intervals_data(m3)
    
    # parameters
    p<- c('b_Sproximity_to_marine_km', 'b_Sproximity_to_inland_km', 'b_Sproximity_to_city_mins', 'b_Sn_hh', 'b_Swealth', 'b_Sproximity_to_marine_km:Sproximity_to_inland_km')
    
    poster<-rbind(posterior %>% mutate(fish = 'Dried'),
                  posterior2 %>% mutate(fish = 'Fresh')) %>% 
        filter(parameter %in% p)
    
    gb<-ggplot(poster, aes(m, parameter, xmin = ll, xmax = hh, col=fish )) +
        geom_vline(xintercept = 0, col='grey') +
        geom_pointrange(data = poster, aes( xmin = ll, xmax = hh), position = position_dodge(width=0.5)) +
        geom_pointrange(data=poster, linewidth=1, aes(xmax=h, xmin =l), position = position_dodge(width=0.5)) +
        scale_x_continuous(breaks=seq(-1, 4, by = 1)) +
        scale_y_discrete(limits = p[c(6,1,2,3,4,5)],
                         position = 'right',
            labels = c('b_Sproximity_to_marine_km' = 'Distance\nmarine water', 
                                'b_Sproximity_to_inland_km' = 'Distance\ninland water', 
                                'b_Sproximity_to_city_mins' = 'Distance\nurban centre', 
                                'b_Sn_hh' = 'Household\nsize', 
                                'b_Swealth' = 'Household\nwealth', 
                                'b_Sproximity_to_marine_km:Sproximity_to_inland_km' = 'Distance\nmarine*inland')) +
        labs(x = 'Posterior effect', y = '') +
        theme_sleek() +
        scale_colour_manual(values = pcols_named) +
        theme(legend.position = 'inside', legend.position.inside = c(0.8, 0.2),
              legend.title=element_blank())
    
    
    # country level intercepts
    posterior <- mcmc_intervals_data(m2, transformations = inv_logit)
    posterior2 <- mcmc_intervals_data(m3, transformations = inv_logit)
    
    posterc<-rbind(posterior %>% mutate(fish = 'Dried'),
                   posterior2 %>% mutate(fish = 'Fresh')) %>% 
        filter(str_detect(parameter, 'r_country\\[')) %>% 
        mutate(country = str_replace_all(parameter, 't\\(r_country\\[', ''),
               country = str_replace_all(country, ',Intercept\\]\\)', ''))
    
    ga<-ggplot(posterc, aes(country, m, col=fish )) +
        geom_vline(xintercept = 0, col='grey') +
        geom_pointrange(data = posterc, aes( ymin = ll, ymax = hh), position = position_dodge(width=0.5)) +
        geom_pointrange(data=posterc, linewidth=1, aes(ymax=h, ymin =l), position = position_dodge(width=0.5)) +
        scale_y_continuous(labels = scales::label_percent()) +
        scale_colour_manual(values = pcols_named) +
        scale_x_discrete(limits=levels(mod_dat$country)[c(1,4,3,2,6,5)]) +
        labs(x = '', y = ylab) +
        theme(legend.position = 'none')
    
    
    lhs<-plot_grid(ga, gb, nrow=1, labels=c('a', 'b'), rel_widths=c(0.8, 1))
    return(lhs)
    
}