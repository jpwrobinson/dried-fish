
fig_post<-function(dat, test=FALSE){
    
    if(test == TRUE){
        load(file = 'data/mod/lsms_mod_test.rds')
        load(file = 'data/mod/lsms_mod_fresh_test.rds')
        } else {
                load(file = 'data/mod/lsms_mod.rds')
                load(file = 'data/mod/lsms_mod_fresh.rds')
        }
    
        ylab = 'Probability fish consumption'
        basesize = 9
        
        c_labs<-data.frame(country = c("CIV","SEN","NGA","MWI","UGA","TZA"),
                           c_name = c("Côte D'Ivoire", "Senegal", "Nigeria", "Malawi", "Uganda", "Tanzania"))
    
    # posterior effect sizes
    posterior <- mcmc_intervals_data(m2)
    posterior2 <- mcmc_intervals_data(m3)
    
    # parameters
    p<- c('b_Sproximity_to_marine_km', 'b_Sproximity_to_inland_km',
          'b_Sproximity_to_city_mins', 'b_Sn_hh',# 'b_Swealth_country0_1', 
          'b_Swealth_ppp', 'b_urban_ruralUrban', 'b_Sproximity_to_marine_km:Sproximity_to_inland_km')
    
    poster<-rbind(posterior %>% mutate(fish = 'Dried'),
                  posterior2 %>% mutate(fish = 'Fresh')) %>% 
        filter(parameter %in% p)
    
    gb<-ggplot(poster, aes(m, parameter, xmin = ll, xmax = hh, col=fish )) +
        geom_vline(xintercept = 0, col='grey') +
        geom_pointrange(data = poster, aes( xmin = ll, xmax = hh), position = position_dodge(width=0.5)) +
        geom_pointrange(data=poster, linewidth=1, aes(xmax=h, xmin =l), position = position_dodge(width=0.5)) +
        scale_x_continuous(breaks=seq(-1, 1, by = .2)) +
        scale_y_discrete(limits = p[c(6,1,2,3,4,5)],
                         position = 'right',
            labels = c('b_Sproximity_to_marine_km' = 'Proximity\nmarine water', 
                                'b_Sproximity_to_inland_km' = 'Proximity\ninland water', 
                                'b_Sproximity_to_city_mins' = 'Proximity\nurban centre', 
                                'b_Sn_hh' = 'Household\nsize', 
                                # 'b_Swealth_country0_1' = 'Household\nwealth [0-1]', 
                                'b_Swealth_ppp' = 'Household\nwealth (PPP)', 
                                'b_urban_ruralUrban' = 'Urban',
                                'b_Sproximity_to_marine_km:Sproximity_to_inland_km' = 'Proximity\nmarine*inland')) +
        labs(x = 'Relative effect size', y = '') +
        theme_sleek() +
        scale_colour_manual(values = pcols_named) +
        theme(legend.position = 'inside', legend.position.inside = c(0.25, 0.85),
              axis.text = element_text(size = basesize), 
              axis.title = element_text(size = basesize),
              legend.title=element_blank())
    
    # gather country intercept posteriors holding other covariates to mean 0
    mod_sim<-dat %>% 
        data_grid(Sproximity_to_inland_km = 0,
                  Sproximity_to_marine_km = 0,
                  Sproximity_to_city_mins = 0,
                  urban_rural = 'Urban',
                  # Swealth_country0_1 = 0,
                  Swealth_ppp = 0,
                  country=unique(mod_dat$country),Sn_hh = 0)
    
    post_mdn<-rbind(
        mod_sim %>% filter(country=='CIV') %>% 
            add_epred_draws(m2, ndraws = 100, re_formula = NA) %>% 
            median_qi() %>% 
            mutate(m = .epred, m_low = .lower, m_upp = .upper, data = 'Grand mean', fish = 'Dried'),
        mod_sim %>% filter(country=='CIV') %>% 
            add_epred_draws(m3, ndraws = 100, re_formula = NA) %>% 
            median_qi() %>% 
            mutate(m = .epred, m_low = .lower, m_upp = .upper, data = 'Grand mean', fish = 'Fresh')
    )
    
    posterc<-rbind(
        mod_sim %>%  
            add_epred_draws(m2, ndraws = 100, re_formula = ~ (1 | country)) %>% 
            median_qi() %>% 
            mutate(m = .epred, ll = .lower, hh = .upper, data = 'Model (expected)', fish = 'Dried') %>% 
            select(country, m, ll, hh, fish, data),
        mod_sim %>%  
            add_epred_draws(m3, ndraws = 100, re_formula = ~ (1 | country)) %>% 
            median_qi() %>% 
            mutate(m = .epred, ll = .lower, hh = .upper, data = 'Model (expected)', fish = 'Fresh') %>% 
            select(country, m, ll, hh, fish, data)
    ) %>% 
        left_join(c_labs)
    
    
    ga<-ggplot(posterc, aes(m,c_name, col=fish)) +
        geom_pointrange(data = posterc, aes( xmin = ll, xmax = hh), position = position_dodge(width=0.5)) +
        geom_path(colour='grey', alpha=0.8) +
        scale_x_continuous(labels = scales::label_percent()) +
        scale_colour_manual(values = pcols_named) +
        scale_y_discrete(limits=rev(c_labs$c_name)) +
        labs(y = '', x = ylab) +
        theme(legend.position = 'none',
              axis.text = element_text(size = basesize), 
              axis.title = element_text(size = basesize))
    
    
    lhs<-plot_grid(ga, gb, nrow=1, labels=c('A', 'B'), rel_widths=c(0.8, 1))
    return(lhs)
    
}
