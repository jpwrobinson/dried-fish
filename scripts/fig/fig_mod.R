

load(file = 'data/mod/lsms_mod.rds')

scales<-scale_y_continuous(labels = scales::label_percent()) +
        scale_fill_manual(values = realm_cols_named)

# proximity to water
mod_dat %>%  
    data_grid(Sproximity_to_water_km = seq_range(Sproximity_to_water_km, n = 100),
              Sproximity_to_city_mins = 0, 
              Swealth = 0,
              nearest_water=levels(mod_dat$nearest_water),
              Sn_hh = 0) %>%  
    mutate(proximity_to_water_km = rep(seq_range(proximity_to_water_km, n = 100), each=2)) %>% 
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>%  
    ggplot(aes(x = proximity_to_water_km)) +
    stat_lineribbon(aes(y = .epred, fill=nearest_water), .width = 0.95, alpha = 0.5) +
    stat_lineribbon(aes(y = .epred, fill=nearest_water), .width = 0.5, alpha = 0.5) +
    # geom_dots(data = mod_dat, aes(y = response, side = ifelse(response==0, "bottom", "top")),
    #           pch = 19, color = "grey20", scale = 0.1) +
    scales +
    labs(x = 'Proximity to water, km', y = 'Probability of dried fish consumption')


# proximity to urban centre
mod_dat %>%  
    data_grid(Sproximity_to_water_km = 0,
              Sproximity_to_city_mins = seq_range(Sproximity_to_city_mins, n = 100),
              Swealth = 0,
              nearest_water=levels(mod_dat$nearest_water),
              Sn_hh = 0) %>%  
    mutate(proximity_to_city_mins = rep(seq_range(proximity_to_city_mins, n = 100), each=2)) %>% 
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>%  
    ggplot(aes(x = proximity_to_city_mins)) +
    stat_lineribbon(aes(y = .epred, fill=nearest_water), .width = 0.95, alpha = 0.5) +
    stat_lineribbon(aes(y = .epred, fill=nearest_water), .width = 0.5, alpha = 0.5) +
    scales +
    labs(x = 'Proximity to urban centre, mins', y = 'Probability of dried fish consumption')

# country level intercepts
m2 %>%
    spread_draws(r_country[state, term]) %>% 
    ggplot(aes(x = inv_logit(r_country),
               y = state)) +
    # stat_halfeye(.width = c(0.5, 0.8)) +
    stat_pointinterval() +
    coord_flip() +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_y_discrete(limits=levels(mod_dat$country)[c(1,4,3,2,6,5)]) +
    labs(y = '', x = 'Probability of dried fish consumption')


lhs<-plot_grid(ga, gb, nrow=2, labels=c('a', 'b'))
rhs<-plot_grid(gc, gc, gc, nrow=3, labels =c('c', 'd', 'e'))

plot_grid(lhs, rhs, ncol = 2)
