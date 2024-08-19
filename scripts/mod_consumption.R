library(rstan)
library(brms)

targets::tar_load(lsms_proximity)
dat<-lsms_proximity

# ## binomial model of dried fish consumption, hierarchical by cluster + country


# m1<-brm(data = mod_dat, family = bernoulli,
#     response ~ 1 + proximity_to_water_km + proximity_to_city_mins + n_hh + wealth +
#         # marine + inland + 
#         nearest_water +
#         (1 | country / hh_cluster),
#     prior = c(prior(normal(0, 1), class = Intercept),
#               prior(normal(0, 1), class = b),
#               prior(cauchy(0, 1), class = sd)),
#     iter = 1000, warmup = 500, chains = 3, cores = 6,
#     seed = 10)

mod_dat<-mod_prep(lsms_proximity)

m2<-brm(data = mod_dat, family = bernoulli,
        response_dried ~ 1 + nearest_water + Sproximity_to_water_km + Sproximity_to_city_mins + Sn_hh + Swealth +
            # marine + inland + 
            (1 | country / hh_cluster),
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(cauchy(0, 1), class = sd)),
        iter = 1000, warmup = 500, chains = 3, cores = 6,
        seed = 10)

m3<-brm(data = mod_dat, family = bernoulli,
        response_fresh ~ 1 + nearest_water + Sproximity_to_water_km + Sproximity_to_city_mins + Sn_hh + Swealth +
            # marine + inland + 
            (1 | country / hh_cluster),
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(cauchy(0, 1), class = sd)),
        iter = 1000, warmup = 500, chains = 3, cores = 6,
        seed = 10)

save(mod_dat, m2, file = 'data/mod/lsms_mod.rds')
save(mod_dat, m3, file = 'data/mod/lsms_mod_fresh.rds')

load(file = 'data/mod/lsms_mod.rds')
summary(m2)
conditional_effects(m2)
plot(m2)
ranef(m2)$country

mod_dat %>%  
    data_grid(Sproximity_to_water_km = seq_range(proximity_to_water_km, n = 100),
              Sproximity_to_city_mins = 0, 
              Swealth = 0,
              # marine = 0, inland = 0, 
              nearest_water=levels(mod_dat$nearest_water),
              # country=unique(mod_dat$country),
              Sn_hh = 0) %>%  
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>%  
    ggplot(aes(x = proximity_to_water_km)) +
    stat_lineribbon(aes(y = .epred, fill=nearest_water), .width = 0.95, alpha = 0.5) +
    # geom_dots(data = mod_dat, aes(side = ifelse(response==0, "bottom", "top")), 
    #           pch = 19, color = "grey20", scale = 0.2) +
    scale_y_continuous(labels = scales::label_percent()) +
    # facet_wrap(~country) + 
    labs(x = 'Proximity to water, km', y = 'Probability of dried fish consumption')

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

## summary stats
# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/
# https://www.andrewheiss.com/blog/2022/09/26/guide-visualizing-types-posteriors/
    

load(file = 'data/mod/lsms_mod_fresh.rds')
summary(m3)
conditional_effects(m3)
plot(m3)
ranef(m3)$country

