library(rethinking)
library(rstan)

targets::tar_load(lsms_proximity)
dat<-lsms_proximity

mod_dat<-dat %>% 
    filter(!is.na(n_hh) & !is.na(monthly_exp)) %>%  ## mostly in Tanzania - check these
    group_by(country) %>% 
    mutate(Swealth = scales::rescale(monthly_exp / sqrt(n_hh), to = c(0,1))) %>%  ## income is equivalence scaled by square root of household size
    ungroup() %>% mutate(
    Sn_hh = scale(n_hh)[,1],
    Sproximity_to_water_km = scale(proximity_to_water_km)[,1],
    Sproximity_to_city_mins = scale(proximity_to_city_mins)[,1],
    nearest_water = as.factor(ifelse(distance_to_inland > distance_to_marine, 'Marine', 'Inland')),
    marine = ifelse(distance_to_inland > distance_to_marine, 1, 0),
    inland = ifelse(marine == 1, 0, 1),
    hh_cluster = as.factor(hh_cluster),
    country = as.factor(country),
    response = ifelse(dried == 'yes', 1, 0)) %>% 
    select(-any_fish)

# ## binomial model of dried fish consumption, hierarchical by cluster + country
# m<-ulam(
#     alist(
#         response ~ dbinom(1, p),
#         
#         logit(p) <-  a_bar + 
#             
#             # access covariates
#             b_1*proximity_to_water_km +
#             b_2*proximity_to_city_mins + 
#             b_3*n_hh +
#             b_4*wealth +
#             b_5[nearest_water] +
#             
#             # nested households in countries
#             # x[hh_cluster]*sigma_a +
#             x2[country],
#         
#         # x[hh_cluster] ~ dnorm(0, 1),
#         x2[country] ~ dnorm(0, 1),
#         b_1 ~ dnorm(0, 1),
#         b_2 ~ dnorm(0, 1),
#         b_3 ~ dnorm(0, 1),
#         b_4 ~ dnorm(0, 1),
#         b_5[nearest_water] ~ dnorm(0, 1),
#         
#         a_bar ~ dnorm(0, 1) 
#         # sigma_a ~ dexp(1),
#         # gq > vector[hh_cluster]:a <<- a_bar + x*sigma_a
#         
#     ), data = mod_dat, chains = 3, cores = 6, log_lik=TRUE)
# precis(m)

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

m2<-brm(data = mod_dat, family = bernoulli,
        response ~ 1 + nearest_water*(Sproximity_to_water_km + Sproximity_to_city_mins + Sn_hh + Swealth) +
            # marine + inland + 
            (1 | country / hh_cluster),
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(cauchy(0, 1), class = sd)),
        iter = 1000, warmup = 500, chains = 3, cores = 6,
        seed = 10)

save(m2, file = 'data/mod/lsms_mod.rds')

load(file = 'data/mod/lsms_mod.rds')
summary(m2)
conditional_effects(m2)
plot(m2)
ranef(m2)$country

mod_dat %>%  
    data_grid(proximity_to_water_km = seq_range(proximity_to_water_km, n = 100),
              proximity_to_city_mins = 0, 
              wealth = 0,
              # marine = 0, inland = 0, 
              nearest_water=levels(mod_dat$nearest_water),
              # country=unique(mod_dat$country),
              n_hh = 0) %>%  
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

