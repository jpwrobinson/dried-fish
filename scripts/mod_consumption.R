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

# weights investigation
# https://rpubs.com/corey_sparks/157901
# mod_dat$weight<-mod_dat$weight/mean(mod_dat$weight)

m2<-brm(data = mod_dat, family = bernoulli,
        # response_dried | weights(weight) ~ 0 + 
            response_dried ~ 1 + 
            Sproximity_to_marine_km * Sproximity_to_inland_km + # Sproximity_to_water_km +
            Sproximity_to_city_mins + Sn_hh + Swealth + urban_rural +
            (1 | country / hh_cluster),
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(cauchy(0, 10), class = sd)),
        iter = 1000, warmup = 500, chains = 3, cores = 6,
        seed = 10)

save(mod_dat, m2, file = 'data/mod/lsms_mod.rds')

m3<-brm(data = mod_dat, family = bernoulli,
        response_fresh ~ 1 + #nearest_water
            Sproximity_to_marine_km * Sproximity_to_inland_km + # Sproximity_to_water_km +
            Sproximity_to_city_mins + Sn_hh + Swealth + urban_rural +
            (1 | country / hh_cluster),
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(cauchy(0, 10), class = sd)),
        iter = 1000, warmup = 500, chains = 3, cores = 6,
        seed = 10)

save(mod_dat, m3, file = 'data/mod/lsms_mod_fresh.rds')

load(file = 'data/mod/lsms_mod.rds')
summary(m2)
conditional_effects(m2)
plot(m2)
ranef(m2)$country

# this is the expectation of the posterior
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

mod_dat %>%  
    data_grid(Sproximity_to_inland_km = 0,
              Sproximity_to_marine_km = 0,
              Sproximity_to_city_mins = 0, 
              Swealth = 0,
              country=unique(mod_dat$country),
              Sn_hh = 0) %>%  
    add_epred_draws(m2, ndraws = 100, re_formula = ~ (1 | country)) %>%  
    ggplot(aes(x = country)) +
    stat_pointinterva(aes(y = .epred), .width = 0.95, alpha = 0.5) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = '', y = 'Probability of dried fish consumption')


# this is the linear predictor
m2 %>%
    spread_draws(r_country[state, term], b_Intercept) %>% 
    mutate(r_country = r_country + b_Intercept) %>% 
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

ppc_dens_overlay(y = dat$response_fresh,
                 yrep = posterior_predict(m3, draws = 50))



posterior <- as.array(m2)
mcmc_pairs(posterior, regex_pars = 'S*')


## sens test to check with simmance et al. 2022
mS<-brm(data = mod_dat %>% 
            filter(country %in% c('TZA', 'MWI', 'UGA')) %>% 
            mutate(prox = ifelse(proximity_to_water_km<= 5, 'near', 'far')), 
        family = bernoulli,
        response_dried ~ 1 + #nearest_water + 
            #Sproximity_to_marine_km * Sproximity_to_inland_km + # Sproximity_to_water_km + 
            prox*Swealth +
            Sproximity_to_city_mins + Sn_hh + Swealth +
            # marine + inland + 
            (1 | country / hh_cluster),
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(cauchy(0, 1), class = sd)),
        iter = 1000, warmup = 500, chains = 3, cores = 6,
        seed = 10)

conditional_effects(mS)
# No difference between far and near consumption. 
# Dried fish decreases with increasing distance from city. 
# No wealth relationship or interaction with distance to city.



# with weights + survey design
# Logistic model of survey design
survey_dat<-read.csv('data/lsms_with_covariates.csv') %>% 
    left_join(lsms_all %>% select(hh_id, country, hhweight), by = c('hh_id', 'country')) %>% 
    filter(!is.infinite(hhweight)) %>% 
    filter(!is.na(n_hh) & !is.na(monthly_exp)) %>%  ## mostly in Tanzania - check these
    group_by(country) %>% 
    mutate(wealth = scales::rescale(monthly_exp / sqrt(n_hh), to = c(0,1))) %>%  ## income is equivalence scaled by square root of household size
    ungroup() %>% mutate(
        proximity_to_city_mins = ifelse(proximity_to_city_mins == 0, 1, proximity_to_city_mins),
        log10_proximity_to_city_mins = log10(proximity_to_city_mins),
        Sn_hh = scale(n_hh)[,1],
        Swealth = scale(wealth)[,1],
        Sproximity_to_water_km = scale(proximity_to_water_km)[,1],
        Sproximity_to_inland_km = scale(distance_to_inland)[,1],
        Sproximity_to_marine_km = scale(distance_to_marine)[,1],
        Sproximity_to_city_mins = scale(log10_proximity_to_city_mins)[,1],
        response_dried = ifelse(dried == 'yes', 1, 0),
        response_fresh = ifelse(fresh == 'yes', 1, 0)) %>% 
    as_survey_design(ids = hh_cluster, weights=hhweight)

mod1<-svyglm(
        formula = response_dried ~ 1 +
            Sproximity_to_marine_km * Sproximity_to_inland_km + 
            Sproximity_to_city_mins + Sn_hh + Swealth,
        design = survey_dat,
        na.action = na.omit,
        family = quasibinomial)

mod2<-svyglm(
    formula = response_fresh ~ 1 +
        Sproximity_to_marine_km * Sproximity_to_inland_km + 
        Sproximity_to_city_mins + Sn_hh + Swealth,
    design = survey_dat,
    na.action = na.omit,
    family = quasibinomial)

library(broom)
summary(mod1)
summary(mod2)

rbind(tidy(mod1) %>% mutate(var = 'Dried'),
      tidy(mod2) %>% mutate(var = 'Fresh')) %>% 
    mutate(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error) %>% 
    filter(term!='(Intercept)') %>% 
    ggplot(aes(x=term, estimate, ymin = ymin, ymax = ymax, col=var)) +
    geom_hline(yintercept = 0) +
    geom_pointrange(position = position_dodge(width=0.5)) +
    coord_flip() 
