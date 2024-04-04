library(rethinking)
library(rstan)

targets::tar_load(lsms_proximity)
dat<-lsms_proximity

mod_dat<-dat %>% ungroup() %>% mutate(
    proximity_to_water_km = scale(proximity_to_water_km)[,1],
    proximity_to_city_mins = scale(proximity_to_city_mins)[,1],
    hh_cluster = as.factor(hh_cluster),
    country = as.factor(country),
    response = ifelse(dried == 'yes', 1, 0)) %>% 
    filter(!is.na(n_hh)) %>%  ## mostly in Tanzania - check these
    select(-any_fish)

## binomial model of dried fish consumption, hierarchical by cluster + country
m<-ulam(
    alist(
        response ~ dbinom(1, p),
        
        logit(p) <-  a_bar + 
            
            # access covariates
            b_1*proximity_to_water_km +
            b_2*proximity_to_city_mins + 
            b_3*n_hh +
            
            # nested households in countries
            # x[hh_cluster]*sigma_a +
            x2[country],
        
        # x[hh_cluster] ~ dnorm(0, 1),
        x2[country] ~ dnorm(0, 1),
        b_1 ~ dnorm(0, 1),
        b_2 ~ dnorm(0, 1),
        b_3 ~ dnorm(0, 1),
        
        a_bar ~ dnorm(0, 1) 
        # sigma_a ~ dexp(1),
        # gq > vector[hh_cluster]:a <<- a_bar + x*sigma_a
        
    ), data = mod_dat, chains = 3, cores = 4, log_lik=TRUE)


m1<-brm(data = mod_dat, family = binomial,
    response | trials(1) ~ 1 + proximity_to_water_km + proximity_to_city_mins + n_hh +
        (1 | country),
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(normal(0, 10), class = b),
              prior(cauchy(0, 1), class = sd)),
    iter = 1000, warmup = 500, chains = 3, cores = 4,
    seed = 10)