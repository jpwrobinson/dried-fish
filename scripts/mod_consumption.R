library(rethinking)
library(rstan)

tar_load(lsms_proximity)
dat<-lsms_proximity

mod_dat<-dat %>% ungroup() %>% mutate(
    proximity_to_water_km = scale(proximity_to_water_km)[,1],
    proximity_to_city_mins = scale(proximity_to_city_mins)[,1],
    hh_id = as.factor(hh_id),
    country = as.factor(country),
    response = ifelse(dried == 'yes', 1, 0)
)

## 1. basic model with species intercepts and global cohort and hollings covariates
m<-ulam(
    alist(
        response ~ dbinom(1, p),
        
        logit(p) <-  a_bar + 
            
            # access covariates
            b_1*proximity_to_water_km +
            b_2*proximity_to_city_mins + 
            
            # nested households in countries
            # x[hh_cluster]*sigma_a + 
            x2[country],
        
        # x[hh_cluster] ~ dnorm(0, 1),
        x2[country] ~ dnorm(0, 1),
        b_1 ~ dnorm(0, 1),
        b_2 ~ dnorm(0, 1),
        
        a_bar ~ dnorm(0, 1) 
        # sigma_a ~ dexp(1),
        # gq > vector[hh_id]:a <<- a_bar + x*sigma_a
        
    ), data = mod_dat, chains = 3, cores = 4, log_lik=TRUE)
