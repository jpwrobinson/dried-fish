
mod_dat<-dat %>% mutate(
    proximity_to_water_km = scale(proximity_to_water_km),
    proximity_to_city_mins = scale(proximity_to_city_mins),
    hh_id = as.factor(hh_id),
    country = as.factor(country)
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
            x[hh_id]*sigma_b + 
            x2[country],
        
        x[cohort] ~ dnorm(0, 1),
        x2[id] ~ dnorm(0, 1),
        z[species] ~ dnorm(0, 1),
        c ~ dnorm(0, 1),
        
        a_bar ~ dnorm(0, 1), 
        sigma_a ~ dexp(1),
        sigma_b ~ dexp(1),
        gq> vector[hh_id]:b <<- x*sigma_b
        
    ), data = mod_dat, chains = 3, cores = 4, log_lik=TRUE)