library(survey)
library(srvyr)
library(tidyverse)

lsms_all<-read.csv(file = 'data/lsms_subset/lsms_for_mod.csv')
lsms_fish<-read.csv(file = 'data/lsms_subset/lsms_fish.csv')

# Observed proportions
obs<-lsms_fish %>% group_by(country, tot_hh) %>% 
    summarise(n_dried = n_distinct(hh_id[form %in% c('dried', 'smoked', 'dry/smoked')]),
              n_fresh = n_distinct(hh_id[form %in% c('fresh')])) %>% 
    mutate(Dried = n_dried / tot_hh,
           Fresh = n_fresh / tot_hh) %>% 
    select(country, Dried, Fresh) %>% 
    pivot_longer(-country, names_to = 'var', values_to = 'm')

# Observed with weights
# create survey object with household cluster 'weights', and household cluster 'ids'
# strata not included, as not all datasets had this variable
ss<-lsms_all %>% filter(!is.infinite(hhweight)) %>% 
    group_by(hh_cluster) %>% mutate(n_hh = n_distinct(hh_id)) %>% 
    filter(n_hh > 1) %>% 
    as_survey_design(ids = hh_cluster, weights=hhweight)


ss %>% group_by(country, dried) %>% 
    summarise(m = survey_prop(vartype=c('ci'))) 

obs_w<-rbind(
    ss %>% group_by(country, dried) %>% 
        summarise(m = survey_prop(vartype=c('ci'))) %>% 
        mutate(var = 'Dried') %>% 
        filter(dried == 'yes') %>% 
        select(country, m, m_low, m_upp, var),

    ss %>% group_by(country, any_fish) %>% 
        summarise(m = survey_prop(vartype=c('ci'))) %>% 
        mutate(var = 'fish') %>% 
        filter(any_fish == 'yes') %>% 
        select(country, m, m_low, m_upp, var),

    ss %>% group_by(country, fresh) %>% 
        summarise(m = survey_prop(vartype=c('ci'))) %>% 
        mutate(var = 'Fresh') %>% 
        filter(fresh == 'yes') %>% 
        select(country, m, m_low, m_upp, var)
)


# Bayesian model: country level intercepts
load(file = 'data/mod/lsms_mod.rds')
load(file = 'data/mod/lsms_mod_fresh.rds')

targets::tar_load(lsms_proximity)
dat<-lsms_proximity
mod_dat<-mod_prep(lsms_proximity) %>% 
    data_grid(Sproximity_to_inland_km = 0,Sproximity_to_marine_km = 0,Sproximity_to_city_mins = 0, Swealth = 0,country=unique(mod_dat$country),Sn_hh = 0)

# posterc<-rbind(m2 %>%
#         spread_draws(r_country[state, term], b_Intercept) %>% 
#         mutate(r_country = inv_logit(r_country + b_Intercept)) %>%
#         median_qi() %>% 
#         mutate(var = 'Dried', country = state, m = r_country, m_low = r_country.lower, m_upp = r_country.upper, data = 'Model') %>% 
#         select(country, m, m_low, m_upp, var, data),
#       m3 %>%
#           spread_draws(r_country[state, term], b_Intercept) %>% 
#           mutate(r_country = inv_logit(r_country + b_Intercept)) %>%
#           median_qi() %>% 
#           mutate(var = 'Fresh', country = state, m = r_country, m_low = r_country.lower, m_upp = r_country.upper, data = 'Model') %>% 
#           select(country, m, m_low, m_upp, var, data)
# )
    
posterc2<-rbind(
    mod_dat %>%  
    add_epred_draws(m2, ndraws = 100, re_formula = ~ (1 | country)) %>% 
    median_qi() %>% 
    mutate(m = .epred, m_low = .lower, m_upp = .upper, data = 'Model (expected)', var = 'Dried') %>% 
    select(country, m, m_low, m_upp, var, data),
    mod_dat %>%  
        add_epred_draws(m3, ndraws = 100, re_formula = ~ (1 | country)) %>% 
        median_qi() %>% 
        mutate(m = .epred, m_low = .lower, m_upp = .upper, data = 'Model (expected)', var = 'Fresh') %>% 
        select(country, m, m_low, m_upp, var, data)
)

plotter<-rbind(obs %>% mutate(data = 'Survey (observed)'), 
               obs_w %>%
                   filter(var != 'fish') %>% 
                   mutate(data = 'Survey (weighted)'), posterc2) %>% 
    mutate(group = paste(country, var),
           data = factor(data, levels = unique(data)))

pdf(file = 'fig/weighted_sensitivity.pdf', height = 5, width=9)
ggplot(plotter, aes(country, m, col=data)) +
    geom_rect(xmin =0.5, xmax = 1.5, ymin =-Inf, ymax = Inf, fill='grey', alpha=0.01, col='white') +
    geom_rect(xmin =2.5, xmax = 3.5, ymin =-Inf, ymax = Inf, fill='grey', alpha=0.01, col='white') +
    geom_rect(xmin =4.5, xmax = 5.5, ymin =-Inf, ymax = Inf, fill='grey', alpha=0.01, col='white') +
    geom_pointrange(data = plotter, aes(ymin = m_low, ymax = m_upp), 
                    position = position_dodge(width=0.5)) +
    facet_wrap(~var, nrow=2) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_discrete(limits=levels(mod_dat$country)[c(1,4,3,2,6,5)]) +
    labs(x = '', y = 'Probability fish consumption') +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 8), 
          axis.title = element_text(size = 8))
dev.off()
