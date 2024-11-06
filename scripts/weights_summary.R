library(survey)
library(srvyr)
library(tidyverse)

lsms_all<-read.csv(file = 'data/lsms_subset/lsms_for_mod.csv')

# Observed proportions
obs<-lsms_fish %>% group_by(country, tot_hh) %>% 
    summarise(n_dried = n_distinct(hh_id[form %in% c('dried', 'smoked', 'dry/smoked')]),
              n_fresh = n_distinct(hh_id[form %in% c('fresh')])) %>% 
    mutate(Dried = n_dried / tot_hh,
           Fresh = n_fresh / tot_hh) %>% 
    select(country, Dried, Fresh) %>% 
    pivot_longer(-country, names_to = 'var', values_to = 'm')

# Observed with weights
ss<-lsms_all %>% filter(!is.infinite(hhweight)) %>% 
    group_by(hh_cluster) %>% mutate(n_hh = n_distinct(hh_id)) %>% 
    filter(n_hh > 1) %>% 
    as_survey_design(strata = hh_cluster, weights = hhweight)

obs_w<-rbind(
    ss %>% group_by(country, dried) %>% 
        summarise(m = survey_prop(weights = hhweight, vartype=c('ci'))) %>% 
        mutate(var = 'Dried') %>% 
        filter(dried == 'yes') %>% 
        select(country, m, m_low, m_upp, var),

    ss %>% group_by(country, any_fish) %>% 
        summarise(m = survey_prop(weights = hhweight, vartype=c('ci'))) %>% 
        mutate(var = 'fish') %>% 
        filter(any_fish == 'yes') %>% 
        select(country, m, m_low, m_upp, var),

    ss %>% group_by(country, fresh) %>% 
        summarise(m = survey_prop(weights = hhweight, vartype=c('ci'))) %>% 
        mutate(var = 'Fresh') %>% 
        filter(fresh == 'yes') %>% 
        select(country, m, m_low, m_upp, var)
)


# Bayesian model: country level intercepts
load(file = 'data/mod/lsms_mod.rds')
load(file = 'data/mod/lsms_mod_fresh.rds')
posterior <- mcmc_intervals_data(m2, transformations = inv_logit)
posterior2 <- mcmc_intervals_data(m3, transformations = inv_logit)

posterc<-rbind(posterior %>% mutate(fish = 'Dried'),
               posterior2 %>% mutate(fish = 'Fresh')) %>% 
    filter(str_detect(parameter, 'r_country\\[')) %>% 
    mutate(country = str_replace_all(parameter, 't\\(r_country\\[', ''),
           country = str_replace_all(country, ',Intercept\\]\\)', ''),
           m_low = ll, m_upp = hh, var = fish, data = 'Model') %>% 
    select(country, m, m_low, m_upp, var, data)


plotter<-rbind(obs %>% mutate(data = 'Observed'), 
               obs_w %>%
                   filter(var != 'fish') %>% 
                   mutate(data = 'Weighted'), posterc) %>% 
    mutate(group = paste(country, var))

ggplot(plotter, aes(country, m, col=data)) +
    geom_rect(xmin =0.5, xmax = 1.5, ymin =-Inf, ymax = Inf, fill='grey', alpha=0.01, col='white') +
    geom_rect(xmin =2.5, xmax = 3.5, ymin =-Inf, ymax = Inf, fill='grey', alpha=0.01, col='white') +
    geom_rect(xmin =4.5, xmax = 5.5, ymin =-Inf, ymax = Inf, fill='grey', alpha=0.01, col='white') +
    geom_pointrange(data = plotter, aes(ymin = m_low, ymax = m_upp), 
                    position = position_dodge(width=0.5)) +
    # geom_path(aes(group=group)) +
    facet_wrap(~var, nrow=2) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_discrete(limits=levels(mod_dat$country)[c(1,4,3,2,6,5)]) +
    labs(x = '', y = 'Probability fish consumption') +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 8), 
          axis.title = element_text(size = 8))
