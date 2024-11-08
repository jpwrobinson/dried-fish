source('scripts/00_plot.R')

## data cleaning functions
source('scripts/norway_clean.R')
source('scripts/read_nutrient_data.R')
lsms_read<-function(){
    source('scripts/read_lsms.R')
    output<-list(lsms_hh, lsms_fish, lsms_all %>% filter(!is.na(lat)))
    return(output)
}

## spatial processing functions
source('scripts/water_prox.R')
source('scripts/city_prox.R')

# model functions
mod_prep<-function(dat){
    
    mod_dat<-dat %>% 
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
            nearest_water = ifelse(distance_to_marine < distance_to_inland , 'Marine', 'Inland'),
            nearest_water = ifelse(distance_to_inland > 100000 & distance_to_marine > 100000, 'Remote', nearest_water),
            nearest_water = as.factor(ifelse(distance_to_inland < 50000 & distance_to_marine < 50000, 'Marine and Inland', nearest_water)),
            hh_cluster = as.factor(hh_cluster),
            country = as.factor(country),
            response_dried = ifelse(dried == 'yes', 1, 0),
            response_fresh = ifelse(fresh == 'yes', 1, 0)) %>% 
        select(-any_fish)
    
    return(mod_dat)
}

# model posteriors
mod_post<-function(mod, dat, var, raw_var){
    
    condo<-conditional_effects(mod, as.name(var), prob=0.95)[[1]] %>% 
        mutate(raw = seq_range(dat[[raw_var]], n=100)) %>% 
        mutate(lower95 = lower__, upper95 = upper__) %>% 
        select({{var}}, raw, estimate__, lower95, upper95)
        
    c2<-conditional_effects(mod, as.name(var), prob=0.5)[[1]] %>% 
            select(lower__:upper__)
    
    condo$lower50<-c2$lower__
    condo$upper50<-c2$upper__
    
    return(condo)
}



# figure functions
source('scripts/fig/figures_to_pdf.R')
source('scripts/fig/fig_fresh_contrast.R')
source('scripts/fig/fig_nutrient_density.R')
source('scripts/fig/fig_dried_rni.R')
source('scripts/fig/fig_rni_species.R')
source('scripts/fig/fig_portion_size.R')
source('scripts/fig/fig_map.R')
source('scripts/fig/fig_post.R')
source('scripts/fig/fig_mod.R')
source('scripts/fig/fig_heat.R')
source('scripts/fig/fig_contaminant_species.R')
source('scripts/fig/fig_contaminant_content.R')
source('scripts/fig/fig_contaminant_portions.R')
source('scripts/dried_fish_maps.R')


## random slice min max
slice_min_max <- function(df, order_by = value, n = 1) {
    order_by = enquo(order_by)
    min <- slice_min(df, !!order_by, n = n) %>%
        mutate(type = "min")
    max <- slice_max(df, !!order_by, n = n) %>%
        mutate(type = "max")
    df <- bind_rows(min, max) %>%
        as_tibble()
    return(df)
    
}