source('scripts/00_plot.R')

## data cleaning functions
source('scripts/norway_clean.R')
source('scripts/read_nutrient_data.R')
lsms_read<-function(){
    source('scripts/read_lsms.R')
    output<-list( lsms_all %>% filter(!is.na(lat)), lsms_hh, lsms_fish)
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
    
    return(mod_dat)
}

source('scripts/fig/fig_mod.R')


# figure functions
source('scripts/fig/figures_to_pdf.R')
source('scripts/fig/fig_fresh_contrast.R')
source('scripts/fig/fig_nutrient_density.R')
source('scripts/fig/fig_dried_rni.R')
source('scripts/fig/fig_rni_species.R')
source('scripts/fig/fig_portion_size.R')
source('scripts/fig/fig_map.R')
source('scripts/fig/fig_mod.R')
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