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
        filter(!is.na(n_hh) & !is.na(monthly_exp) & !is.na(urban_rural)) %>%  ## mostly in Tanzania - check these
        group_by(country) %>% 
        mutate(log10_wealth_country = log10((monthly_exp / sqrt(n_hh)) + 1),
               # Swealth_country0 = scales::rescale(monthly_exp / sqrt(n_hh), to = c(0,1)),
               Swealth_country = scales::rescale(log10_wealth_country, to = c(0,1))) %>%  ## income is equivalence scaled by square root of household size, and 0-1 by country
        ungroup() %>% mutate(
            proximity_to_city_mins = ifelse(proximity_to_city_mins == 0, 1, proximity_to_city_mins),
            log10_proximity_to_city_mins = log10(proximity_to_city_mins),
            wealth_ppp = monthly_exp / ppp / sqrt(n_hh),  ## income is converted to PPP, scaled by square root of household size, and 0-1 across dataset
            log10_wealth_ppp = log10(wealth_ppp + 1),
            # Swealth_ppp = rescale(log10_wealth_ppp, to=c(0,1)),
            Swealth_ppp = scale(log10_wealth_ppp)[,1],
            Sn_hh = scale(n_hh)[,1],
            urban_rural = factor(str_to_title(urban_rural)),
            urban = ifelse(urban_rural == 'Urban', 1, 0),
            rural = ifelse(urban_rural == 'Rural', 1, 0),
            Sproximity_to_water_km = scale(proximity_to_water_km)[,1],
            Sproximity_to_inland_km = scale(distance_to_inland)[,1],
            Sproximity_to_marine_km = scale(distance_to_marine)[,1],
            Sproximity_to_city_mins = scale(log10_proximity_to_city_mins)[,1],
            nearest_water = ifelse(distance_to_marine < distance_to_inland , 'Marine', 'Inland'),
            nearest_water = ifelse(distance_to_inland > 1000 & distance_to_marine > 1000, 'Remote', nearest_water),
            nearest_water = as.factor(ifelse(distance_to_inland < 500 & distance_to_marine < 500, 'Marine and Inland', nearest_water)),
            hh_cluster = as.factor(hh_cluster),
            country = as.factor(country),
            response_dried = ifelse(dried == 'yes', 1, 0),
            response_fresh = ifelse(fresh == 'yes', 1, 0)) %>% 
        select(-any_fish)
    
    return(mod_dat)
}

# model posteriors
# Function takes model object, dataset, and variable name, estimates median y ~ var with 95% and 50% HPDI
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

# Function takes model object, dataset, and variable name, generates posterior samples across conditions, estimates median y ~ var with 95% and 50% HPDI
mod_post_contrast<-function(mod, dat, var, raw_var, quantile){
    
    Q = quantile(dat[[var]], probs = quantile)
    Swealth_Q = data.frame(Swealth_country = Q)
    
    condo<-conditional_effects(mod, effects = as.name(var), conditions = Swealth_Q, prob=0.95)[[1]] %>% 
        mutate(raw = seq_range(dat[[raw_var]], n=100)) %>% 
        mutate(lower95 = lower__, upper95 = upper__) %>% 
        select({{var}}, raw, estimate__, lower95, upper95)
    
    c2<-conditional_effects(mod, as.name(var),, conditions = Swealth_Q, prob=0.5)[[1]] %>% 
        select(lower__:upper__)
    
    condo$lower50<-c2$lower__
    condo$upper50<-c2$upper__
    
    
    return(condo)
}


plot_post<-function(dried, fresh, mod_dat, var, raw_var, xlab, quantile = NULL, type = 'conditional'){
    
    scales<-list(
        scale_y_continuous(labels = scales::label_percent(), limits=c(0,1)), 
        scale_fill_manual(values = pcols_named),
        scale_colour_manual(values = pcols_named))
    
    basesize = 9
    ylab = 'Probability of fish consumption'
    
    if(type == 'conditional'){
    # proximity to water (marine)
    da1<-mod_post(dried, mod_dat, as.name(var), as.name(raw_var)) %>% 
        mutate(mod = 'Dried') 
    
    fa1<-mod_post(fresh, mod_dat, as.name(var), as.name(raw_var)) %>% 
        mutate(mod = 'Fresh') 
    }
    
    if(type == 'contrast'){
        # proximity to water (marine)
        da1<-mod_post_contrast(dried, mod_dat, as.name(var), as.name(raw_var), quantile = quantile) %>% 
            mutate(mod = 'Dried') 
        
        fa1<-mod_post_contrast(fresh, mod_dat, as.name(var), as.name(raw_var), quantile = quantile) %>% 
            mutate(mod = 'Fresh') 
    }
    
    datter<-rbind(da1, fa1)
    
    gg<-ggplot(datter, aes(x = raw)) +
        geom_lineribbon(aes(y = estimate__, ymin = lower50, ymax = upper50,fill=mod), alpha = 0.5) +
        geom_lineribbon(aes(y = estimate__, ymin = lower95, ymax = upper95,fill=mod), alpha = 0.1) +
        scales +
        theme(legend.position = 'none', 
              plot.margin = unit(c(.05, .01, .05, .05), 'cm'),
              axis.text = element_text(size = basesize), 
              axis.title = element_text(size = basesize)) +
        labs(x = xlab, y = ylab)  
    
    # Create the inset histogram
    inset_hist <- ggplot(mod_dat, aes(.data[[raw_var]])) +
        geom_histogram(bins = 20, fill = "steelblue", color = "white") +
        scale_x_continuous(expand=c(0,0)) +
        theme_void() 
    
    # Convert the inset plot into a grob
    inset_grob <- ggplotGrob(inset_hist)
    
    # Add the inset histogram to the main plot
    gg<-gg +
        annotation_custom(
            grob = inset_grob,
            xmin = 0, xmax = max(mod_dat[[raw_var]]),  # Adjust the x-axis placement of the inset
            ymin = -Inf, ymax = 0.1  # Adjust the y-axis placement of the inset
        )
    
    return(print(gg))
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
source('scripts/fig/fig_mod2.R')
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