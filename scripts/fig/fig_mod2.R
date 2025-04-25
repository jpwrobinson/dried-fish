fig_mod2<-function(dat){
    
    load(file = 'data/mod/lsms_mod.rds')
    load(file = 'data/mod/lsms_mod_fresh.rds')
    
    # proximity to water (marine)
    da<-plot_post(dried = m2, fresh = m3, mod_dat = mod_dat, var = 'Sproximity_to_marine_km', raw_var='distance_to_marine', xlab = 'Distance to marine, km')
    
    # proximity to water (inland)
    db<-plot_post(dried = m2, fresh = m3, mod_dat = mod_dat, var = 'Sproximity_to_inland_km', raw_var='distance_to_inland', xlab = 'Distance to inland, km')
    
    # proximity to city (change to hours here)
    dc<-plot_post(dried = m2, fresh = m3, mod_dat = mod_dat %>% mutate(proximity_to_city_mins = proximity_to_city_mins/60), 
                  var='Sproximity_to_city_mins', raw_var='proximity_to_city_mins', xlab='Proximity to urban centre, hours') 
    
    # household wealth
    dd<-plot_post(dried = m2, fresh = m3, mod_dat = mod_dat, var='Swealth_ppp', raw_var='log10_wealth_ppp', xlab = 'Household wealth, PPP') 
    
    # household size
    de<-plot_post(dried = m2, fresh = m3, mod_dat = mod_dat, var='Sn_hh', raw_var='n_hh', xlab='Household size')
    
    plot_grid(da, db, dc, dd, de, nrow=1, labels=c('a', 'b', 'c', 'd', 'e'))
}


fig_mod_contrast<-function(dat){
    
    load(file = 'data/mod/lsms_mod.rds')
    load(file = 'data/mod/lsms_mod_fresh.rds')
    
    # dried
    # proximity to water (marine)
    da<-plot_post_contrast(fish='dried', mod = m2, mod_dat = mod_dat, var = 'Sproximity_to_marine_km', raw_var='distance_to_marine', xlab = 'Distance to marine, km')
    
    # proximity to water (inland)
    db<-plot_post_contrast(fish='dried', mod = m2, mod_dat = mod_dat, var = 'Sproximity_to_inland_km', raw_var='distance_to_inland', xlab = 'Distance to inland, km')
    
    # proximity to city (change to hours here)
    dc<-plot_post_contrast(fish='dried', mod = m2, mod_dat = mod_dat %>% mutate(proximity_to_city_mins = proximity_to_city_mins/60), 
                  var='Sproximity_to_city_mins', raw_var='proximity_to_city_mins', xlab='Proximity to urban centre, hours') 
    
    # fresh
    # proximity to water (marine)
    da2<-plot_post_contrast(fish='fresh', mod = m3, mod_dat = mod_dat, var = 'Sproximity_to_marine_km', raw_var='distance_to_marine', xlab = 'Distance to marine, km')
    
    # proximity to water (inland)
    db2<-plot_post_contrast(fish='fresh', mod = m3, mod_dat = mod_dat, var = 'Sproximity_to_inland_km', raw_var='distance_to_inland', xlab = 'Distance to inland, km')
    
    # proximity to city (change to hours here)
    dc2<-plot_post_contrast(fish='fresh', mod = m3, mod_dat = mod_dat %>% mutate(proximity_to_city_mins = proximity_to_city_mins/60), 
                  var='Sproximity_to_city_mins', raw_var='proximity_to_city_mins', xlab='Proximity to urban centre, hours') 
    

    plot_grid(da, db, dc, da2, db2, dc2, nrow=2, 
              labels=c('a', 'b', 'c', 'd', 'e', 'f'))
}


