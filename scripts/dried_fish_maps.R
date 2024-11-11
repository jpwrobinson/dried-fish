lsms_map_hh<-function(dat1, dat2){
    
    ber_proj4 <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs'
    w<-ne_download(scale = 10, type = 'countries', category = 'cultural') %>% 
        st_as_sf() %>% 
        st_transform(ber_proj4) %>% 
        mutate(NAME = recode(NAME, 'Dem. Rep. Congo' = 'Congo DR',
                             'Guinea-Bissau' = 'Guinea Bissau'))  %>% 
        filter(CONTINENT == 'Africa' & SUBREGION!='Northern Africa') %>% 
        filter(ADM0_A3 %in% dat1$country)
    
    ls_points<-dat1 %>% 
        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
        st_transform(ber_proj4) 
    
    ls_points<-dat2 %>% 
        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
        st_transform(ber_proj4) 
    
    # overlay lakes
    inland <- st_read("data/maps/8ark3lcpfw_GLWD_level1/glwd_1.shp",
                      crs = 4326) #4326 = WGS84
    
    #Berhmann projection so units become meters.
    inlandB <- st_transform(inland,ber_proj4)
    
    base<-tm_shape(w) + 
                   tm_borders(col='black') +
                   tm_facets(by = 'SUBREGION') +
                   tm_shape(inlandB) +
                   tm_polygons(col = 'lightblue')
    
    g1<-base +
        tm_shape(ls_points %>% filter(dried=='no')) + 
        tm_dots(alpha=0.5) +
        tm_shape(ls_points %>% filter(dried == 'yes')) +
        tm_dots( col='red', size=0.01) +
        tm_layout(main.title = 'Households consuming dried fish (red), over all households surveyed (black)')
    
    g2<-base +
        tm_shape(ls_points) + 
        tm_dots(alpha=0.5) +
        tm_layout(main.title = 'Households surveyed by LSMS')
    

    pdf(file = 'fig/map_lsms_household_surveys.pdf', height=7, width=12)
    print(g1)
    print(g2)
    dev.off()

    g3a<-ggplot(dat2, 
                aes(distance_to_marine, distance_to_inland, col=nearest_water)) + geom_point()
    
    g3<-base +
        tm_shape(ls_points) + 
        tm_dots(col='nearest_water', alpha=0.5, palette=realm_cols)
    
    g4<-base +
        tm_shape(ls_points) +
        tm_dots(col = 'proximity_to_city_mins', palette="-RdYlBu")
    
    g5<-base +
        tm_shape(ls_points) +
        tm_dots(col = 'distance_to_inland', palette="-RdYlBu")
    
    g6<-base +
        tm_shape(ls_points) +
        tm_dots(col = 'distance_to_marine', palette="-RdYlBu")
    
    pdf(file = 'fig/map_lsms_proximity_covariates.pdf', height=7, width=12)
    print(g3a)
    print(g3)
    print(g4)
    print(g5)
    print(g6)
    dev.off()
    
    
}


