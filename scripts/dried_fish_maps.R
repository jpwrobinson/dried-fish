lsms_map_hh<-function(dat){

    hh<-read.csv(file = 'data/lsms_subset/lsms_all_hh.csv')
      
    # check prop dried by country  
    dat %>% 
        group_by(country) %>% 
        mutate(N = n_distinct(hh_id)) %>% 
        group_by(country, N, form2) %>% 
        summarise(n = n_distinct(hh_id)) %>% 
        mutate(prop= n / N*100)
    
    ber_proj4 <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs'
    w<-ne_download(scale = 10, type = 'countries', category = 'cultural') %>% 
        st_as_sf() %>% 
        st_transform(ber_proj4) %>% 
        mutate(NAME = recode(NAME, 'Dem. Rep. Congo' = 'Congo DR',
                             'Guinea-Bissau' = 'Guinea Bissau'))  %>% 
        filter(CONTINENT == 'Africa' & SUBREGION!='Northern Africa') %>% 
        filter(ADM0_A3 %in% dat$country)
    
    ls_points<-dat %>% 
        filter(!is.na(lat) & form2 == 'dried') %>% 
        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
        st_transform(ber_proj4) 
    
    hh_points<-hh %>% 
        filter(!is.na(lat)) %>% 
        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
        st_transform(ber_proj4) 
    
    # overlay lakes
    inland <- st_read("data/maps/8ark3lcpfw_GLWD_level1/glwd_1.shp",
                      crs = 4326) #4326 = WGS84
    
    #Berhmann projection so units become meters.
    inlandB <- st_transform(inland,ber_proj4)
    
    
    g1<-tm_shape(w) + 
        # tm_grid() +
        tm_borders() +
        tm_facets(by = 'SUBREGION') +
        tm_shape(inlandB) +
        tm_polygons(col = 'lightblue') +
        tm_shape(hh_points %>% filter(!hh_id %in% ls_points$hh_id)) + 
        tm_dots(alpha=0.5) +
        tm_shape(ls_points %>% filter(form2=='dried')) +
        tm_dots( col='red', size=0.01) +
        tm_layout(main.title = 'Households consuming dried fish (red), over all households surveyed (black)')
    
    g2<-tm_shape(w) + 
        # tm_grid() +
        tm_borders() +
        tm_facets(by = 'SUBREGION') +
        tm_shape(inlandB) +
        tm_polygons(col = 'lightblue') +
        tm_shape(hh_points) + 
        tm_dots(alpha=0.5) +
        tm_layout(main.title = 'Households surveyed by LSMS')
    
    pdf(file = 'fig/map/lsms_household_surveys.pdf', height=7, width=12)
    print(g1)
    print(g2)
    dev.off()
}