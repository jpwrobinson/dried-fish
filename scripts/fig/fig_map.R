lsms_map_fig<-function(dat){
    
    # check prop dried by country  
    # dat %>% 
    #     group_by(country) %>% 
    #     mutate(N = n_distinct(hh_id)) %>% 
    #     group_by(country, N, form2) %>% 
    #     summarise(n = n_distinct(hh_id)) %>% 
    #     mutate(prop= n / N*100)
    
    ber_proj4 <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs'
    w<-ne_download(scale = 10, type = 'countries', category = 'cultural') %>% 
        st_as_sf() %>% 
        st_transform(ber_proj4) %>% 
        mutate(NAME = recode(NAME, 'Dem. Rep. Congo' = 'Congo DR',
                             'Guinea-Bissau' = 'Guinea Bissau'))  %>% 
        filter(CONTINENT == 'Africa' & SUBREGION!='Northern Africa')
    
    # subset LSMS countries
    w_foc<-w %>% filter(ADM0_A3 %in% dat$country)
    
    ls_points<-dat %>% 
        st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
        st_transform(ber_proj4) 
    
    # overlay lakes
    inland <- st_read("data/maps/8ark3lcpfw_GLWD_level1/glwd_1.shp",
                      crs = 4326) #4326 = WGS84

    #Berhmann projection so units become meters.
    inlandB <- st_transform(inland,ber_proj4)

    # Get marine EEZ
    w_eez<-w %>% 
        # st_union() %>% 
        # st_cast('MULTILINESTRING') %>% 
        st_buffer(dist = units::set_units(200, 'km')) %>% 
        st_union(by_feature=TRUE) %>% 
        st_cast('POLYGON')
    
    
    ## re-intersect buffer with country borders
    ## data.frame for pop merge
    marine_20k_diss<- st_difference(w_eez, w %>% st_union()) 
    
    # st_bbox(w)
    bboxer = tmaptools::bb(matrix(c(-1800000, -2500000, 4061039, 2253951),2,2))

    tm_shape(marine_20k_diss, bbox=bboxer) + tm_fill(col='#a6bddb') +
    tm_shape(w, bbox=bboxer) + 
        tm_borders(col='grey90', alpha=0.5) +
        tm_shape(w_foc) +
        tm_polygons(fill='grey90', border.col='black') +
        tm_shape(inlandB) +
        tm_polygons(col = 'lightblue', border.col='transparent') +
        tm_shape(ls_points %>% filter(form2=='dried')) +
        tm_dots( col='#b2182b', size=0.01) +
        tm_scale_bar(breaks = c(0, 100, 500), text.size = 1)
    
    print(gmap)
    
}