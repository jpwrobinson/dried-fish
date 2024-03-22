water_prox<-function(data){
    
    
    ## function to calculate distance to water body
    # tar_load(lsms_data)
    # dat<-lsms_data
    
    # load maps
    ber_proj4 <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs' # Berhmann equal area
    proj <- 'ESRI:53032' ## Azimuthal Equidistant World
    
    inland <- st_read("data/maps/8ark3lcpfw_GLWD_level1/glwd_1.shp",
                      crs = "EPSG:4326") %>%  #4326 = WGS84
                st_transform(proj)
    
    marine<-ne_download(scale = 10, type = 'countries', category = 'cultural') %>% 
        st_as_sf() %>% 
        st_transform(ber_proj4) %>%
        st_union() %>% 
        st_cast('MULTILINESTRING') %>% 
        st_transform(proj) 
    
    # get all dried fish points
    ls_points<-dat %>% 
        st_as_sf(coords = c('lon', 'lat'), crs = "EPSG:4326") %>% 
        st_transform(proj) 
    
    # estimate distances between each HH and inland water bodies (in metres)
    # straight-line distance tends to be highly correlated (R > 0.91) with both walking and travel time distance (Simmance ref. 51)
    dists<-data.frame(hh = ls_points$hh_id, distance_to_marine = NA, distance_to_inland = NA)
    
    for(i in 1:dim(ls_points)[1]){
    
        # find nearest lake, and distance to that lake
        nearest<-st_nearest_feature(ls_points[i,], inland)
        di<-st_distance(ls_points[i,], inland[nearest,])
        
        # find distance to coastline
        dm<-st_distance(ls_points[i,], marine)
        
        dists$distance_to_inland[i]<-di
        dists$distance_to_marine[i]<-dm
        }
        
    
    dat<-dat %>% 
        mutate(distance_to_marine = dist_marine, distance_to_inland = dist_inland) %>% 
        rowwise() %>% 
        mutate(proximity_to_water_km = min(distance_to_marine, distance_to_inland)/1000)
    
    return(dat)
}

tm_shape(w) +
    tm_borders() +
    tm_shape(inland) +
    tm_polygons(col = 'lightblue') +
    tm_shape(ls_points[1,]) +
    tm_dots(alpha=0.5)
