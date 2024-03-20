
library(sf)
library(rnaturalearth)
library(tmap)

lsms<-read.csv(file = 'data/lsms_subset/lsms_all.csv') %>% 
    mutate(form2 = ifelse(form %in% c('dried', 'dry/smoked', 'smoked'), 'dried', form)) %>% 
    group_by(hh_id, form2, lat, lon, country) %>% 
    summarise(n = length(unique(form2))) 
  
# check prop dried by country  
lsms %>% 
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
    filter(ADM0_A3 %in% lsms$country)

ls_points<-lsms %>% 
    filter(!is.na(lat)) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
    st_transform(ber_proj4) 

# overlay lakes
inland <- st_read("data/population/8ark3lcpfw_GLWD_level1/glwd_1.shp",
                  crs = 4326) #4326 = WGS84

#Berhmann projection so units become meters.
inlandB <- st_transform(inland,ber_proj4)


tm_shape(w) + 
    # tm_grid() +
    tm_borders() +
    tm_facets(by = 'SUBREGION') +
    tm_shape(ls_points %>% filter(form2=='dried')) +
    tm_dots() 


