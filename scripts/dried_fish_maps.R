
library(sf)
library(rnaturalearth)
lsms<-read.csv(file = 'data/lsms_subset/lsms_all.csv') %>% 
    mutate(dried = ifelse(form %in% c('dried', 'dry/smoked', 'smoked'), 'dried', form)) %>% 
    group_by(hh_id, dried, lat, lon, country) %>% 
    summarise(n = length(fish)) %>% 
    pivot_wider(names_from = dried, values_from = n)


ber_proj4 <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs'
w<-ne_download(scale = 10, type = 'countries', category = 'cultural') %>% st_as_sf() %>% 
    st_transform(ber_proj4) %>% 
    mutate(NAME = recode(NAME, 'Dem. Rep. Congo' = 'Congo DR',
                         'Guinea-Bissau' = 'Guinea Bissau'))  %>% 
    filter(CONTINENT == 'Africa' & SUBREGION!='Northern Africa') %>% 
    filter(ADM0_A3 %in% lsms$country)

ggplot(w) + 
    geom_sf() +
    geom_point(data = lsms %>% filter(fresh==1), aes(y = lat, x = lon)) +
    geom_point(data = lsms %>% filter(dried==1), aes(y = lat, x = lon), col='red') +
    # facet_wrap(~country) +
    ggthemes::theme_map()

