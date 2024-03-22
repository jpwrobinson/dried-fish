
# tar_load(lsms_data)
# dat<-lsms_data

# Travel time in minutes to nearest city in 2015, estimated by Weiss et al. 2018 Nature and downloaded from:
# https://resourcewatch.org/data/explore/cit01701-Travel-Time-to-Major-Cities?section=Discover&selectedCollection=&zoom=3&lat=0&lng=0&pitch=0&bearing=0&basemap=dark&labels=light&layers=%255B%257B%2522dataset%2522%253A%2522ccbcaf7b-1619-4298-8275-b135d1e8e04e%2522%252C%2522opacity%2522%253A1%252C%2522layer%2522%253A%2522d787d894-f7af-47c4-af0f-0849b06686ee%2522%257D%255D&aoi=&page=1&sort=most-viewed&sortDirection=-1
# Oxford website was down (https://map.ox.ac.uk/research-project/accessibility_to_cities/)

city_prox<-function(dat){
	# raster format with a spatial resolution of 30 arc seconds (around 1 km at the equator)
	cit<-terra::rast('data/maps/cit_017_accessibility_to_cities/accessibility_to_cities_2015_v1.0.tif')


	ber_proj4 <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs' # Berhmann equal area
	proj <- 'ESRI:53032' ## Azimuthal Equidistant World


	ls_points<-dat %>% 
	        filter(!is.na(lat) & form2 == 'dried') %>% 
	        st_as_sf(coords = c('lon', 'lat'), crs = "EPSG:4326") %>% 
	        st_transform(st_crs(cit))

	# extract travel time for each household
	city_prox<-terra::extract(cit, ls_points)
	ls_points$proximity_to_city_mins<-city_prox$accessibility_to_cities_2015_v1.0
	dat$proximity_to_city_mins<-city_prox$accessibility_to_cities_2015_v1.0

	# note that 38 points do not fall on cities raster (coastal households)
	# replace these households with their nearest household that has a positive city proximity
	ls_points2<-ls_points %>% filter(proximity_to_city_mins < 0)

	nearest<-st_nearest_feature(ls_points2, 
								ls_points %>% filter(proximity_to_city_mins > 0))

	ls_points2$geometry<-ls_points[nearest,'geometry']$geometry
	city_prox2<-terra::extract(cit, ls_points2)

	ls_points$proximity_to_city_mins[ls_points$proximity_to_city_mins<0]<-city_prox2$accessibility_to_cities_2015_v1.0
	dat$proximity_to_city_mins[dat$proximity_to_city_mins<0]<-city_prox2$accessibility_to_cities_2015_v1.0

	return(dat)
}


# # plotting and exploring results
# # with google earth and visual cities layer online
# # looks good
# r <- clamp(cit, 0, 500) 
# pal <- colorRampPalette(c("green","red"))
# plot(cit, xlim = c(39, 40), ylim = c(-6, -5), col=pal(10))
# points(dat$lon, dat$lat, col='black')
# dat %>% filter(lon > 39.2 & lat >-5.5)