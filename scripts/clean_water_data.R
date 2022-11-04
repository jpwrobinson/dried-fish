
setwd('dried-fish')
library(tidyverse); library(readxl); library(janitor)

fi<-read.csv('data/infoods_fish_shellfish_4.0.csv', encoding='UTF-8') %>%
		clean_names() %>%
		slice(-1) %>%
		filter(subgroup=='Finfish' & processing %in% c('r')) %>%
		select(country_region, type, processing, scientific_name, edible, water_g)

## fix ranges
## drop plus/minus values
fi$water_g<-str_split_fixed(fi$water_g, '--', '2')[,1]
## average ranges
ind<-grepl('-', fi$water_g)
rr<-as.matrix(str_split_fixed(fi$water_g, '-', '2')[ind,], nrow=2)
fi$water_g[ind]<-apply(rr, 1, function(x){mean(as.numeric(x))})
fi$water_g<-as.numeric(fi$water_g)

## get species level water means
sp<-fi %>% filter(!is.na(water_g)) %>%
		group_by(scientific_name) %>% summarise(water = mean(water_g))

ggplot(sp, aes(scientific_name, water)) + geom_point() + coord_flip()
ggplot(sp, aes(water)) + geom_histogram() 