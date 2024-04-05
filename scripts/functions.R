source('scripts/00_plot.R')

## data cleaning functions
source('scripts/norway_clean.R')
source('scripts/read_nutrient_data.R')
lsms_read<-function(){
    source('scripts/read_lsms.R')
    output<-list( lsms_all %>% filter(!is.na(lat)), lsms_hh, lsms_fish)
    return(output)
}

## spatial processing functions
source('scripts/water_prox.R')
source('scripts/city_prox.R')


# figure functions
source('scripts/fig/figures_to_pdf.R')
source('scripts/fig/fig_fresh_contrast.R')
source('scripts/fig/fig_nutrient_density.R')
source('scripts/fig/fig_dried_rni.R')
source('scripts/fig/fig_rni_species.R')
source('scripts/fig/fig_portion_size.R')
source('scripts/fig/fig_map.R')
source('scripts/fig/fig_contaminant_species.R')
source('scripts/fig/fig_contaminant_content.R')
source('scripts/dried_fish_maps.R')
