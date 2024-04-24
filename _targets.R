library(targets)
source('scripts/functions.R')

# nutrient arguments
portionK = 6
portionW = 6
pop = 'Children'
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 'vitamin_a1', 'vitamin_d3', 'vitamin_b12')
cons<-c('lead', 'mercury', 'cadmium')

# tar_load(nut_data)
# tar_load(lsms_data)
# tar_load(lsms_proximity)

list(
    # read and clean nutrient dataset
    tar_target(meta_dry, metat_dry('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_DRIED.csv')),
    tar_target(nut_data, norway_read(path='data/norway_sep22/2022-734 downloaded 06.01.23.xlsx', 
                                     filesave='dried_nutrient_estimates',
                                     metat = meta_dry)),
    
    
    # lsms household maps
    tar_target(lsms_data, lsms_read()[[1]]),
    tar_target(lsms_water, water_prox(lsms_data)),
    tar_target(lsms_proximity, city_prox(lsms_water)),
    tar_target(lsms_save, write.csv(lsms_proximity, 'data/lsms_with_covariates.csv', row.names=FALSE)),
    tar_target(lsms_map, lsms_map_hh(dat1= lsms_data, dat2=lsms_proximity)),
    
    # modelling
    tar_target(mod_dat, mod_prep(lsms_proximity)),
    
    # figures on nutrient values

    # Figure 1
    # nutrient density by species and form
    tar_target(figND, fig_ndensity(nut_data, portion = portionK)),
    tar_target(figRNI_avg, fig_dried_rni(nut_data, portion = portionK)[[1]]),

    tar_target(figMap, lsms_map_fig(lsms_data)),
    tar_target(figMod, fig_mod(mod_dat)),

    
    # Sup Figures
    # change in nutrient content relative to fresh samples
    tar_target(figContrast, fig_fresh_contrast(nut_data)),    
    # species radar plots for RNI
    tar_target(figRNI_species, figRNI(nut_data, portion = portionK)),
    # portion size plots for RNI
    tar_target(figPortionSize, figPortion(nut_data)),
    # processed forms RNI radars
    tar_target(figRNI_forms, fig_dried_rni(nut_data, portion = portionK)[[2]]),
    # contaminant levels
    tar_target(figContam, figContaminant(nut_data, portion = portionK)),
    # contaminant levels by species
    tar_target(figContamS, figContaminant_Species(nut_data, portion = portionK)),
    
    ## compile figures
    tar_target(figAll, figs(
        fig1a = figND, 
        fig1b = figRNI_avg,
        fig2 = figMap,
        fig3 = figMod,
        figS1 = figContrast,
        figS2 = figRNI_species,
        figS3 = figPortionSize[[2]],
        figS4 = figRNI_forms,
        figS5 = figContam,
        figS6 = figContamS
    ))
    
)

# tar_manifest()
# tar_visnetwork()
# tar_load(lsms_data)



# run in sudo terminal after sourcing to replace drive figures
# cp /Users/robins64/Documents/git_repos/dried-fish/fig/Figure1.pdf /Users/robins64/My\ Drive/1_WORK/Manuscripts/dried-fish-drive/Figure1.pdf 
# cp /Users/robins64/Documents/git_repos/dried-fish/SupMat.pdf /Users/robins64/My\ Drive/1_WORK/Manuscripts/dried-fish-drive/SupMat.pdf 

