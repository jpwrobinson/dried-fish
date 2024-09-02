library(targets)

# nutrient arguments
portionK = 9
portionW = 41
pop = 'Children'
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 'vitamin_a1', 'vitamin_d3', 'vitamin_b12')
cons<-c('lead', 'mercury', 'cadmium')

source('scripts/functions.R')

# tar_load(nut_data)
# tar_load(lsms_data)
# tar_load(lsms_proximity)
# tar_load(mod_dat)

list(
    # read and clean nutrient dataset
    tar_target(meta_dry, metat_dry('data/sample_metadata/all_dried_fish_samples.csv')),
    tar_target(nut_data, norway_read(path1='data/norway_results/2022-734 downloaded 06.01.23.xlsx', 
                                     path2='data/norway_results/2023-1200 Lancaster_James_25.06.2024_final results.xlsx', 
                                     filesave='dried_nutrient_estimates',
                                     metat = meta_dry)),
    
    
    # lsms extract consumption and proximity covariates
    tar_target(lsms_data, lsms_read()[[3]]),
    tar_target(lsms_water, water_prox(lsms_data)),
    tar_target(lsms_proximity, city_prox(lsms_water)),
    tar_target(lsms_save, write.csv(lsms_proximity, 'data/lsms_with_covariates.csv', row.names=FALSE)),
    
    # modelling
    tar_target(mod_dat, mod_prep(lsms_proximity)),
    
    # household maps
    tar_target(lsms_map, lsms_map_hh(dat1= lsms_data, dat2=lsms_proximity, dat3 = mod_dat)),
    
    # figures on nutrient values

    # Figure 1
    # nutrient density by species and form
    tar_target(figND, fig_ndensity(nut_data, portion = portionK)),
    tar_target(figRNI_avg, figPortion(nut_data)[[3]]),
    # tar_target(figRNI_avg, fig_dried_rni(nut_data, portion = portionK)[[1]]),

    tar_target(figMap, lsms_map_fig(lsms_data)),
    tar_target(figPost, fig_post(mod_dat)),
    tar_target(figMod, fig_mod(mod_dat, model='dried')),
    tar_target(figModF, fig_mod(mod_dat, model='fresh')),
    tar_target(figHeat, fig_heat(mod_dat)),

    
    # Sup Figures
    # change in nutrient content relative to fresh samples
    tar_target(figContrast, fig_fresh_contrast(nut_data)),    
    # species radar plots for RNI
    tar_target(figRNI_species, figRNI(nut_data, portion = portionK)),
    # portion size plots for RNI
    tar_target(figPortionSize, figPortion(nut_data)[[2]]),
    tar_target(figPortionDat, figPortion(nut_data)[[4]]),
    # processed forms RNI radars
    tar_target(figRNI_forms, fig_dried_rni(nut_data, portion = portionK)[[2]]),
    # contaminant levels
    # tar_target(figContam, figContaminant(nut_data, portion = portionK)),
    tar_target(figContam, figContaminantPortion(nut_data, portion = portionK)),
    # contaminant levels by species
    tar_target(figContamS, figContaminant_Species(nut_data, portion = portionK)),
    
    ## compile figures
    tar_target(figAll, figs(
        fig1a = figND, 
        fig1b = figRNI_avg,
        fig1c = figContrast,
        fig2 = figMap,
        fig3 = figPost,
        fig4a = figMod,
        fig4b = figModF,
        figS1 = figRNI_species,
        figS2 = figPortionSize,
        figS3 = figRNI_forms,
        figS4 = figContam,
        figS5 = figContamS,
        figS6 = figHeat
    ))
    
)

# tar_manifest()
# tar_visnetwork()
# tar_make()



# run in sudo terminal after sourcing to replace drive figures
# cp /Users/robins64/Documents/git_repos/dried-fish/fig/Figure1.pdf /Users/robins64/My\ Drive/1_WORK/Manuscripts/dried-fish-drive/Figure1.pdf 
# cp /Users/robins64/Documents/git_repos/dried-fish/SupMat.pdf /Users/robins64/My\ Drive/1_WORK/Manuscripts/dried-fish-drive/SupMat.pdf 

