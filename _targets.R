library(targets)
source('scripts/00_plot.R')
source('scripts/norway_clean.R')
source('scripts/read_nutrient_data.R')

portion = 6
pop = 'Children'
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 'vitamin_a1', 'vitamin_d3', 'vitamin_b12')
cons<-c('lead', 'mercury', 'cadmium')

# figure functions
source('scripts/fig/Figure1a_fresh_contrast.R')
source('scripts/fig/Figure1b_nutrient_density.R')
source('scripts/fig/Figure1c_dried_rni.R')
source('scripts/fig/FigureSX_rni_species.R')
source('scripts/fig/FigureSX_portion_size.R')

list(
    # read and clean nutrient dataset
    tar_target(meta_dry, metat_dry('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_DRIED.csv')),
    tar_target(nut_data, norway_read(path='data/norway_sep22/2022-734 downloaded 06.01.23.xlsx', 
                                     filesave='dried_nutrient_estimates',
                                     metat = meta_dry)),
    
    # figures on nutrient values
    # panel a = change in nutrient content relative to fresh samples
    tar_target(fig1a, fig1a(nut_data)),
    # panel b = nutrient density by species and form
    tar_target(fig1b, fig1b(nut_data)),
    # panel c = processed forms RNI radars
    tar_target(fig1c, fig1c(nut_data)),
    
    # sup fig = species radar plots for RNI
    tar_target(figRNI, figRNI(nut_data)),
    # sup fig = portion size plots for RNI
    tar_target(figPortion, figPortion(nut_data))
    
    
    
    
)

# tar_manifest()
# tar_visnetwork()



# run in terminal after sourcing to replace drive figures
# cp /Users/robins64/Documents/git_repos/dried-fish/fig/Figure1_dried_content.pdf /Users/robins64/Google\ Drive/1_WORK/Manuscripts/dried-fish-drive/Figure1.pdf 
# cp /Users/robins64/Documents/git_repos/dried-fish/fig/FigureSX_dried_portion_source.pdf /Users/robins64/Google\ Drive/1_WORK/Manuscripts/dried-fish-drive/Figure2.pdf 
# cp /Users/robins64/Documents/git_repos/dried-fish/SupMat.pdf /Users/robins64/Google\ Drive/1_WORK/Manuscripts/dried-fish-drive/SupMat.pdf 