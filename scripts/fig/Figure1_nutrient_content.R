
## plotting args
source('scripts/00_plot.R')


portion = 12.75
pop = 'Children'

# panel a = change in nutrient content relative to fresh samples
source('scripts/fig/Figure1a_fresh_contrast.R')
# panel b = nutrient density by species and form
source('scripts/fig/Figure1b_nutrient_density.R')
# panel c = processed forms RNI radars
source('scripts/fig/Figure1c_dried_rni.R')
# sup fig = species radar plots for RNI
source('scripts/fig/FigureSX_rni_species.R')

pdf(file = 'fig/Figure1_dried_content.pdf', height =5, width=16)
print(
    plot_grid(g1A + theme(plot.margin=unit(c(1,1,1,0), 'cm')), 
              g1B + theme(plot.margin=unit(c(1,3,1,0), 'cm')), 
              g1C,
              ncol=3, labels=c('a', 'b', 'c'), rel_widths=c(1, 1, 1))
)
dev.off()

pdf(file = 'fig/FigureSX_dried_species.pdf', height =7, width=12)
print(
    gSX
)
dev.off()

pdf(file = 'fig/FigureSX_dried_forms.pdf', height =2, width=10)
print(
    gS1B
)
dev.off()


## contaminants
# panel b = processed forms contaminant radars
source('scripts/fig/Figure1_contaminant_content.R')
# sup fig = species radar plots for contaminant
source('scripts/fig/FigureSX_contaminant_species.R')

pdf(file = 'fig/Figure1_contaminant_content.pdf', height =5, width=9)
print(
    g1D
)
dev.off()

pdf(file = 'fig/FigureSX_dried_contaminant_species.pdf', height =7, width=12)
print(
    gSX
)
dev.off()