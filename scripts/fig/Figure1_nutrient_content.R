
## plotting args
source('scripts/00_plot.R')


portion = 12.75
pop = 'Children'

# panel a = change in nutrient content relative to fresh samples
source('scripts/fig/Figure1a_fresh_contrast.R')
# panel b = processed forms RNI radars
source('scripts/fig/Figure1b_dried_rni.R')
# sup fig = species radar plots for RNI
source('scripts/fig/FigureSX_rni_species.R')

pdf(file = 'figures/Figure1_dried_content.pdf', height =5, width=9)
print(
    plot_grid(g1A + theme(plot.margin=unit(c(2,2,2,0), 'cm')), 
              g1B, ncol=2, labels=c('a', 'b'), rel_widths=c(1, 1))
)
dev.off()

pdf(file = 'figures/FigureSX_dried_species.pdf', height =7, width=12)
print(
    gSX
)
dev.off()


## contaminants
# panel b = processed forms contaminant radars
source('scripts/fig/Figure1_contaminant_content.R')
# sup fig = species radar plots for contaminant
source('scripts/fig/FigureSX_contaminant_species.R')

pdf(file = 'figures/Figure1_contaminant_content.pdf', height =5, width=9)
print(
    g1D
)
dev.off()

pdf(file = 'figures/FigureSX_dried_contaminant_species.pdf', height =7, width=12)
print(
    gSX
)
dev.off()