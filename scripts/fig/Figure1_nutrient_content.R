library(tidyverse)
library(cowplot)
library(ggradar)

## plotting args
source('scripts/theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh', 'Sun-dried', 'Smoked','Fried', 'Powder')
pcols_named<-c('Fresh' = pcols[1], 'Sun-dried' = pcols[2], 'Smoked' = pcols[3],'Fried' = pcols[4], 'Powder' = pcols[5])


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