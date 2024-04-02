figs<-function(fig1a, fig1b, figS1, figS2, figS3, figS4, figS5, figS6){

    pdf(file = 'fig/Figure1.pdf', height =5, width=10)
    print(
        plot_grid(
            fig1a + theme(plot.margin=unit(c(1,3,1,0), 'cm')), 
            fig1b,
            ncol=2, labels=c('a', 'b'), rel_widths=c(1, 1))
    )
    dev.off()
    
    pdf(file = 'fig/Figure2.pdf', height =8, width=10)
    print(
        gmap
    )
    dev.off()

    pdf(file = 'fig/FigureS1.pdf', height =5, width=8)
    print(
        figS1
    )
    dev.off()

    pdf(file = 'fig/FigureS2.pdf', height =7, width=12)
    print(
        figS2
    )
    dev.off()


    pdf(file = 'fig/FigureS3.pdf', height =3, width=17)
    print(
        figS3
    )
    dev.off()

    pdf(file = 'fig/FigureS4.pdf', height =3, width=9)
    print(
        figS4
    )
    dev.off()


    pdf(file = 'fig/FigureS5.pdf', height =3, width=9)
    print(
        figS5
    )
    dev.off()

    pdf(file = 'fig/FigureS6.pdf', height =7, width=12)
    print(
        figS6
    )
    dev.off()

}