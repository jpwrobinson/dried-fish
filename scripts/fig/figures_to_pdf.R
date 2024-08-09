figs<-function(fig1a, fig1b, fig1c, fig2, fig3, figS1, figS2, figS3, figS4, figS5, figS6){

    th2<-theme(plot.margin=unit(c(1,.5,.1,0), 'cm'), axis.title = element_text(size = 10), axis.text = element_text(size = 10))
    
    pdf(file = 'fig/Figure1.pdf', height =3.5, width=12)
    print(
        plot_grid(
            fig1a + th2, 
            fig1b + th2, 
            fig1c + th2, 
            ncol=3, labels=c('a', 'b', 'c'), rel_widths=c(1, 1, 1))
    )
    dev.off()
    
    pdf(file = 'fig/Figure2.pdf', height =8, width=10)
    print(
        fig2
    )
    dev.off()
    
    pdf(file = 'fig/Figure3.pdf', height =10, width=7)
    print(
        fig3
    )
    dev.off()
    

    pdf(file = 'fig/FigureS1.pdf', height =7, width=12)
    print(
        figS1
    )
    dev.off()

    pdf(file = 'fig/FigureS2.pdf', height =3, width=12)
    print(
        figS2
    )
    dev.off()


    pdf(file = 'fig/FigureS3.pdf', height =3, width=9)
    print(
        figS3
    )
    dev.off()

    pdf(file = 'fig/FigureS4.pdf', height =3, width=9)
    print(
        figS4
    )
    dev.off()


    pdf(file = 'fig/FigureS5.pdf', height =7, width=12)
    print(
        figS5
    )
    dev.off()

    # pdf(file = 'fig/FigureS6.pdf', height =7, width=12)
    # print(
    #     figS6
    # )
    # dev.off()

}