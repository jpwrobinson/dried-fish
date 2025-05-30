figs<-function(fig1a, fig1b, fig1c, fig2, fig3, fig4, figS1, figS2a, figS2b, figS3, figS4, figS5, figS6, figS7){

    th2<-theme(plot.margin=unit(c(1,.5,.1,0), 'cm'), axis.title = element_text(size = 10), axis.text = element_text(size = 10))
    
    pdf(file = 'fig/Figure1.pdf', height =3.5, width=12)
    print(
        plot_grid(
            fig1a + th2, 
            fig1b + th2, 
            fig1c + th2, 
            ncol=3, labels=c('A', 'B', 'C'), label_fontface = "plain",
            rel_widths=c(1, 1, 1))
    )
    dev.off()
    
    pdf(file = 'fig/Figure2.pdf', height =8, width=10)
    print(
        fig2
    )
    dev.off()
    
    pdf(file = 'fig/Figure3.pdf', height =3, width=8)
    print(
        fig3
    )
    dev.off()
    
    pdf(file = 'fig/Figure4.pdf', height =2.5, width=13)
    print(fig4)
    dev.off()
    

    pdf(file = 'fig/FigureS1.pdf', height =7, width=12)
    print(
        figS1
    )
    dev.off()

    pdf(file = 'fig/FigureS2.pdf', height =6, width=10)
    print(
        plot_grid(figS2a, figS2b, nrow = 2, labels=c('A', 'B'), label_fontface = "plain")
    )
    dev.off()


    pdf(file = 'fig/FigureS3.pdf', height =5, width=12)
    print(
        figS3
    )
    dev.off()

    pdf(file = 'fig/FigureS4.pdf', height =3, width=8.5)
    print(
        figS4
    )
    dev.off()


    pdf(file = 'fig/FigureS5.pdf', height =7, width=12)
    print(
        figS5
    )
    dev.off()

    

    pdf(file = 'fig/FigureS6.pdf', height =5, width=4)
    print(
        figS6
    )
    dev.off()

    
    pdf(file = 'fig/FigureS7.pdf', height =4, width=11)
    print(
        figS7
    )
    dev.off()
    
}