### Shiny app to viz DRIED nutrient predictions (JPWR)
# April 2022 (omega update)

## packages
library(shiny)
library(shinyjs)
library(tidyverse)
library(ggradar)

## plotting args
source('theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh','Fried', 'Powder','Smoked', 'Sun-dried')

## get RDA reference vals
source('rda_reader.R')
rda$nutrient<-str_to_title(rda$nutrient)
rda$nutrient[rda$nutrient=='Vitamin_a']<-'Vitamin A'
rda$nutrient[rda$nutrient=='Vitamin_d']<-'Vitamin D'
rda$nutrient[rda$nutrient=='Vitamin_b12']<-'Vitamin B12'
rda$nutrient[rda$nutrient=='Omega_3']<-'Omega-3 (DHA + EPA)'

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Epa_dha', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

## load data
nut_dry_whole<-read.csv('dried_nutrient_estimates_long.csv') 
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
## tidy names
nutl<-nut_dry_whole %>% 
    filter(nutrient %in% nuts) %>%
    mutate(nutrient = str_to_title(nutrient)) %>% 
    rename(species = latin_name, fbname = local_name, mu = value) %>% 
    mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                              'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Folate', 'Epa_dha'))) %>%
    mutate(nutrient = recode(nutrient, Epa_dha = 'Omega-3 (DHA + EPA)', 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) %>% 
    mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
    mutate(fbname = ifelse(species
                           == 'Encrasicholina punctifer', 'Omena (marine)', fbname),
           fbname = ifelse(species == 'Rastrineobola argenteus', 'Omena (freshwater)', fbname))

    

## units in labels
nutl$lab<-nutl$nutrient
levels(nutl$lab)<-c("'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'),
                    "'Zinc, mg'",expression('Iodine, '*mu*'g'), 
                    expression('Vitamin A, '*mu*'g'),expression('Vitamin B12, '*mu*'g'),
                    expression('Vitamin D, '*mu*'g'),expression('Folate, '*mu*'g'), "'Omega-3 (DHA+EPA), g'")

nutl_agg<-nutl %>% 
    group_by(species, fbname, nutrient, lab) %>% 
    mutate(n = length(mu)) %>% 
    group_by(form, species, fbname, n, nutrient, lab) %>% 
    summarise(mu = median(mu)) %>% 
    ungroup() %>% droplevels() %>% 
    ## add RDA and units
    left_join(rda) %>% 
    left_join(units) %>% 
    mutate(rni_women = mu/rni_women*100,
           rni_kids = mu/rni_kids*100,
           rni_men = mu/rni_men*100,
           rni_pregnant = mu/rni_pregnant*100)


# Define UI for application 
ui <- fluidPage(
    useShinyjs(),
    theme = bslib::bs_theme(bootswatch = "lux"),
    headerPanel(div("Fish nutrient content", img(src = "FishNapp_logo.png", height=98, width=130)), windowTitle = 'Fish nutrient content'),
    p('Dried fish nutrient content in Ghana & Kenya: use the drop-down box below to select species by their scientific name'),
    sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
            selectizeInput("sp", label = "Scientific name", choices = NULL, multiple=FALSE),
            selectizeInput("diet", label = "Dietary population", choices = NULL),
            sliderInput("portionWet", "Wet portion, g", value = 50, min = 10, max = 250, step=10),
            sliderInput("portionDry", "Processed portion, g", value = 50, min = 10, max = 250, step=10),
            tabPanel("Table of intakes",
                     tableOutput('rda_table')),
            h4('Background'),
        HTML(r"(
             Use the drop-down box above to select species by their scientific name. Plots will be generated for 1 species, showing nutrient content of different processing forms (e.g. fresh, smoked, sun-dried).
             <br> <br>
             Nutrient data are observed values in samples collected in Kenya and Ghana in January/February 2022. Samples were analyzed at IMR Bergen.
             <br> <br>
             Recommended intakes for minerals and vitamin are from <a href="http://apps.who.int/iris/bitstream/handle/10665/42716/9241546123.pdf" target="_blank">WHO/FAO</a>,
             assuming 10% bioavailability for iron and moderate bioavailabity for zinc.)")),
        # Main panel for displaying outputs
        mainPanel(
            tabsetPanel(
                tabPanel("Recommended intakes", downloadButton('downloadP1', 'Save as PDF'), plotOutput('spider')),
                tabPanel("Nutrient concentrations", downloadButton('downloadP2', 'Save as PDF'), plotOutput('posteriors'))
                )
        )
    )
)


server<-function(input, output, session) {

    # observeEvent(length(c(input$sp, input$name)) == 0,
    #              {shinyjs::enable("sp")})

    updateSelectizeInput(session, "diet", choices = c("Children (6mo - 5 yrs)", 'Adult women (18-65 yrs)', 'Pregnant women', 'Adult men (18-65 yrs)'), server = TRUE)
    updateSelectizeInput(session, 'sp', choices = c(nutl$species), server = TRUE)
    
    colSelect<-reactive({c('form', 'nutrient', 'rni')})
    
    nutSelect<-reactive({
        validate(need(!is.null(c(input$sp, input$name)), 'To start, type one or more scientific or common names'))
        if(is.null(input$sp)){
            req(input$name) 
            nutl$species[nutl$fbname %in% input$name]
        } else {req(input$sp) 
            input$sp}
    })
    nameSelect<-reactive({
        validate(need(!is.null(c(input$sp, input$name)), 'To start, type one or more scientific or common names'))
        if(is.null(input$name)){
            req(input$sp) 
            nutl$fbname[nutl$species %in% input$sp]
        } else {req(input$name) 
            input$name}
    })

    # frmSelect<-reactive({req(input$form) 
    #           input$form})
    rnSelect<-reactive({req(input$diet) 
              input$diet})
    ptnSelectWet<-reactive({req(input$portionWet) 
              input$portionWet})
    ptnSelectDry<-reactive({req(input$portionDry) 
        input$portionDry})

    ## outputs
    
    spiderPlot <- reactive({
        
        ## arrange data
        dat<-nutl_agg[nutl_agg$species %in% nutSelect(),] %>% 
            # filter(fbname %in% nameSelect()) %>% 
            # filter(form == frmSelect()) %>% 
            mutate(rni = case_when(str_detect(rnSelect(), 'Children') ~ rni_kids, 
                                   str_detect(rnSelect(), 'Adult women')~rni_women,
                                   str_detect(rnSelect(), 'Adult men')~rni_men,
                                   str_detect(rnSelect(), 'Pregnant')~rni_pregnant)) %>% 
            mutate(rni = ifelse(form == 'Fresh', rni / (100/ptnSelectWet()), rni / (100/ptnSelectDry()))) %>%
            mutate(rni = rni/100) %>%
            ## cap nutrient RDA at 100% (i.e. a species either meets (100%) or doesn't meet (<100%) the RDA)
            mutate(rni = case_when(rni > 1 ~ 1, TRUE ~ rni))
        
        fbname_long<-paste0(unique(dat$species), ' = ', unique(dat$fbname), collapse='\n')
    
        dat<-dat[,colSelect()]
        # dat<-dat[,c('form', 'nutrient', 'rni')]
        dat<-dat %>% pivot_wider(names_from = nutrient, values_from = rni) %>% select_if(~ !any(is.na(.))) %>% 
            arrange(form)
        
        tit<-if(str_detect(rnSelect(), 'Children')){
                    paste0('children (6 mo - 5 yrs)')} else 
                      if(str_detect(rnSelect(), 'Adult women')){
                    paste0('adult women (18-65)')} else
                      if(str_detect(rnSelect(), 'Adult men')){
                        paste0('adult men (18-65)')} else
                          if(str_detect(rnSelect(), 'Pregnant')){
                            paste0('pregnant women')}
        tit<-paste0('\n\nRecommended intakes for ', tit)
        subtit<-'\nRadar plots show the contribution of a single fish portion to recommended daily nutrient intakes (capped at 100%).'
        cap<-paste('\n\n\n', fbname_long)
        pcols_select<-pcols[which(pcols_order %in% unique(dat$form))]
        
        ggradar(dat, 
                        group.colours = pcols_select,
                        base.size = 1,
                        group.point.size = 2,
                        group.line.width = 1,
                        background.circle.colour = "white",
                        axis.label.size = 4,
                        fill=TRUE,
                        gridline.mid.colour = "grey") + 
                labs(
                    title = tit,
                    subtitle = subtit,
                    caption = cap
                    ) +
                coord_equal(clip='off') +
            theme(
                plot.title = element_text(size=14, colour='black', face=2, hjust=1),
                plot.subtitle = element_text(size=11, colour='black', face=3, hjust=1),
                plot.caption = element_text(size=12, colour='#636363', face=3),
                legend.text = element_text(size = 11))
        
    })
    
    postPlot <- reactive({
        
        dat<-nutl_agg[nutl_agg$species %in% nutSelect(),] %>% filter(!is.na(mu))
        dat2<-nutl[nutl$species %in% nutSelect(),] %>% filter(!is.na(mu))
        fbname_long<-paste0(unique(dat2$species), ' = ', unique(dat2$fbname), collapse='\n')
        cap<-paste('\n\n\n', fbname_long, '\n\n', 'Number of composite samples =',  dat$n)
            
        ggplot(dat, aes(form, mu, fill=form)) + 
            geom_bar(stat='identity', alpha=0.7) +
            geom_jitter(data=dat2, alpha=0.8, pch=21, col='black') +
            facet_wrap(~lab, scales='free', nrow=1, labeller=label_parsed) +
            labs(x = '', y = 'concentration per 100 g',caption = cap) +
            scale_fill_manual(values = pcols) +
            scale_y_continuous(expand=c(0.01,0.01)) +
            # scale_colour_manual(values = pcols) +
            theme(axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.line.x = element_blank(),
                  strip.text.x = element_text(size = 12),
                  legend.title=element_blank(),
                  plot.caption = element_text(size=12, colour='#636363', face=3),
                    plot.title = element_text(size=14, colour='black', face=2),
                  legend.text = element_text(size = 11))
    })
    
    output$spider<- renderPlot({spiderPlot()})
    output$posteriors<- renderPlot({postPlot()})
    
    output$downloadP1 <- downloadHandler(
        filename = 'Species_RNI.pdf', 
        content = function(file){
            ggsave(file, spiderPlot(), height = 5, width = 12)
        })
    
    output$posteriors<- renderPlot({postPlot()})
    output$downloadP2 <- downloadHandler(
        filename = 'Species_nutrient_concentrations.pdf', 
        content = function(file){
            ggsave(file, postPlot(), height = 7, width = 15)
        })
    
    rda_tab<-reactive({  dat<-rda %>% 
        mutate(nutrient = str_to_title(nutrient)) %>% 
        left_join(units) %>% 
        mutate(rni = case_when(str_detect(rnSelect(), 'Children') ~ rni_kids, 
                               str_detect(rnSelect(), 'Adult women')~rni_women,
                               str_detect(rnSelect(), 'Adult men')~rni_men,
                               str_detect(rnSelect(), 'Pregnant')~rni_pregnant)) %>% 
        select(nutrient, rni, unit) %>% 
        mutate(rni = round(rni, 0)) %>% 
        rename('Recommended daily intake' = rni)
    })

    # 
    # output$download <- downloadHandler(
    #     filename = "Species_Nutrient_Predictions.csv",
    #     content = function(file) {
    #         write.csv(tabber(), file)
    #     })
    # 
    output$rda_table <- renderTable({rda_tab()}, digits=0)

}

# Run the application 
shinyApp(ui = ui, server = server)

