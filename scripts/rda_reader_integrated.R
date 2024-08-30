# Script to read recommended intakes for dried fish analysis
# Sept-2024, JPW Robinson


## Minerals and vitamin are RNI from various sources, collated by Kendra Byrd and Lydia O'Meara
## See Table SX in dried fish project

rda<-data.frame(nutrient = c('calcium', 'iron', 'selenium', 'zinc','iodine', 'omega_3', 'vitamin_a', 'vitamin_d', 'vitamin_b12', 'folate'))


## RNI: Recommended Nutrient Intake 
## women betwee 18-49 years, RNI per day
ca<-1150
fe<-30.1
se<-55
zn<-11.4
omega<-1.1

i<-150

vita<-650
vitd<-5
vitb12<-2.4
folate<-330 # this is a placeholder as not using in dried fish analysis (not conc in fish)


rda$rni_women = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)


## children between 6 months - 5 years
ca<-450
fe<-7.5
se<-20
zn<-4.1
omega<-0.7

i<-110

vita<-250
vitd<-5
vitb12<-0.8
folate<-120 # this is a placeholder as not using in dried fish analysis (not conc in fish)


rda$rni_kids = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)

## !!! !!! !!! !!! !!! 
## Pregnant women and Men are placeholders - from harmonized guidelines.
## !!! !!! !!! !!! !!! 

## pregnant women, RNI per day
ca<-1000
fe<-16
se<-60
zn<-12.6
omega<-1.4

i<-220

vita<-700
vitd<-15
vitb12<-2.6
folate<-600


rda$rni_pregnant = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)

## men betwee 18-49 years, RNI per day
ca<-962.5
fe<-11
se<-55
zn<-14
omega<-1.1

i<-150

vita<-750
vitd<-15
vitb12<-2.4
folate<-330

rda$rni_men = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)


## contaminants - Rexsten et al. 2021
cont<-data.frame(nutrient = c('lead', 'mercury', 'cadmium'))

## these are wet weight limits. what to use for dried fish? (ie partially wet)
## mg per kg
lead <- 0.3
mercury <- 0.5 ## assuming 100% methylmercury
cadmium <- 0.05 
# arsenic has no limit in EU or Ghana, and WHO withdrew limit in 2011

cont$max_limit<-c(lead, mercury, cadmium)

## convert to mg per 100g
cont$max_limit_100g<-cont$max_limit / 10

## these are weight specific limits (permissible)
## mug per kg body weight per week (Mg) or month (Cd)
lead <- NA
mercury <- 1.6 ## assuming 100% methylmercury
cadmium <- 25 

# convert to mg
cont$ptwi<-c(lead, mercury, cadmium)/1000

## convert to body weight kg
girl_36mo<-12.8
adult<-65
cont$ptwi_child<-cont$ptwi * girl_36mo
cont$ptwi_adult<-cont$ptwi * adult


## rename to limit_100g, depending on contaminant type
if(pop == 'Children'){
cont<-cont %>% mutate(
    limit_100g = ifelse(nutrient == 'lead', max_limit_100g, ptwi_child))} else {
        if(pop == 'Adult'){
            cont<-cont %>% mutate(
                limit_100g = ifelse(nutrient == 'lead', max_limit_100g, ptwi_adult))
        }
        } 
    
