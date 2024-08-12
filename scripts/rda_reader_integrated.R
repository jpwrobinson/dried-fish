# Script to read recommended intakes for FishNutrients shiny app
# Sept-2022, JPW Robinson


## Minerals and vitamin are RNI from Beal et al. age harmonized values
## Omega-3 intakes are adequate intakes from National Academies at https://pubmed.ncbi.nlm.nih.gov/12449285/

rda<-data.frame(nutrient = c('calcium', 'iron', 'selenium', 'zinc','iodine', 'omega_3', 'vitamin_a', 'vitamin_d', 'vitamin_b12', 'folate'))


## RNI: Recommended Nutrient Intake 
## women betwee 18-49 years, RNI per day
ca<-962.5
fe<-16
se<-55
zn<-11
omega<-1.1

i<-150

vita<-650
vitd<-15
vitb12<-2.4
folate<-330


rda$rni_women = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)

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


## children between 6 months - 5 years
ca<-538.33
fe<-7.67
se<-23.33
zn<-4.47
omega<-round((0.5*182.5 + 0.7 * 1095 + 0.9*365) / (1095+365+182.5), 2) # check with Kendra

i<-96.67

vita<-266.67
vitd<-14.17
vitb12<-0.93
folate<-120


rda$rni_kids = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)


## contaminants - Hasselberg et al. 2020 (EU limits)
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
## mug per kg body weight
lead <- NA
mercury <- 1.6 ## assuming 100% methylmercury
cadmium <- 25 
# arsenic has no limit in EU or Ghana, and WHO withdrew limit in 2011

# convert to mg
cont$ptwi<-c(lead, mercury, cadmium)/1000

## convert to body weight kg
girl_6mo<-6.2
adult<-65
cont$ptwi_child<-cont$ptwi * girl_6mo
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
    
