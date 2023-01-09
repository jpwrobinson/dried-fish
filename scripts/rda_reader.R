# Script to read recommended intakes for FishNutrients shiny app
# Sept-2022, JPW Robinson


## Minerals and vitamin are RNI from FAO/WHO at http://apps.who.int/iris/bitstream/handle/10665/42716/9241546123.pdf
## Omega-3 intakes are adequate intakes from National Academies at https://pubmed.ncbi.nlm.nih.gov/12449285/

rda<-data.frame(nutrient = c('calcium', 'iron', 'selenium', 'zinc','iodine', 'omega_3', 'vitamin_a', 'vitamin_d', 'vitamin_b12', 'folate'))


## RNI: Recommended Nutrient Intake 
## women betwee 18-65 years, RNI per day
ca<-1000
fe<-29.4
se<-26
zn<-4.9
omega<-1.1

i<-150

vita<-500
vitd<-5
vitb12<-2.4
folate<-400


rda$rni_women = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)

## pregnant women, RNI per day
ca<-1200
fe<-29.4
se<-29
zn<-7.5
omega<-1.4

i<-200

vita<-800
vitd<-5
vitb12<-2.6
folate<-600

rda$rni_pregnant = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)

## men betwee 18-65 years, RNI per day
ca<-1000
fe<-13.7
se<-34
zn<-7
omega<-1.6

i<-150

vita<-600
vitd<-5
vitb12<-2.4
folate<-400

rda$rni_men = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)


## children between 6 months - 5 years
ca<-511.1
fe<-6.3
se<-17.3
zn<-4.3
omega<-round((0.5*182.5 + 0.7 * 1095 + 0.9*365) / (1095+365+182.5), 2) # check with Kendra

i<-90

vita<-388.9
vitd<-5
vitb12<-round((0.7*182.5 + 0.9 * 1095 + 1.2*365) / (1095+365+182.5), 2)
folate<-round((80*182.5 + 150 * 1095 + 200*365) / (1095+365+182.5), 2)


rda$rni_kids = c(ca, fe, se, zn, i, omega, vita, vitd, vitb12, folate)
