### LSMS metadata

#### Suitable

* Uganda = National Panel Survey 2010-2011

  * fish is Fresh or Dry/Smoked (code 122?)
  * Rural / urban in geovars csv below (urban = 1, other urban = 2, rural = 3)
  * GPS is in UNPS_Geovars_112.csv
  * Note that more recent surveys exist, with better fish coverage, but no GPS data
* Senegal = Enquête Harmonisée sur le Conditions de Vie des Ménages 2018-2019 [same values for Côte d'Ivoire]

  * fish recorded in species and processing within S7b_Conso_Al
  * GPS in grappe_gps_sen2018.csv
  * Rural / ubran is Milieu de residence (s00q04 in s00_me_sen2018.csv)
* Tanzania = National Panel Survey 2014-2015
  * Fresh or dried/salted fish, including dagaa in both categories (codes 0808 and 0809)
  * GPS is in npsy4.ea file, but not provided for extended dataset
  * Rural / urban in hh_sec_a (y4_rural: urban = 0, rural = 1)
* Malawi = Fifth Integrated Household Survey 2019-2020
  * Fresh or dried fish (502 and 503 in G_001????)
  * GPS data in householdgeovariables_ihs5.csv BUT DOESNT SEEM TO MATCH
  * No rural / urban metric
* Ivory Coast = Enquête Harmonisée sur le Conditions de Vie des Ménages 2018-2019
  * fish recorded in species and processing within S7b_Conso_Al (codes 35-40,  39 and 40 are smoked)
  * GPS data yes
  * Rural / urban is Milieu de residence (s00q04 in s00_me_civ2018.csv)
* Nigeria = Living Standards Survey 2018-2019
  * fresh, frozen, smoked, dried (sect6b_food_cons)
  * Urban/rural in secta_cover.csv under SECTOR (urban = 1, rural = 2)
  * GPS not available (different households to general survey 2018-19)

#### Not suitable

Ghana = Socioeconomic Panel Survey: 2009-2010. 

* fish recorded, but only FISH or CANNED/TIN FISH
* other surveys from 20th century

MALAWI - no match but ID looks same format

UGANDA - no fish data looks truncated

TANZANIA - need to find rural/urban



For missing GPS, use previous or later panels for data.