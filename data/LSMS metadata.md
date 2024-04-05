### LSMS metadata

#### Suitable

* Senegal = Enquête Harmonisée sur le Conditions de Vie des Ménages 2018-2019 [same values for Côte d'Ivoire]

  * fish recorded in species and processing within S7b_Conso_Al
  * GPS in grappe_gps_sen2018.csv
  * Rural / ubran is Milieu de residence (s00q04 in s00_me_sen2018.csv)
  * household expenditure is s09c_me_sen2018
* Ivory Coast = Enquête Harmonisée sur le Conditions de Vie des Ménages 2018-2019
  
  * fish recorded in species and processing within S7b_Conso_Al (codes 35-40,  39 and 40 are smoked)
  * GPS data yes
  * Rural / urban is Milieu de residence (s00q04 in s00_me_civ2018.csv)
  * household expenditure is s09c_me_civ2018.csv, sum of s09cq03 (CFA franc), for last 30 day expenditure
* Malawi = Fifth Integrated Household Survey 2019-2020
  * Fresh or dried fish (502 and 503 in G_001????)
  * GPS data in householdgeovariables_ihs5.csv but this didn't match well, so using geo data from ihs4.csv
  * No rural / urban metric
  * Household expenditure is Module I (HH_MOD_I1 for 7 days / HH_MOD_I2 for 1 month). Module J is past year. Sum of hh_i06 is value in MK (Malawian Kwacha).
* Nigeria = Living Standards Survey 2018-2019
  * fresh, frozen, smoked, dried (sect6b_food_cons)
  * Urban/rural in secta_cover.csv under SECTOR (urban = 1, rural = 2)
  * GPS not available (different households to general survey 2018-19)
  * Non-food expenditure is section 11 (sect11b_harvestw4.csv), sum of s11bq4 (in Naira currency) for all items starting with 3 (7 day recall = 1, 6 month = 4, 12 month = 5...in different sect11 csvs)
 * Uganda = National Panel Survey 2010-2011
    * fish is Fresh or Dry/Smoked (code 122?)
    * Rural / urban in geovars csv below (urban = 1, other urban = 2, rural = 3)
    * GPS is in UNPS_Geovars_112.csv
    * Note that more recent surveys exist, with better fish coverage, but no GPS data (2018-19, )
    * household expenditure is GSEC15c, the non-durable goods and frequently purchased services during last 30 days. Sum of h15cq5, value of purchases from the past. GSEC15d is expenses from past year.

* Tanzania = National Panel Survey 2014-2015

  * Fresh or dried/salted fish, including dagaa in both categories (codes 0808 and 0809)

  * GPS is in npsy4.ea file, but not provided for extended dataset

  * Rural / urban in hh_sec_a (y4_rural: urban = 0, rural = 1)

  * Monthly expenditure is hh_sec_k.csv sum of hh_k02 for items with code starting 2 [monthly]. codes starting 1 are 7 day recall, starting 3 are 12 month recall (hh_sec_l.csv)



#### Not suitable

Ghana = Socioeconomic Panel Survey: 2009-2010. 

* fish recorded, but only FISH or CANNED/TIN FISH
* other surveys from 20th century
