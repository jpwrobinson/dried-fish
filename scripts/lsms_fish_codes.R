# Fish items are:


civ_fish_code<-data.frame(
    fish = c('Tilapia frais ( carpe grise importée)', 'Appolo frais (Chinchards)', 'Sardinelles fraiches', 'Autres poissons frais', 
             'Poisson fumé mangni', 'Autres Poissons fumés'),
    form = c('fresh', 'fresh', 'fresh', 'fresh', 'smoked', 'smoked'),
    s07bq01 = 35:40)


## This is same format as CIV, but different fish groups
sen_fish_code<-data.frame(
    fish = c('Poisson frais yaboye ou obo (sardinelle)','Poisson frais thiof/ seudeu (baracouda)','Poisson frais wass ',
             'Autre Poisson frais (dorade, youfouf, rouget, siket [capitaine], thiarumbekh [mollette], …..)',
             'Poisson fumé Kethiakh (sardinelle)','Autre Poisson fumé (Con fumé, yaboye ou obo fumé, …)',
             'Poisson séché','Crabes, crevettes et autres fruits de mer','Conserves de poisson '),
    form = c('fresh', 'fresh', 'fresh', 'fresh', 'smoked', 'smoked', 'dried', 'invertebrates', 'canned'),
    s07bq01 = 35:43)

nga_fish_code<-data.frame(
    fish = c('Fish - fresh', 'Fish - frozen', 'Fish - smoked', 'Fish - dried', 'Snails', 
      'Seafood (lobster, crab, prawns, etc)', 'Canned fish/seafood', 'Other fish or seafood (specify)'),
    form = c('fresh', 'frozen', 'smoked', 'dried', 'invertebrates', 'invertebrates', 'canned', 'other'),
    item_cd = 100:107
)

mal_fish_code<-data.frame(
    fish = c('Dried fish small','Dried fish medium', 'Dried fish large', 
             'Fresh fish small', 'Fresh fish medium', 'Fresh fish large',
             'Smoked fish small', 'Smoked fish medium', 'Smoked fish large'
             ),
    form = rep(c('dried', 'fresh', 'smoked'), each=3),
    hh_g02 = c(5021, 5022, 5023, 5031,5032, 5033, 5121, 5122, 5123)
)

uga_fish_1819_code<-data.frame(
    fish = c('Fresh tilapia', 'Fresh Nile perch ', 'Dry/ Smoked Tilapia', 'Dry/Smoked Nile perch ',
             'Dried Nkejje ', 'Silver Fish (Mukene) ', 'Other fresh fish ', 'Other dry/smoked fish '),
    form = c('fresh','fresh', 'dry/smoked','dry/smoked', 'dried', 'dried', 'fresh', 'dry/smoked'),
    CEB01 = c(1221, 1222, 1231, 1232, 1234, 1237, 1235, 1236) # note I have dropped underscores that are in PDF but not in dataset
)

uga_fish_1011_code<-data.frame(
    fish = c('Fresh fish', 'Dry/Smoked fish'),
    form = c('fresh', 'dry/smoked'),
    itmcd = c(122,123)
)


tza_fish_code<-data.frame(
    fish = c('Fresh fish and seafood', 'Dried/salted fish and seafood'),
    form = c('fresh','dried'),
    itemcode = c(0808, 0809)
)

tza_unit_code<-data.frame(
    hh_j02_1 = 1:5,
    unit = c('kg', 'g', 'l', 'ml', 'pieces')
)