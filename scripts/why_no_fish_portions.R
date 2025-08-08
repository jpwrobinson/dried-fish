

## We didn't use fish portions / weights. Why not?

# In CIV, units were 
# "Tas"     "Morceau" "Unité"   "Kg"      "Carton"  "Sachet" 
table(civ_fish$unit)/dim(civ_fish)[1]*100

# In SEN, units were 
# [1] "Tas"             "Unité"           "Morceau"         "Sachet"         
# [5] "Kg"              "Boite"           NA                "Boîte de tomate"
# [9] "Panier"  
table(sen_fish$unit)


# In NGA, units were 
# Piece Small, Heap Small, kg, Milk cup, g, ???, Derica small, Other, ?, Packet small, cigarette cup
table(nga_fish$unit, nga_fish$form)
table(nga_fish$unit)/dim(nga_fish)[1]*100
# >93% of obs were Heap or Piece small.


# In MAL, units were 
# [1] "HEAP LARGE"      "HEAP MEDIUM"     "HEAP SMALL"      "PIECE MEDIUM"   
# [5] "PIECE LARGE"     NA                "OTHER (SPECIFY)" "PIECE SMALL"  
unique(mal_fish$unit)
table(mal_fish$unit, mal_fish$form)
# these have unit conversions in LSMS data repo

# In UGA, (most) units were 
# 55 = whole fish (small), 56 = whole fish (medium), 57 = whole fish (large)
# 90 = heap (large), 91 = heap (medium)
table(uga_fish$unit)/dim(uga_fish)[1]*100

# In TZA, units were 
# gram or kg
table(tza_fish$unit)


