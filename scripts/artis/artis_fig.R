# devtools::install_github("Seafood-Globalization-Lab/exploreARTIS@v1.0.0", dependencies = TRUE)
# note I had to install ggsankey manually

# loading library
library(exploreARTIS)

# Filter ARTIS data to small pelagic fishes
filter_artis(mini_artis,
             year = 2016:2020,
             exporter = "CHL",
             species = "salmo salar")