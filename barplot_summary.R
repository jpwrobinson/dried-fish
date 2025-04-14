
library(tidyverse)
theme_set(theme_classic())

pdf(file = 'fig/barplot_summary.pdf', height=5, width=9)

people<-data.frame(n = c(146, 94), cat = c('Dried', 'Fresh'))

ggplot(people, aes(cat, n)) + geom_col()

# Create a data frame for the pie chart
people2 <- data.frame(
    category = c("Dried", "Fresh", "None"),
    value = c(.36, 94/ 405.56, 0.408) # Example values
)


# Create the pie chart
ggplot(people2, aes(x = "", y = value, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +  # Remove unnecessary gridlines and axes
    theme(plot.title = element_text(hjust = 0.5)) # Center the title


beal<-data.frame(
	grams = c(22, 239, 7, 218, 145, 119, 180, 448, 463, 480),
	food = c('Dried fish', 'Dark leafy greens', 'Liver', 'Crustaceans','Goat', 'Beef','Eggs', 'Milk', 'Fresh fish', 'Chicken')
	)

ggplot(beal, aes(fct_reorder(food, -grams), grams)) + 
    geom_col() + coord_flip() +
    geom_text(aes(label = grams), hjust=-.2, size=3) +
    scale_y_continuous(expand=c(0,0.05), limits=c(0, 600)) + labs(x = '')

dev.off()

