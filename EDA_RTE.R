# Option pour afficher en notation normale
options(scipen = 999)

#Librairies
library(ggplot2)
library(tidyverse)
library(plotly)
library(lubridate)

#importation des données
evolution_production <- read.csv2("RTE Data/Evolution production electrique.csv", header = TRUE, stringsAsFactors = FALSE, na.strings ="" )
evolution_parc <- read.csv2("RTE Data/Evolution parc.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
evolution_installation <- read.csv2("RTE Data/Partition des installation de prod hors solaire et eolien.csv",header = TRUE, stringsAsFactors = FALSE, na.strings = "")
evolution_installation_hs <- read.csv2("RTE Data/Partition des installations de prod.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

#Description des jeux de données
str(evolution_production)
str(evolution_parc)
str(evolution_installation)
str(evolution_installation_hs)

#Suppression de la colonne Nature
evolution_production$Nature <- NULL

evolution_production <- filter(evolution_production, Filière != "Production totale") #Filtre sur production totale

#Feature engineering
evolution_production$Date <- ym(evolution_production$Date)
evolution_production$Année <- year(evolution_production$Date)
evolution_production$Mois <- month(evolution_production$Date, label = TRUE, abbr = FALSE)
evolution_production$Trimestre <- paste0("T",quarter(evolution_production$Date, with_year = FALSE))
View(head(evolution_production))

prod_by_energy <- aggregate(Valeur..TWh. ~ Filière, data = evolution_production, sum) # Agrégation par filière
prod_by_energy$Filière <- reorder(prod_by_energy$Filière, prod_by_energy$Valeur..TWh.)


bar_chart_energy <- ggplot(prod_by_energy, aes(x = Valeur..TWh., y = Filière)) +
  geom_col(fill = "steelblue") +
  labs(title = "Production par filière", x = "TWh", y = NULL) +
  theme_minimal()

# 3. La commande magique pour rendre le tout interactif
ggplotly(bar_chart_energy)


production_by_month <- aggregate(Valeur..TWh. ~ Date, data = evolution_production, sum)

p2 <- ggplot(evolution_production, aes(x = Trimestre, y = Valeur..TWh., group = Année, color = as.factor(Année))) +
  geom_line() +
  labs(title = "Comparaison saisonnière par mois", x = "Mois", y = "TWh", color = "Année") +
  theme_minimal()

ggplotly(p2)
