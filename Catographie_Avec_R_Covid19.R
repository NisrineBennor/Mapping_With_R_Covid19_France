#***************Carthographie sur R **************************

##########################################################
#Projet: Analyse COVID-19 en France: données des cas     #
# confirmés en le 04-03-2020 et le 25-03-2020            #
#Auteur : Nisrine Bennor                                 #
# Ingénieur en informatique biomédicale                  #
##########################################################


#-------Librarie de ce projet---------
### Données publiées par Santé Publique France sur 
##les cas confirmées d'infection au virus COVID-19, accumulées au fil des jours.
##----installer les packages -------
install.packages("tidyverse", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)
install.packages("raster",dependencies = TRUE)

install.packages("tmap")
install.packages("viridis")
install.packages("leaflet")
install.packages("mapview")
install.packages("shiny")


#................................................
####-----------chargement de librairie---------
library(tidyverse)
library(sf)
####----------Les packages de visualisation-----
library(tmap)
library(viridis)
library(leaflet)
library(mapview)
###------------library ggplot2 --------------
library(shiny)

#-------Importation du jeu de données--------
region <- read_csv2('COVID.csv')
str(region)
names(region)

#----- Renommer les noms de région avec un accent  
names(region)[2]<- "Auvergne-Rhône-Alpes"
names(region)[3]<- "Bourgogne-Franche-Comté"
names(region)[8]<- "Île-de-France"
names(region)[13]<- "Provence-Alpes-Côte d'Azur"


str(region)

### la variable date est considérée comme chiffre par défaut en R.
### le but est de faire comprendre à R que c'est une variable de type "date"
### pour celà j'utilise la fonction "as.POSIXct()" qui permet de convertir cette variable en date
### ordonée le jeu de données sur les dates

class(region$Date)

region <- region %>% 
  mutate(Date = as.POSIXct(Date)) %>%
  arrange(Date)

str(region)

## Extraction du nbre cumulé de cas confirmés 

# le Nbre cumuliers de cas 'confirmés' du covid-19
confirmes <- region[region$Date == max(region$Date),][-1]

### Retenir que les valeurs entières en convertissant ce vecteur en valeur entières 
confirmes2 <- as.integer(confirmes)

### regroupons le vecteur 'Regions' et 'confimes' dans un seul jeu de données en colonnes
covid19_region <- data.frame(Regions= as.character(names(confirmes)), confirmes = confirmes2)

# -------------- on obtient un nouveau jeu de données propre pour être manipuler --------------


####-------------Importation du fichir shapefile --------------
### ------ site : http://www.div-gis.org/gdata ----------------


unzip("regions-20180101-shp.zip")
list.files()
france <- st_read("regions-20180101.shp")

#------------Reconstitution du fichier shapefile ---------------
# dans le fichier on va utiliser 2 variables géographique 
# on crée une nouvelle variable 
# qui va contenir le nom de régions 
# restreindre le fichier 'france' au fichier 'france2'

france$ID <- france$nom

france <- france[c(7,6)]

### fusion entre les données géographiques 'france2'
### et le jeu de données 'covid19_region'
MapDataCovid <-inner_join(france, covid19_region, by=c("ID"="Regions"))

## le jeu de données 'MapDataCovid' doit être de type 'sf'
class(MapDataCovid)

str(MapDataCovid)
#------------------------visualisation des cartes ---------------------

#-----------les cartes à rond proportionnels ou les cartes statiques----------------

#librarie
install.packages("ggplot2")
library(ggplot2)



## cartes avec ggplot2
### Etape 1:Première carte basique 

ggplot(MapDataCovid) +
  aes(fill = confirmes) +
  geom_sf() 

### ajouter des cercles proportionnels aux nbre de cas confirmés 
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)

## Etape3: Modification à l'échelle gradient du remplissage des couleurs:

ggplot(MapDataCovid)+
  geom_sf(aes(fill=confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)+
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "aliceblue",
                       mid = "cadetblue3", high = "cadetblue4")

# couleur claire quand le nbre de cas confirmés est faible;
# couleur rouge sonbrequand le nombre de cas comfirmés est élevé


# Etape 4 : Modifaication de la taille des cercles proportionnelles
ggplot(MapDataCovid)+
  geom_sf(aes(fill=confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)+
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "aliceblue",
                       mid = "cadetblue3", high = "cadetblue4")+
  scale_size_area(name="confirmés", max_size = 20)


#Etape5: Ajout de titre + changement de theme
ggplot(MapDataCovid)+
  geom_sf(aes(fill=confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)+
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "aliceblue",
                       mid = "cadetblue3", high = "cadetblue4")+
  scale_size_area(name="confirmés", max_size = 20)+
  ggtitle("Nombre de cas comfirmés en france\n jusqu'à ce jour 25 Mars 2020 ")+
  theme_minimal() # theme du fond 

#Etape 6 : Ajout de l'étiquette des différentes régions administratives
ggplot(MapDataCovid)+
  geom_sf(aes(fill=confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)+
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "aliceblue",
                       mid = "cadetblue3", high = "cadetblue4")+
  scale_size_area(name="confirmés", max_size = 20)+
  ggtitle("Nombre de cas comfirmés en france\n jusqu'à ce jour 25 Mars 2020 ")+
  theme_minimal() +
  geom_sf_text(aes(label=ID), vjust=-0.5, check_overlap = T, 
               fontface="italic", colour="black")


#Etape 7: 
# ---- ajouter le nbre de cas confirmés au nom des régions 
# Région + Nbre de cas 

MapDataCovid <- MapDataCovid %>% 
  mutate(char1= as.character(ID),
         char2= as.character(confirmes),
         ID2= paste(char1, char2, sep = "\n"))

#Affichage
ggplot(MapDataCovid)+
  geom_sf(aes(fill=confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)+
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "aliceblue",
                       mid = "cadetblue3", high = "cadetblue4")+
  scale_size_area(name="confirmés", max_size = 20)+
  ggtitle("Nombre de cas comfirmés en france\n jusqu'à ce jour 25 Mars 2020 ")+
  theme_minimal() +
  geom_sf_text(aes(label=ID), vjust=-0.5, check_overlap = T, 
               fontface="italic", colour="black")



#Etape 8:Elimination des axes et de leurs étiquettes 

ggplot(MapDataCovid)+
  geom_sf(aes(fill=confirmes))+
  stat_sf_coordinates(aes(size=confirmes, fill=confirmes), color= "red",
                      shape=20, alpha=0.6)+
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "aliceblue",
                       mid = "cadetblue3", high = "cadetblue4")+
  scale_size_area(name="confirmés", max_size = 20)+
  ggtitle("Nombre de cas comfirmés en france\n jusqu'à ce jour 25 Mars 2020 ")+
  theme_minimal() +
  geom_sf_text(aes(label=ID), vjust=-0.5, check_overlap = T, 
               fontface="italic", colour="black")+
  theme(axis.title.x = element_blank(), ## supprimer l'étiquette de l'axe X
        axis.title.y = element_blank(),## supprimer l'étiquette de l'axe Y
        axis.text = element_blank(),## supprimer  les axes X et Y
        legend.position = "bottom")


#----------- Deuxième Carte (interactive) avec 'tm_shape'--------

#Library
library(tmap)# pour les cartes statiques et interavtives 

##********---------- carte Interactive 

#Etape 1: Importation de la carte de la France 
tm_shape(MapDataCovid)+
  tm_polygons()


#Etape2: Remplissage des régions selon le nbr de cas conformés
tm_shape(MapDataCovid)+
  tm_polygons("confirmes")
# Rendre interractive la Carte
tmap_mode("view")
tmap_last()


# Etape 3: Ajouter les paramètres à la carte (arguments)/ titre de la légende 
tm_shape(MapDataCovid)+
  tm_polygons("confirmes", id="ID2",
              title="Nombre de cas confirmés")


# Etape 4: Modifcation de l'échelle de remplissage des couleurs 

# Notre propre échelle 
breaks=c(0,5,10,20,40,50,60)*100
## breaks : permet de changer et de personnaliser l'échelle de remplissage des couleurs;
## en fonction du nombre des cas confirmés dans le jeu de données 

#Carte 
tm_shape(MapDataCovid)+
  tm_polygons("confirmes", id="ID2",
              title="Nombre de cas confirmés",
              breaks = breaks)


# Etape 5: Ajout de l'étiquette des noms de régions
tm_shape(MapDataCovid)+
  tm_polygons("confirmes", id="ID2",
              title="Nombre de cas confirmés",
              breaks = breaks) +
  tm_text("ID2",scale=1.3, shadow = T) # ajout des noms de région;

# Etape 6: Ajout de cercles à rond proportionnel aux nbre de cas confirmés
tm_shape(MapDataCovid)+
  tm_polygons("confirmes", id="ID2",
              title="Nombre de cas confirmés",
              breaks = breaks) +
  tm_text("ID2",scale=1.3, shadow = T)+
  tm_bubbles(size="confirmes", col = "red", alpha=.5, scale = 5, shape = 20)

  
  
  
  
  
  
  
































  









