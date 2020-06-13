# Le but : Performer un data wrangling simplifié sur le traffic parisien  en utilisant  dplyr & plyr avec des  packages  de  R pipelines

library(dplyr)
library(tidyr)
library(plyr)


df_paris <- read.csv("Paris__Data_circulation.csv")
str(df_paris)

#Étapes
# 1.  Remplacer les cellules vides avec NA
# 2.  Supprimer les variables descriptives non vouu
# 3. Supprimer les variables avec un grand nombre des valeurs manquantes

df_paris <- df_paris %>%
              mutate_each(funs(replace(., . == "", NA))) %>%
              subset(., select = -c(INCKEY, COLDETKEY, DIAGRAMLINK, REPORTLINK, 
                                    REPORTNO, SDOTCOLNUM, SEGKEY, SPDCASENO))   %>%
              select(which(colMeans(is.na(.)) < 0.8))
dim(df_paris)

df_paris %>% tbl_df %>% glimpse()

#Séparer les données entre latitude et longitude
col_name <- 'Shape'
df_paris <- separate(df_paris, col = Shape, into = c("Latitude", "Longitude"), sep = ",")
str(df_paris)

# Supprimer '(' et ')' des latitude & longitude pour les convertir aux valeurs numériques 
df_paris$Latitude <- gsub("[[:punct:]]","", df_paris$Latitude)
df_paris$Longitude <- gsub("[[:punct:]]","", df_paris$Longitude)

df_paris$Latitude <- as.numeric(df_paris$Latitude)
df_paris$Longitude <- as.numeric(df_paris$Longitude)


#  Calcul du nombre des valeurs manquantes dans chaque valeurs 
missing_data <- df_paris %>%
                sapply(function(x) sum(is.na(x)))
View(as.data.frame(missing_data))

missing_index <- which(is.na(df_paris))
View(df_paris[missing_index, ])

# Trouver le nombre des lignes dont les valeurs sont NA
df_paris <- na.omit(df_paris)
str(df_paris)


# Données trouvés

#  Avec  summary()
summary(df_paris)

#  Résumé des variables numériques basée des catégories
summary2 <- ddply(df_paris, .(ADDRTYPE), numcolwise(median))
summary2

#Résumer en utilisant dplyr
summary3 <- df_paris %>%
            dplyr::group_by(ADDRTYPE, HITPARKEDCAR) %>%
            dplyr::summarise(Median_PersonCount = median(PERSONCOUNT),
                             Median_Distance = median(DISTANCE)) %>%
            dplyr::arrange(ADDRTYPE)
summary3
