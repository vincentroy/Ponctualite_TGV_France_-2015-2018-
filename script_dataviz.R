library(tidyverse)
library(ggplot2)
library(RColorBrewer)

df_trains <- read.csv("regul_tgv.csv", sep = ";")

# Sélection des colonnes qui nous intéressent pour l'analyse
data_TGV <- df_trains %>%
  select(c(1:8, 10:12, 14:16))

# modification des titres de colonnes
labels <- c("annee", "mois", "service", "gare_depart", "gare_arrivee", "duree_moy_trajet", "nbre_trains_prevus", "nbre_trains_annules", "nbre_trains_retard_depart", "retard_depart_moyen_trains_en_retard", "retard_depart_moyens_tous_trains", "nbre_trains_retard_arrivee", "retard_arrivee_moyen_trains_en_retard", "retard_arrivee_moyens_tous_trains")
colnames(data_TGV) <- labels

# Passage des variables mois et année en facteur
data_TGV$annee <- factor(data_TGV$annee, ordered = T)
data_TGV$mois <- factor(data_TGV$mois, ordered = T)
levels(data_TGV$mois) <- c("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout", "septembre", "octobre", "novembre", "decembre")

# Vérification s'il y a des valeurs nulles
sum(is.na(data_TGV))

# Taux d'annulation moyen par gare de départ par an
annul_depart_par_gare <- data_TGV %>%
  group_by(gare_depart, annee) %>%
  summarise(trains_attendus = sum(nbre_trains_prevus, na.rm = T),
            trains_annules = sum(nbre_trains_annules, na.rm = T),
    taux_annul_moy = 100 * trains_annules / trains_attendus) %>%
  ungroup()

ggplot(data = annul_depart_par_gare, 
       mapping = aes(x = annee, y = gare_depart, fill = taux_annul_moy)) +
  geom_tile() +
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd")) +
  labs(title = "Taux d'annulation TGV par an et par gare de départ",
       subtitle = "Data: data.sncf.com",
       x = "Année",
       y = "Gare de départ",
       fill = "Taux d'annulation (%)")

# Voyons ce que donne le retard moyen au départ par gare
retard_moy_depart_par_gare <- data_TGV %>%
  group_by(annee, mois, gare_depart) %>%
  summarise(retard_moyen = mean(retard_depart_moyens_tous_trains))

ggplot(data = retard_moy_depart_par_gare, 
       mapping = aes(x = mois, y = gare_depart, fill = retard_moyen)) +
  geom_tile() +
  facet_wrap(~ annee, ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd")) +
  labs(title = "Retard moyen au départ par mois et par gare de départ",
       subtitle = "Data: data.sncf.com",
       x = "Mois",
       y = "Gare de départ",
       fill = "Retard moyen (en min)")

# Voyons ce que donne le retard moyen à l'arrivée par gare
retard_moy_arrivee_par_gare <- data_TGV %>%
  group_by(annee, mois, gare_arrivee) %>%
  summarise(retard_moyen = mean(retard_arrivee_moyen_trains_en_retard))

ggplot(data = retard_moy_arrivee_par_gare, 
       mapping = aes(x = mois, y = gare_arrivee, fill = retard_moyen)) +
  geom_tile() +
  facet_wrap(~ annee, ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd")) +
  labs(title = "Retard moyen à l'arrivée par mois et par gare d'arrivée",
       subtitle = "Data: data.sncf.com / Remarque : Seuls les trains en retard ont été considérés pour cette moyenne \n (elle ne concerne donc pas tous les trains qui arrivent dans cette gare)",
       x = "Mois",
       y = "Gare d'arrivée",
       fill = "Retard moyen (en min)")

# Voyons ce que donne la fréquence des arrivées en retard par gare d'arrivée
freq_retard_arrivee_par_gare <- data_TGV %>%
  group_by(annee, mois, gare_arrivee) %>%
  summarise(freq_retard = 100 * mean(nbre_trains_retard_arrivee / nbre_trains_prevus))

ggplot(data = freq_retard_arrivee_par_gare, 
       mapping = aes(x = mois, y = gare_arrivee, fill = freq_retard)) +
  geom_tile() +
  facet_wrap(~ annee, ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd")) +
  labs(title = "Fréquence des arrivées en retard par mois et par gare d'arrivée",
       subtitle = "Data: data.sncf.com",
       x = "Mois",
       y = "Gare d'arrivée",
       fill = "Fréquence de retard (%)")

# Voyons l'évolution de la fréquence des arrivées en retard par gare d'arrivée entre 2017 et 2018
freq_retard_arrivee_par_gare_1718 <- data_TGV %>%
  filter(annee == "2017" | annee == "2018") %>%
  group_by(annee, gare_arrivee) %>%
  summarise(freq_retard = 100 * mean(nbre_trains_retard_arrivee / nbre_trains_prevus))

ggplot(data = freq_retard_arrivee_par_gare_1718, 
       mapping = aes(x = freq_retard, y = gare_arrivee, colour = annee)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 20, 
             linetype = "dashed", 
             colour = "black", 
             size = 1) +
  labs(title = "Evolution de la fréquence des arrivées en retard entre 2017 et 2018, par gare d'arrivée",
       subtitle = "Data: data.sncf.com",
       x = "Fréquence de retard (%)",
       y = "Gare d'arrivée",
       fill = "Année") +
  theme_minimal() +
  geom_line(size = 1, colour = "grey70", alpha = 0.6) +
  geom_line()


## Lien à suivre https://tender-curie-5b83bc.netlify.com/2019/02/12/exploration-of-french-high-speed-trains-delays-part-2/#fnref1
## Utiliser df_3
