# Library ----

library(tidyverse)
library(sf)
library(units)


# Import des données ----

obs_loup <- read.xlsx("data/suivi_indices_2023_BZH.xlsx",
                      startRow = 1, 
                      colNames = TRUE,
                      skipEmptyRows = TRUE,
                      skipEmptyCols = TRUE) %>%
  select(-starts_with("Colonne")) %>%
  filter(AVIS == 'LOUP')

communes <- XXX

# Mise en forme de la table ----

obs_loup2 <- obs_loup %>%
  mutate(obs_loup, poils = case_when(
    Type.indice == "poils" ~ 1,
    Type.indice != "poils" ~ 0)) %>%
  mutate(obs_loup, empreinte = case_when(
    (Type.indice == "trace" | Type.indice == "empreinte") ~ 1,
    (Type.indice != "trace" & Type.indice != "empreinte") ~ 0)) %>%
  mutate(obs_loup, piege_photo = case_when(
    (Type.indice == "Piège Photo" | Type.indice == "Piège-photo") ~ 1,
    (Type.indice != "Piège Photo" & Type.indice != "Piège-photo") ~ 0)) %>%
  mutate(obs_loup, obs_visu = case_when(
    Type.indice == "obs visu" ~ 1,
    Type.indice != "obs visu" ~ 0)) %>%
  mutate(obs_loup, constat = case_when(
    Type.indice == "constat" ~ 1,
    Type.indice != "constat" ~ 0))%>%
  mutate(obs_loup, depouille = case_when(
    Type.indice == "depouille" ~ 1,
    Type.indice != "depouille" ~ 0)) %>%
  select(INSEE, nb.victime, poils, empreinte, piege_photo, obs_visu, constat, depouille) %>%
  mutate(synth_obs_loup, nb_victimes = case_when(
    nb.victime == "2 + 1" ~ '3',
    nb.victime == "4+1" ~ '5', 
    (nb.victime != "2 + 1" & nb.victime != "4+1") ~ nb.victime)) %>%
  mutate(nb_victimes  = as.numeric(nb_victimes))
  
# Assemblage des diférents tableurs ----

XXX

# Group_by commune ----

synth_obs_loup <- obs_loup2  %>%
    as.data.frame() %>%
    group_by(INSEE) %>%
    summarise(poils = sum(poils, na.rm = T),
              empreinte = sum(empreinte, na.rm = T), 
              piege_photo = sum(piege_photo, na.rm = T), 
              obs_visu = sum(obs_visu, na.rm = T), 
              constat = sum(constat, na.rm = T), 
              depouille = sum(depouille, na.rm = T),
              nb_victimes = sum(nb_victimes, na.rm = T))

  
