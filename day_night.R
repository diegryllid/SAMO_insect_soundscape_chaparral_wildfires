
library(readr)
library(scales)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(stringr)

tz <- "America/Los_Angeles"

#read and categorize by night/day
full_df <- read_csv("D:/R_project/full_data/full_variables_df.csv") 

#community analyses

annotation_df <-
read_excel("D:/R_project/full_data/Annotation.xlsx", sheet = "Sheet1") %>%
  as.data.frame() %>% 
  replace(is.na(.), 0)
  
  ann_appearances_sums<-
 annotation_df %>% 
    summarize(
    O.californicus = sum(O.californicus),
    O.rileyi = sum(O.rileyi),
    O.quadripunctatus = sum(O.quadripunctatus),
    G.saxatilis = sum(G.saxatilis),
    G.veletisoides = sum(G.veletisoides),
    P.californica = sum(P.californica),
    A.longipennis = sum(A.longipennis),
    A.diminutiva_dactyla = sum(A.diminutiva_dactyla),
    A.diminutiva_malibu = sum(A.diminutiva_malibu),
    A.morsei_tecnitota = sum(A.morsei_tecnitota),
    A.morsei_curtatus = sum(A.morsei_curtatus),
    katydid6 = sum(katydid6),
    S.furcata = sum(S.furcata),
    C.bruneri = sum(C.bruneri),
    katydid10 = sum(katydid10),
    katydid11 = sum(katydid11),
    katydid12 = sum(katydid12),
    katydid13 = sum(katydid13),
    katydid14 = sum(katydid14),
    katydid15 = sum(katydid15),
  .by = Recorder) %>% 
  mutate(cricket_total_app=rowSums(
    select(.,
           O.californicus, O.quadripunctatus,
           O.rileyi, G.saxatilis, G.veletisoides)),
    katydid_total_app = 
      rowSums(
        select(.,
               P.californica, A.longipennis, A.diminutiva_dactyla,
               A.diminutiva_malibu, A.morsei_tecnitota, A.morsei_curtatus,
               katydid6, C.bruneri, S.furcata, katydid10,
               katydid11, katydid12, katydid13, katydid14, katydid15)))
  
  recorders_by_locality<-
    ann_appearances_sums %>% 
    left_join(
      full_df %>% 
        filter(
          season == "summer",
          day_night == "night"
        ) %>% 
        select(locality, recorder_id, last_fire) %>% 
        rename(Recorder = recorder_id) %>% 
        unique()) %>% 
    dplyr::select(locality, last_fire, Recorder)

ann_appearances_relativebygroup<-
ann_appearances_sums  %>%
  mutate(
    O.californicus = O.californicus/cricket_total_app,
    O.rileyi = O.rileyi/cricket_total_app,
    O.quadripunctatus = O.quadripunctatus/cricket_total_app,
    G.saxatilis = G.saxatilis/cricket_total_app,
    G.veletisoides = G.veletisoides/cricket_total_app,
    P.californica = P.californica/katydid_total_app,
    A.longipennis = A.longipennis/katydid_total_app,
    A.diminutiva_dactyla = A.diminutiva_dactyla/katydid_total_app,
    A.diminutiva_malibu = A.diminutiva_malibu/katydid_total_app,
    A.morsei_tecnitota = A.morsei_tecnitota/katydid_total_app,
    A.morsei_curtatus = A.morsei_curtatus/katydid_total_app,
    katydid6 = katydid6/katydid_total_app,
    S.furcata = S.furcata/katydid_total_app,
    C.bruneri = C.bruneri/katydid_total_app,
    katydid10 = katydid10/katydid_total_app,
    katydid11 = katydid11/katydid_total_app,
    katydid12 = katydid12/katydid_total_app,
    katydid13 = katydid13/katydid_total_app,
    katydid14 = katydid14/katydid_total_app,
    katydid15 = katydid15/katydid_total_app)

ann_appearances_relativeaccross<-
  ann_appearances_sums  %>%
  mutate(
    O.californicus = O.californicus/(cricket_total_app + katydid_total_app),
    O.rileyi = O.rileyi/(cricket_total_app + katydid_total_app),
    O.quadripunctatus = O.quadripunctatus/(cricket_total_app + katydid_total_app),
    G.saxatilis = G.saxatilis/(cricket_total_app + katydid_total_app),
    G.veletisoides = G.veletisoides/(cricket_total_app + katydid_total_app),
    P.californica = P.californica/(katydid_total_app + cricket_total_app),
    A.longipennis = A.longipennis/(katydid_total_app + cricket_total_app),
    A.diminutiva_dactyla = A.diminutiva_dactyla/(katydid_total_app + cricket_total_app),
    A.diminutiva_malibu = A.diminutiva_malibu/(katydid_total_app + cricket_total_app),
    A.morsei_tecnitota = A.morsei_tecnitota/(katydid_total_app + cricket_total_app),
    A.morsei_curtatus = A.morsei_curtatus/(katydid_total_app + cricket_total_app),
    katydid6 = katydid6/(katydid_total_app + cricket_total_app),
    S.furcata = S.furcata/(katydid_total_app + cricket_total_app),
    C.bruneri = C.bruneri/(katydid_total_app + cricket_total_app),
    katydid10 = katydid10/(katydid_total_app + cricket_total_app),
    katydid11 = katydid11/(katydid_total_app + cricket_total_app),
    katydid12 = katydid12/(katydid_total_app + cricket_total_app),
    katydid13 = katydid13/(katydid_total_app + cricket_total_app),
    katydid14 = katydid14/(katydid_total_app + cricket_total_app),
    katydid15 = katydid15/(katydid_total_app + cricket_total_app))

ann_appearances_means_byfire <- ann_appearances_sums  %>%
  left_join(recorders_by_locality) %>% 
  group_by(locality, last_fire) %>% 
  summarize_all(mean) %>% 
  select(!c(Recorder, cricket_total_app, katydid_total_app))

ann_appearances_sums_byfire<-
  ann_appearances_sums  %>%
  left_join(recorders_by_locality) %>% 
  group_by(locality, last_fire) %>% 
  summarize_all(sum) %>% 
  select(!c(Recorder, cricket_total_app, katydid_total_app))

ann_appearances_relativeaccross_byfire<-
  ann_appearances_sums  %>%
  left_join(recorders_by_locality) %>% 
  group_by(locality, last_fire) %>% 
  summarize_all(sum) %>% 
  mutate(
    O.californicus = O.californicus/(cricket_total_app + katydid_total_app),
    O.rileyi = O.rileyi/(cricket_total_app + katydid_total_app),
    O.quadripunctatus = O.quadripunctatus/(cricket_total_app + katydid_total_app),
    G.saxatilis = G.saxatilis/(cricket_total_app + katydid_total_app),
    G.veletisoides = G.veletisoides/(cricket_total_app + katydid_total_app),
    P.californica = P.californica/(katydid_total_app + cricket_total_app),
    A.longipennis = A.longipennis/(katydid_total_app + cricket_total_app),
    A.diminutiva_dactyla = A.diminutiva_dactyla/(katydid_total_app + cricket_total_app),
    A.diminutiva_malibu = A.diminutiva_malibu/(katydid_total_app + cricket_total_app),
    A.morsei_tecnitota = A.morsei_tecnitota/(katydid_total_app + cricket_total_app),
    A.morsei_curtatus = A.morsei_curtatus/(katydid_total_app + cricket_total_app),
    katydid6 = katydid6/(katydid_total_app + cricket_total_app),
    S.furcata = S.furcata/(katydid_total_app + cricket_total_app),
    C.bruneri = C.bruneri/(katydid_total_app + cricket_total_app),
    katydid10 = katydid10/(katydid_total_app + cricket_total_app),
    katydid11 = katydid11/(katydid_total_app + cricket_total_app),
    katydid12 = katydid12/(katydid_total_app + cricket_total_app),
    katydid13 = katydid13/(katydid_total_app + cricket_total_app),
    katydid14 = katydid14/(katydid_total_app + cricket_total_app),
    katydid15 = katydid15/(katydid_total_app + cricket_total_app)) %>% 
  select(!c(Recorder, cricket_total_app, katydid_total_app))

ann_specieslist <-
  ann_appearances_sums %>% 
  left_join(
    full_df %>% 
      filter(
        season == "summer",
        day_night == "night"
      ) %>% 
      select(locality, recorder_id, last_fire) %>% 
      rename(Recorder = recorder_id) %>% 
      unique()) %>% 
  group_by(locality, last_fire) %>% 
  summarize_all(sum) %>% 
  select(!c(Recorder, cricket_total_app, katydid_total_app)) %>% 
  mutate(across(O.californicus:katydid15, ~case_when(. >= 1 ~ cur_column()), .names = 'new_{col}')) %>%
  unite(species_list, starts_with('new'), na.rm = TRUE, sep = ' ') %>% 
  select(locality, last_fire, species_list)
  
ann_presences <-
  annotation_df %>% 
  summarize(
    cricket_avg_richness = mean(crickets),
    katydid_avg_richness = mean(katydids),
    mean_richness = mean(crickets)+mean(katydids),
    O.californicus = sum(O.californicus),
    O.rileyi = sum(O.rileyi),
    O.quadripunctatus = sum(O.quadripunctatus),
    G.saxatilis = sum(G.saxatilis),
    G.veletisoides = sum(G.veletisoides),
    P.californica = sum(P.californica),
    A.longipennis = sum(A.longipennis),
    A.diminutiva_dactyla = sum(A.diminutiva_dactyla),
    A.diminutiva_malibu = sum(A.diminutiva_malibu),
    A.morsei_tecnitota = sum(A.morsei_tecnitota),
    A.morsei_curtatus = sum(A.morsei_curtatus),
    katydid6 = sum(katydid6),
    S.furcata = sum(S.furcata),
    C.bruneri = sum(C.bruneri),
    katydid10 = sum(katydid10),
    katydid11 = sum(katydid11),
    katydid12 = sum(katydid12),
    katydid13 = sum(katydid13),
    katydid14 = sum(katydid14),
    katydid15 = sum(katydid15),
    .by = Recorder) %>% 
  mutate(cricket_richness=rowSums(
    select(.,
           O.californicus, O.rileyi, G.saxatilis, G.veletisoides)!=0),
    katydid_richness = 
      rowSums(
        select(.,
               P.californica, A.longipennis, A.diminutiva_dactyla,
               A.diminutiva_malibu, A.morsei_tecnitota, A.morsei_curtatus,
               katydid6, C.bruneri)!=0)) %>% 
  mutate(total_richness = cricket_richness + katydid_richness) %>% 
  rename(recorder_id = Recorder)

##species histogram PICK MOST COMMON SPECIES IN THE SAME ORDER

total_appearances<-
ann_appearances_sums %>% 
  left_join(recorders_by_locality) %>% 
  select(!c(cricket_total_app, katydid_total_app)) %>% 
  
  filter(locality == "topanga", last_fire == 1900) %>% 
  select(!c(Recorder, locality, last_fire)) %>% 
  colSums() %>% as.list() %>%  as.data.frame() %>%
  pivot_longer(O.californicus:katydid15, names_to = "species", values_to = "appearances") %>% 
  filter(appearances != 0) %>%  arrange()

##$###

#alluvial diagram
library(ggalluvial)
library(ggrepel)

#rancho
# flow_df<-
# ann_appearances_means_byfire %>% 
#   filter(locality == "rancho") %>% 
#   select(!c(locality)) %>% 
#   pivot_longer("O.californicus":katydid15, names_to = "species",
#                values_to = "appearances") %>% 
#  mutate(species = factor(species, levels = unique(species)))
# 
# rancho_flow_plot <-
# flow_df %>% 
# ggplot(aes(x = last_fire, y = appearances, fill = species)) +
#   geom_flow(aes(alluvium = species, stratum = species), alpha = .7, color = "black",
#             width = 0, curve_type = "cubic") +
#   geom_vline(xintercept = ann_appearances_means_byfire %>% 
#                filter(locality == "rancho") %>% 
#                pull(last_fire),
#              linetype = 2,
#              linewidth = 0.5)+
#   theme_minimal() +
#   geom_text_repel(data = flow_df %>% filter(last_fire == max(last_fire)),
#     aes(stratum = species, label = species),stat = "stratum", size = 4,
#     nudge_x = -25, direction = "y", fontface = "italic")+
#   geom_text_repel(data = flow_df %>% filter(last_fire == setdiff(min(last_fire),
#                                                                  max(last_fire))),
#             aes(stratum = species, label = species),stat = "stratum", size = 4,
#             nudge_x = 25, direction = "y", fontface = "italic")+
#   theme(plot.margin = unit(c(0.1, 3, 0.1, 0.1), 
#                            "inches"),
#     panel.grid.minor = element_line(size = 0.25,
#                                         linetype = 1),
#         panel.grid.major = element_line(color = "gray",
#                                         size = 0.25,
#                                         linetype = 1),
#         legend.position = "none",
#     axis.text.x=element_text(size=14),
#     axis.text.y=element_text(size=14))+
#   scale_x_reverse(breaks = ann_appearances_means_byfire %>% 
#                     filter(locality == "rancho") %>%
#                     filter(last_fire != 2018) %>% 
#                     pull(last_fire),
#                   minor_breaks = seq(1990, 2020, 5),
#                   expand = c(.445, 0)) +
#   labs(x =NULL, y = NULL)
# 
# #eleanor
# flow_df<-
#   ann_appearances_means_byfire %>% 
#   filter(locality == "eleanor") %>% 
#   select(!c(locality)) %>% 
#   pivot_longer("O.californicus":katydid15, names_to = "species",
#                values_to = "appearances") %>% 
#   mutate(species = factor(species, levels = unique(species)))
# 
# eleanor_flow_plot <-
# flow_df %>% 
#   ggplot(aes(x = last_fire, y = appearances, fill = species)) +
#   geom_flow(aes(alluvium = species, stratum = species), alpha = .7, color = "black", width = 0,
#             curve_type = "cubic") +
#   geom_vline(xintercept = ann_appearances_means_byfire %>% 
#                filter(locality == "eleanor") %>% 
#                pull(last_fire),
#              linetype = 2,
#              linewidth = 0.5)+
#   theme_minimal() +
#   geom_text_repel(data = flow_df %>% filter(last_fire == max(last_fire)),
#                   aes(stratum = species, label = species),stat = "stratum", size = 4,
#                   nudge_x = -25, direction = "y", fontface = "italic")+
#   geom_text_repel(data = flow_df %>% filter(last_fire == setdiff(min(last_fire),
#                                                                  max(last_fire))),
#                   aes(stratum = species, label = species),stat = "stratum", size = 4,
#                   nudge_x = 25, direction = "y", fontface = "italic")+
#   theme(plot.margin = unit(c(0.1, 0.36, 0.1, 0.86), 
#                            "inches"),
#     panel.grid.minor = element_line(size = 0.25,
#                                         linetype = 1),
#         panel.grid.major = element_line(color = "gray",
#                                         size = 0.25,
#                                         linetype = 1),
#         legend.position = "none",
#     axis.text.x=element_text(size=14),
#     axis.text.y=element_text(size=14))+
#   scale_x_reverse(
#     breaks = ann_appearances_means_byfire %>% 
#                     filter(locality == "eleanor") %>% 
#                     filter(last_fire != 1984) %>% 
#                     pull(last_fire),
#                   minor_breaks = seq(1900, 2020, 5),
#                   expand = c(.145, 0)) +
#   labs(x =NULL, y = NULL)
# 
# ##topanga
# flow_df<-
# ann_appearances_means_byfire %>% 
#   filter(locality == "topanga") %>% 
#   select(!c(locality)) %>% 
#   pivot_longer("O.californicus":katydid15, names_to = "species",
#                values_to = "appearances") %>% 
#  mutate(species = factor(species, levels = unique(species)))
# 
# topanga_flow_plot<-
# flow_df %>% 
# ggplot(aes(x = last_fire, y = appearances, fill = species)) +
#   geom_flow(aes(alluvium = species, stratum = species), alpha = .7, color = "black", width = 0,
#             curve_type = "cubic") +
#   geom_vline(xintercept = ann_appearances_means_byfire %>% 
#                filter(locality == "topanga") %>% 
#                pull(last_fire),
#              linetype = 2,
#              linewidth = 0.5)+
#   theme_minimal() +
#   geom_text_repel(data = flow_df %>% filter(last_fire == max(last_fire)),
#     aes(stratum = species, label = species),stat = "stratum", size = 4,
#     nudge_x = -23, direction = "y", fontface = "italic")+
#   geom_text_repel(data = flow_df %>% filter(last_fire == setdiff(min(last_fire),
#                                                                  max(last_fire))),
#             aes(stratum = species, label = species),stat = "stratum", size = 4,
#             nudge_x = 23, direction = "y", fontface = "italic")+
#   theme(panel.grid.minor = element_line(size = 0.25,
#                                         linetype = 1),
#         panel.grid.major = element_line(color = "gray",
#                                         size = 0.25,
#                                         linetype = 1),
#         legend.position = "none",
#         axis.text.x=element_text(size=14),
#         axis.text.y=element_text(size=14))+
#   scale_x_reverse(breaks = ann_appearances_means_byfire %>% 
#                     filter(locality == "topanga") %>% 
#                     pull(last_fire),
#                   minor_breaks = seq(1900, 2020, 5),
#                   expand = c(.20, 0)) +
#   labs(x =NULL, y = NULL)
# 
# grid.arrange(rancho_flow_plot,
#              eleanor_flow_plot,
#              topanga_flow_plot,
#              ncol= 1,
#              bottom = text_grob(expression("Years from last wildfire"), size = 16),
#              left = text_grob(expression("Presences"), rot = 90, size = 16))

    
##Euclidean distance and nMDS
##Euclidean distance


library(vegan)
library(car)

#Topanga
nmds_df<-
ann_appearances_sums  %>%
  left_join(recorders_by_locality) %>% 
  filter(locality == "topanga") %>% 
  select(!c(locality, cricket_total_app, katydid_total_app))

ndf_last_fires<-
  nmds_df %>% 
  pull(last_fire)

ndf_recorders<-
  nmds_df %>% 
  pull(Recorder)

ndf_species<-
  nmds_df %>% 
  select(!c(last_fire, Recorder))


ndf.dist <- vegdist(ndf_species, method="euclidean")
ndf.mds <- metaMDS(ndf.dist, try=50)

#perMANOVA
d.manova <- adonis2(ndf_species ~ ndf_last_fires, method = "euclidean",
                    data= as.data.frame(ndf_last_fires))
d.manova

#MANOVA Post-Hoc comparison
library(pairwiseAdonis)

fire.adonis.pw <- pairwise.adonis(x = ndf_species,
                                  factors= ndf_last_fires,
                                  sim.method='euclidean',
                                  p.adjust.m='holm')
fire.adonis.pw <- as.data.frame(fire.adonis.pw)
fire.adonis.pw$F.Model <- round(fire.adonis.pw$F.Model, 2)
fire.adonis.pw$R2 <- round(fire.adonis.pw$R2, 2)
fire.adonis.pw

#ggplot NMDS plot
nnplot_topanga<-
ndf.mds$points %>% as.data.frame() %>% 
  cbind(ndf_last_fires) %>% 
  ggplot(aes(x = MDS1, y = MDS2, fill = ndf_last_fires)) +
  geom_point(size = 2.5, color='black',
             shape = 21)  +
  stat_ellipse(aes(group = ndf_last_fires,
                   fill = ndf_last_fires),
               color = "gray",
               level = 0.9,
               geom = "polygon",
               alpha = 0.25) +
  theme_bw() +
  scale_color_distiller(palette="Spectral",
                        direction = -1,
                        limits = c(1900, 2021)) +
  scale_fill_distiller(palette="Spectral",
                        direction = -1,
                       name = NULL,
                       breaks = sort(unique(ndf_last_fires)),
                       labels = sort(unique(ndf_last_fires)),
                       limits = c(1900, 2021)) +
  geom_text(aes(x = 11.5, y = 6,
                label = paste("Stress =",round(ndf.mds$stress,4)))) +
  geom_text(aes(x = -4.6, y = 6,
                label = paste("Topanga State Park"))) +
  labs(x =NULL, y = NULL)+
  theme(plot.margin = unit(c(0.1, 0.08, 0.1, 0.17), 
                           "inches"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) 


#Eleanor
nmds_df<-
  ann_appearances_sums  %>%
  left_join(recorders_by_locality) %>% 
  filter(locality == "eleanor") %>% 
  select(!c(locality, cricket_total_app, katydid_total_app))

ndf_last_fires<-
  nmds_df %>% 
  pull(last_fire)

ndf_recorders<-
  nmds_df %>% 
  pull(Recorder)

ndf_species<-
  nmds_df %>% 
  select(!c(last_fire, Recorder))


ndf.dist <- vegdist(ndf_species, method="euclidean")
ndf.mds <- metaMDS(ndf.dist, try=50)

#perMANOVA
d.manova <- adonis2(ndf_species ~ ndf_last_fires, method = "euclidean",
                    data= as.data.frame(ndf_last_fires))
d.manova

#MANOVA Post-Hoc comparison

fire.adonis.pw <- pairwise.adonis(x = ndf_species,
                                  factors= ndf_last_fires,
                                  sim.method='euclidean',
                                  p.adjust.m='holm')
fire.adonis.pw <- as.data.frame(fire.adonis.pw)
fire.adonis.pw$F.Model <- round(fire.adonis.pw$F.Model, 2)
fire.adonis.pw$R2 <- round(fire.adonis.pw$R2, 2)
fire.adonis.pw


#ggplot NMDS plot
nnplot_eleanor<-
  ndf.mds$points %>% as.data.frame() %>% 
  cbind(ndf_last_fires) %>% 
  ggplot(aes(x = MDS1, y = MDS2, fill = ndf_last_fires)) +
  geom_point(size = 2.5, color='black',
             shape = 21)  +
  stat_ellipse(aes(group = ndf_last_fires,
                   fill = ndf_last_fires),
               color = "gray",
               level = 0.9,
               geom = "polygon",
               alpha = 0.25) +
  theme_bw() +
  scale_color_distiller(palette="Spectral",
                        direction = -1,
                        limits = c(1900, 2021)) +
  scale_fill_distiller(palette="Spectral",
                       direction = -1,
                       name = NULL,
                       breaks = c(1900,1984, 2006, 2013),
                       labels = c(1900,1984, 2006, 2013),
                       limits = c(1900, 2021)) +
  geom_text(aes(x = 7.3, y = 6.5,
                label = paste("Stress =",round(ndf.mds$stress,4))))+
  geom_text(aes(x = -6, y = 6.5,
                label = "Lake Eleanor Open Space"))+
  labs(x =NULL, y = NULL)+
  theme(plot.margin = unit(c(0.1, 0.08, 0.1, 0.17), 
                           "inches"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) 


#Rancho
nmds_df<-
  ann_appearances_sums  %>%
  left_join(recorders_by_locality) %>% 
  filter(locality == "rancho") %>% 
  select(!c(locality, cricket_total_app, katydid_total_app))

ndf_last_fires<-
  nmds_df %>% 
  pull(last_fire)

ndf_recorders<-
  nmds_df %>% 
  pull(Recorder)

ndf_species<-
  nmds_df %>% 
  select(!c(last_fire, Recorder))


ndf.dist <- vegdist(ndf_species, method="euclidean")
ndf.mds <- metaMDS(ndf.dist, try=50)

#perMANOVA
d.manova <- adonis2(ndf_species ~ ndf_last_fires, method = "euclidean",
                    data= as.data.frame(ndf_last_fires))
d.manova

#MANOVA Post-Hoc comparison
library(pairwiseAdonis)

fire.adonis.pw <- pairwise.adonis(x = ndf_species,
                                  factors= ndf_last_fires,
                                  sim.method='euclidean',
                                  p.adjust.m='holm')
fire.adonis.pw <- as.data.frame(fire.adonis.pw)
fire.adonis.pw$F.Model <- round(fire.adonis.pw$F.Model, 2)
fire.adonis.pw$R2 <- round(fire.adonis.pw$R2, 2)
fire.adonis.pw

#ggplot NMDS plot
nnplot_rancho<-
  ndf.mds$points %>% as.data.frame() %>% 
  cbind(ndf_last_fires) %>% 
  ggplot(aes(x = MDS1, y = MDS2, fill = ndf_last_fires)) +
  geom_point(size = 2.5, color='black',
             shape = 21)  +
  ggforce::geom_mark_ellipse(aes(fill = ndf_last_fires,
                                 group = ndf_last_fires),
                             color = "gray",
                             alpha = 0.25)+
  theme_bw() +
  scale_color_distiller(palette="Spectral",
                        direction = -1) +
  scale_fill_distiller(palette="Spectral",
                       direction = -1,
                       name = "Last fire",
                       breaks = sort(unique(ndf_last_fires)),
                       labels = sort(unique(ndf_last_fires)),
                       limits = c(1900, 2021)) +
  xlim(-5.7, 6.2)+
  ylim(-5, 6.5)+
  geom_text(aes(x = 5.1, y = 6.5,
                label = paste("Stress =",round(ndf.mds$stress,4))))+
  geom_text(aes(x = -1.5, y = 6.5,
                label = "Circle X Ranch/ Rancho Sierra Vista/ Satwiwa"))+
  labs(x =NULL, y = NULL)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

####GRID
grid.arrange(nnplot_rancho,
             nnplot_eleanor,
             nnplot_topanga,
             ncol= 1,
             bottom = text_grob(expression("MDS1"), size = 16),
             left = text_grob(expression("MDS2"), rot = 90, size = 16))


#jaccard
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

#topanga
unburnt <-
ann_specieslist %>% filter(locality == "topanga" & last_fire == 1900) %>% 
  pull(species_list) %>% strsplit(" ")
  
top_1961 <-
ann_specieslist %>% filter(locality == "topanga" & last_fire == 1961) %>% 
  pull(species_list) %>% str_split(" ")

top_1984 <-
  ann_specieslist %>% filter(locality == "topanga" & last_fire == 1984) %>% 
  pull(species_list) %>% str_split(" ")

top_2021 <-
  ann_specieslist %>% filter(locality == "topanga" & last_fire == 2021) %>% 
  pull(species_list) %>% str_split(" ")


c(jaccard(unburnt[[1]], unburnt[[1]]),
  jaccard(unburnt[[1]], top_1961[[1]]),
  jaccard(unburnt[[1]], top_1984[[1]]),
  jaccard(unburnt[[1]], top_2021[[1]]))

jaccard_plot_topanga <- data.frame(
  Year = c("1950", "1961", "1984", "2021"),
  Index = c(jaccard(top_2021[[1]], top_2021[[1]]),
              jaccard(top_2021[[1]], top_1984[[1]]),
              jaccard(top_2021[[1]], top_1961[[1]]),
              jaccard(top_2021[[1]], unburnt[[1]])))

plot(jaccard_plot_topanga$Year, jaccard_plot_topanga$Index)

rm(unburnt, top_1961, top_1984, top_2021)

#eleanor
unburnt <-
  ann_specieslist %>% filter(locality == "eleanor" & last_fire == 1900) %>% 
  pull(species_list) %>% strsplit(" ")

el_1984 <-
  ann_specieslist %>% filter(locality == "eleanor" & last_fire == 1984) %>% 
  pull(species_list) %>% strsplit(" ")

el_1988 <-
  ann_specieslist %>% filter(locality == "eleanor" & last_fire == 1988) %>% 
  pull(species_list) %>% strsplit(" ")

el_2006 <-
  ann_specieslist %>% filter(locality == "eleanor" & last_fire == 2006) %>% 
  pull(species_list) %>% strsplit(" ")

c(jaccard(unburnt[[1]], el_2006[[1]]),
  jaccard(unburnt[[1]], el_1988[[1]]),
  jaccard(unburnt[[1]], el_1984[[1]]),
  jaccard(unburnt[[1]], unburnt[[1]]))

jaccard_plot_eleanor <- data.frame(
  Year = c("1984", "1988", "2006"),
  Index = c(jaccard(unburnt[[1]], el_1984[[1]]),
            jaccard(unburnt[[1]], el_1988[[1]]),
            jaccard(unburnt[[1]], el_2006[[1]])))

plot(jaccard_plot_eleanor$Year, jaccard_plot_eleanor$Index)

#rancho
ra_2018 <-
  ann_specieslist %>% filter(locality == "rancho" & last_fire == 2018) %>% 
  pull(species_list) %>% strsplit(" ")

ra_2013 <-
  ann_specieslist %>% filter(locality == "rancho" & last_fire == 2013) %>% 
  pull(species_list) %>% strsplit(" ")

ra_1993 <-
  ann_specieslist %>% filter(locality == "rancho" & last_fire == 1993) %>% 
  pull(species_list) %>% strsplit(" ")

c(jaccard(ra_1993[[1]], ra_2018[[1]]),
  jaccard(ra_1993[[1]], ra_2013[[1]]),
  jaccard(ra_1993[[1]], ra_1993[[1]]))

#Indicator species analysis
library(indicspecies)

#Topanga
species<-
ann_appearances_means_byfire %>% as.data.frame() %>% 
  filter(locality =="topanga") %>% 
  select(-c(locality, last_fire)) 

groups <- ann_appearances_means_byfire %>% as.data.frame() %>% 
  filter(locality =="topanga") %>%  
  mutate(year = 2022-last_fire) %>% pull(year) 

indval<-
multipatt(species, groups,
          control = how(nperm=999))

summary(indval)

#Eleanor
species<-
  ann_appearances_means_byfire %>% as.data.frame() %>% 
  filter(locality =="eleanor") %>% 
  select(-c(locality, last_fire)) 

groups <- ann_appearances_means_byfire %>% as.data.frame() %>% 
  filter(locality =="eleanor") %>%  
  mutate(year = 2022-last_fire) %>% pull(year) 

indval<-
  multipatt(species, groups,
            control = how(nperm=999))

summary(indval)

#Rancho
species<-
  ann_appearances_means_byfire %>% as.data.frame() %>% 
  filter(locality =="rancho") %>% 
  select(-c(locality, last_fire)) 

groups <- ann_appearances_means_byfire %>% as.data.frame() %>% 
  filter(locality =="rancho") %>%  
  mutate(year = 2022-last_fire) %>% pull(year) 

indval<-
  multipatt(species, groups,
            control = how(nperm=999))

summary(indval)

##Shannon diversity index
library(vegan)
library(tibble)

diversity_indices<-
ann_appearances_sums %>% 
  column_to_rownames(var = "Recorder") %>% 
  select(!c(cricket_total_app, katydid_total_app)) %>% 
  mutate(shannon = diversity(., index = "shannon"),
         simpson = diversity(., index = "simpson"),
         invsimpson = diversity(., index = "invsimpson")) %>% 
  mutate(Recorder = as.character(row.names(.)))%>% 
  select(Recorder, shannon, simpson, invsimpson) %>% 
  rename(recorder_id = Recorder)
  

full_df <-
full_df %>% 
  left_join(
    ann_presences %>% 
      select(cricket_avg_richness, katydid_avg_richness, mean_richness,
        recorder_id, cricket_richness, katydid_richness, total_richness)
  )

#average indices

averaged <-
  full_df %>% 
  summarize(ac = mean(acoustic_complexity), 
            sd_ac = sd(acoustic_complexity),
            ad = mean(acoustic_diversity), 
            sd_ad = sd(acoustic_diversity),
            ae = mean(acoustic_evenness), 
            sd_ae = sd(acoustic_evenness),
            bi = mean(bioacoustic_index),
            sd_bi = sd(bioacoustic_index),
            h = mean(H),
            sd_h = sd(H),
            nds = mean(ndsi),
            sd_nds = sd(ndsi),
            temp = mean(temperature_celcius),
            sd_temp = sd(temperature_celcius),
            ndvi = mean(ndvi),
            .by = c(locality, season, day_night, last_fire,
                    years_last_fire, recorder_id, elevation,
                    slope, aspect, distance_10y, vegetation,
                    cricket_richness, katydid_richness, total_richness,
                    cricket_avg_richness, katydid_avg_richness, mean_richness)) %>% 
  mutate(last_fire = as.character(last_fire)) %>% filter(day_night != "astro_twilight") %>% 
  mutate(aspect = case_when( #rescaled aspect NORTH = 1, SOUTH = 0
    aspect > 180 ~ 
      rescale(aspect, to = c(1,0),
              from = c(180, 360)),
    aspect < 180 ~
      rescale(aspect, to = c(0,1),
              from = c(0, 180)),
    TRUE ~ NA)) %>% 
  rename(ndsi = nds, sd_ndsi = sd_nds)

#standard deviation averages
sd_avg <-
averaged %>% 
  summarize(ac = mean(sd_ac), 
            ad = mean(sd_ad), 
            ae = mean(sd_ae), 
            bi = mean(sd_bi),
            h = mean(sd_h),
            ndsi = mean(sd_ndsi),
            .by = c(locality, season, day_night))


#plot by day/night

plot_averaged <-
  function(df, index) {
    ggplot(df, aes(x = last_fire, y = .data[[index]], color = season)) +
      geom_boxplot() +
      stat_summary(fun = mean,
                   geom = "line",
                   aes(group = season)) +
      facet_wrap(~day_night)+
      theme_bw()+
      labs(y = toupper(index), x = "Last fire")+
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 
                               "inches"))+
      scale_color_manual(values = c("#A35027", "#3B90CC"))}

ac_plot <- plot_averaged(averaged,"ac")
ad_plot <- plot_averaged(averaged,"ad")
ae_plot <- plot_averaged(averaged,"ae")
bi_plot <- plot_averaged(averaged,"bi")
h_plot <- plot_averaged(averaged,"h")
ndsi_plot <- plot_averaged(averaged,"ndsi")

#extract legend for grid
grid_legend <-
  averaged %>% 
  ggplot(aes(x = last_fire, y = ac, color = season)) +
  geom_boxplot() +
  stat_summary(fun = median,
               geom = "line",
               aes(group = season)) +
  facet_wrap(~day_night)+
  theme_bw()+
  labs(y = toupper(deparse(substitute(df))), x = "Last fire")+
  scale_color_manual(values = c("#A35027", "#3B90CC"))

# extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)}

grid_legend <- extract_legend(grid_legend)

#grid
top_plot <-
  function(gg) {gg +
      theme(axis.text.x=element_blank())}

distance_y <-
  function(gg, m, d) {gg +
      theme(plot.margin = unit(c(0.1, 0.1, 0.1, m), 
                               "inches"),
            axis.title.y=element_text(vjust = d))}



main_title <- expression(bold("Topanga"))
x_title <- expression("Last Fire")

grid.arrange(arrangeGrob(
  top_plot(ac_plot),
  distance_y(top_plot(ad_plot), 0.02, -1),
  distance_y(top_plot(ae_plot), 0.2, +4),
  distance_y(top_plot(bi_plot), 0.2, +4),
  distance_y(h_plot, 0.13, +1),
  ndsi_plot,
  nrow = 3,
  ncol=2),
  grid_legend, ncol=2, widths=c(10, 1),
  bottom = text_grob(x_title),
  top = text_grob(main_title))
# 
# #PCA
# library(ggfortify)
# library(RColorBrewer)
# 
# data_pca <-
#   function (df, seas, diel, local) {
#     df %>% 
#       dplyr::filter(season == seas,
#              day_night == diel,
#              locality == local) %>% 
#       summarize(temperature = mean(temperature_celcius),
#                 temp_sd = sd(H),
#                 ndvi = mean(ndvi),
#                 .by = c(locality, season, day_night, last_fire, recorder_id, slope, aspect,
#                         elevation, distance_10y, years_last_fire, vegetation)) %>% 
#       mutate(last_fire = as.character(last_fire)) %>%
#       dplyr::filter(day_night != "astro_twilight") %>% 
#       mutate(aspect = case_when( #rescaled aspect NORTH = 1, SOUTH = 0
#         aspect > 180 ~ 
#           rescale(aspect, to = c(1,0),
#                   from = c(180, 360)),
#         aspect < 180 ~
#           rescale(aspect, to = c(0,1),
#                   from = c(0, 180)),
#         TRUE ~ NA)) %>% 
#       select(aspect, slope, aspect, elevation, years_last_fire, temperature, ndvi)
#   }
# 
# plot_pca <-
#   function(pc, df) {
#     plot <- autoplot(pc, data = df, colour = "years_last_fire",
#                      loadings = TRUE, loadings.colour = 'blue',
#                      loadings.label = TRUE, loadings.label.size = 3)+
#       theme_bw() +
#       scale_color_distiller(palette="Spectral",
#                             direction = 1)+
#       stat_ellipse(aes(group = years_last_fire,
#                        color = years_last_fire),
#                    level=0.8)+
#       theme(legend.position = "none",
#             plot.title = element_text(vjust = -7, hjust = 0.5,
#                                       size=10))+
#       ggtitle(deparse(substitute(df)))
#     pca <- summary(pc)
#     return(list(pca, plot))
#   }
# 
# #plot everything
# locality <- "topamnga"
# 
# #summer
# night<-
# data_pca(df = full_df, seas ="summer", diel = "night", local = locality) 
# 
# pca_plot_summer_night <- night %>% 
#    prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(night)
# 
# dawn<-
#   data_pca(df = full_df, seas ="summer", diel = "dawn", local = locality) 
# 
# pca_plot_summer_dawn <- dawn %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(dawn)
# 
# day<-
#   data_pca(df = full_df, seas ="summer", diel = "day", local = locality) 
# 
# pca_plot_summer_day <- day %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(day)       
# 
# dusk<-
#   data_pca(df = full_df, seas ="summer", diel = "dusk", local = locality) 
# 
# pca_plot_summer_dusk <- dusk %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(dusk)
# 
# #fall
# 
# night<-
#   data_pca(df = full_df, seas ="fall", diel = "night", local = locality) 
# 
# pca_plot_fall_night <- night %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(night)
# 
# dawn<-
#   data_pca(df = full_df, seas ="fall", diel = "dawn", local = locality) 
# 
# pca_plot_fall_dawn <- dawn %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(dawn)
# 
# day<-
#   data_pca(df = full_df, seas ="fall", diel = "day", local = locality) 
# 
# pca_plot_fall_day <- day %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(day)
# 
# dusk<-
#   data_pca(df = full_df, seas ="fall", diel = "dusk", local = locality) 
# 
# pca_plot_fall_dusk <- dusk %>% 
#   prcomp(center = TRUE, scale. = TRUE) %>% 
#   plot_pca(dusk)
# 
# #grid
# 
# #extract legend for grid
# grid_legend <-
#   autoplot(prcomp(night, center = TRUE, scale. = TRUE),
#      data = night, colour = "years_last_fire",
#            loadings = TRUE, loadings.colour = 'blue',
#            loadings.label = TRUE, loadings.label.size = 3)+
#   theme_bw() +
#   scale_color_distiller(palette="Spectral", name = "Last fire (y)",
#                         direction = 1,
#                         breaks = sort(unique(night$years_last_fire)),
#                         labels = sort(unique(night$years_last_fire)))
# 
# # extracts legends from ggplots
# extract_legend <- function(my_ggp) {
#   step1 <- ggplot_gtable(ggplot_build(my_ggp))
#   step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
#   step3 <- step1$grobs[[step2]]
#   return(step3)}
# 
# grid_legend <- extract_legend(grid_legend)
# 
# main_title <- expression(bold("Topanga"))
# 
# grid.arrange(arrangeGrob(
#   pca_plot_summer_night[[2]], pca_plot_summer_dawn[[2]],
#   pca_plot_summer_day[[2]], pca_plot_summer_dusk[[2]],
#   top = "Summer", ncol = 1),
#   arrangeGrob(pca_plot_fall_night[[2]], pca_plot_fall_dawn[[2]],
#   pca_plot_fall_day[[2]], pca_plot_fall_dusk[[2]],
#   top = "Fall", ncol=1),
#   grid_legend, ncol=3,
#   widths=c(4.5, 4.5, 1),
#   top = text_grob(main_title))
# 
# ###discriminant analysis for locality
# library(MASS)
# library(caret)
# library(ggord)
# 
# dfa<-
# function(dataf, variable, seas, diel) {
#   dfa_df <-
#     dataf %>% 
#     filter(season == seas,
#            day_night == diel) %>% 
#     dplyr::select(., {{variable}}, elevation, temp,
#                   slope, aspect, ndvi)
#   
#   ## Split the data into training (80%) and test set (20%)
#   set.seed(123)
#   
#   training.samples <- createDataPartition(dfa_df[[deparse(substitute(variable))]], 
#                                           p = 0.8, 
#                                           list = FALSE)
#   
#   train.data <- dfa_df[training.samples, ] 
#   test.data <- dfa_df[-training.samples, ] 
#   
#   ## normalize data
#   # Estimate preprocessing parameters
#   preproc.param <- train.data %>% 
#     preProcess(method = c("center", "scale"))
#   # Transform the data using the estimated parameters
#   train.transformed <- preproc.param %>% predict(train.data)
#   test.transformed <- preproc.param %>% predict(test.data)
#   
#   # Fit the model
#   fo <- reformulate(".", deparse(substitute(variable)))
#   model <- lda(fo, data = train.transformed, method = "t")
#   model
#   # Make predictions
#   predictions <- model %>% predict(test.transformed)
#   # Model accuracy
#   acc<-mean(predictions$class==test.transformed[[deparse(substitute(variable))]])
#   
#   plot<-
#   ggord::ggord(model, train.transformed %>% 
#                  pull({{variable}}))
#   
#   return(model)
# }
# 
# 
# dfa(averaged, variable = locality, "summer", "night")
# dfa(averaged, variable = vegetation, "summer", "night")
# dfa(averaged, variable = last_fire, "summer", "night")
# 
# 
# ###discriminant analysis within locality was not possible
# 
# #chi-square
# chi_sq_df<-
#   averaged %>% 
#   filter(season == "summer",
#          day_night == "night") %>% 
#   dplyr::select(.,vegetation, locality)
# 
# temp<-
# table(chi_sq_df)
# 
# chisq.test(table(chi_sq_df)) #locality and vegetation are highly associated
# 
# chi_sq_df<-
#   averaged %>% 
#   filter(season == "summer",
#          day_night == "night") %>% 
#   dplyr::select(., vegetation,  years_last_fire)
# 
#   chisq.test(table(chi_sq_df)) #last fire and vegetation are even more so associated
# 
# ### community ecology analysis
#   
#   

#non-parametric regression


# mod.ss <- ss(reg_df$years_last_fire,reg_df$index,nknots=4) 
# mod.ss
# plot(reg_df$years_last_fire,reg_df$index)
# lines(reg_df$years_last_fire,fitted(mod.ss), lty=10,col="blue")
# mod.ss
# 
# summary(mod.ss)
# 
# plot(mod.ss)


#GAM for multiple variables per locality
library(gam)

seas <- "summer"
diel <- "day"
loc <- "topanga"

reg_df <-
  averaged %>%
  filter(season == seas,
         day_night == diel,
         locality == loc) %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(diversity_indices)

#model fit and selection function

mod_sel<- #model with smoothing splines
function(dataf, index,loc, diel, seas, diversity_ind){
  
  df <-
    dataf %>%
    filter(season == {{seas}},
           day_night == {{diel}},
           locality == {{loc}}) %>% 
    mutate(last_fire = as.numeric(last_fire)) %>% 
    mutate(recorder_id = as.character(recorder_id)) %>% 
    left_join(diversity_ind)
  
  
  mod1 <- gam(
    reformulate(c("s(last_fire)", "vegetation", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod2 <- gam(
    reformulate(c("s(last_fire)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod3 <- gam(
    reformulate(c("s(last_fire)", "s(elevation)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod4 <- gam(
    reformulate(c("s(last_fire)", "s(elevation)", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod5 <- gam(
    reformulate(c("s(last_fire)", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod6 <- gam(
    reformulate(c("s(last_fire)"),
                response = deparse(substitute(index))), data = df)
  
  mod7 <- gam(
    reformulate(c("s(last_fire)", "s(ndvi)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod8 <- gam(
    reformulate(c("s(last_fire)", "s(ndvi)", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod9 <- gam(
    reformulate(c("s(last_fire)", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod10 <- gam(
    reformulate(c("s(last_fire)", "vegetation", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod11 <- gam(
    reformulate(c("s(last_fire)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod12 <- gam(
    reformulate(c("s(last_fire)", "elevation", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod13 <- gam(
    reformulate(c("s(last_fire)", "elevation", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod14 <- gam(
    reformulate(c("s(last_fire)", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod15 <- gam(
    reformulate(c("s(last_fire)", "s(ndvi)", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod16 <- gam(
    reformulate(c("s(last_fire)", "vegetation", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod17 <- gam(
    reformulate(c("s(last_fire)", "s(elevation)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod18 <- gam(
    reformulate(c("s(last_fire)", "s(elevation)", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod19 <- gam(
    reformulate(c("s(last_fire)", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod20 <- gam(
    reformulate(c("s(last_fire)", "ndvi", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod21 <- gam(
    reformulate(c("s(last_fire)", "ndvi", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod22 <- gam(
    reformulate(c("s(last_fire)", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod23 <- gam(
    reformulate(c("s(last_fire)", "vegetation", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod24 <- gam(
    reformulate(c("s(last_fire)", "vegetation", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod25 <- gam(
    reformulate(c("s(last_fire)", "elevation", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod26 <- gam(
    reformulate(c("s(last_fire)", "elevation", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod27 <- gam(
    reformulate(c("s(last_fire)", "ndvi", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod28 <- gam(
    reformulate(c("s(last_fire)", "ndvi", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod29 <- gam(
    reformulate(c("last_fire", "vegetation", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod30 <- gam(
    reformulate(c("last_fire", "vegetation", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod31 <- gam(
    reformulate(c("last_fire", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod32 <- gam(
    reformulate(c("last_fire", "s(elevation)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod33 <- gam(
    reformulate(c("last_fire", "s(elevation)", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod34 <- gam(
    reformulate(c("last_fire", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod35 <- gam(
    reformulate(c("last_fire"),
                response = deparse(substitute(index))), data = df)
  
  mod36 <- gam(
    reformulate(c("last_fire", "s(ndvi)", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod37 <- gam(
    reformulate(c("last_fire", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod38 <- gam(
    reformulate(c("last_fire", "vegetation", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod39 <- gam(
    reformulate(c("last_fire", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod40 <- gam(
    reformulate(c("last_fire", "elevation", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod41 <- gam(
    reformulate(c("last_fire", "elevation", "s(ndvi)"),
                response = deparse(substitute(index))), data = df)
  
  mod42 <- gam(
    reformulate(c("last_fire", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod43 <- gam(
    reformulate(c("last_fire", "s(ndvi)", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod44 <- gam(
    reformulate(c("last_fire", "vegetation", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod45 <- gam(
    reformulate(c("last_fire", "s(elevation)", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod46 <- gam(
    reformulate(c("last_fire", "s(elevation)", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod47 <- gam(
    reformulate(c("last_fire", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod48 <- gam(
    reformulate(c("last_fire", "ndvi", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod49 <- gam(
    reformulate(c("last_fire", "ndvi", "s(elevation)"),
                response = deparse(substitute(index))), data = df)
  
  mod50 <- gam(
    reformulate(c("last_fire", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod51 <- gam(
    reformulate(c("last_fire", "vegetation", "elevation"),
                response = deparse(substitute(index))), data = df)
  
  mod52 <- gam(
    reformulate(c("last_fire", "vegetation", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod53 <- gam(
    reformulate(c("last_fire", "elevation", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod54 <- gam(
    reformulate(c("last_fire", "elevation", "ndvi"),
                response = deparse(substitute(index))), data = df)
  
  mod55 <- gam(
    reformulate(c("last_fire", "ndvi", "vegetation"),
                response = deparse(substitute(index))), data = df)
  
  mod56 <- gam(
    reformulate(c("last_fire", "ndvi", "elevation"),
                response = deparse(substitute(index))), data = df)
 
  
  model <-
    c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9",
      "mod10", "mod11", "mod12", "mod13", "mod14", "mod15", "mod16", "mod17",
      "mod18", "mod19", "mod20", "mod21", "mod22", "mod23", "mod24", "mod25",
      "mod26", "mod27", "mod28", "mod29", "mod30", "mod31", "mod32", "mod33",
      "mod34", "mod35", "mod36", "mod37", "mod38", "mod39", "mod40", "mod41",
      "mod42", "mod43", "mod44", "mod45", "mod46", "mod47", "mod48", "mod49",
      "mod50", "mod51", "mod52", "mod53", "mod54", "mod55", "mod56")
  
  aic<-
    c(mod1$aic, mod2$aic, mod3$aic, mod4$aic, mod5$aic, mod6$aic, mod7$aic,
      mod8$aic, mod9$aic, mod10$aic, mod11$aic, mod12$aic, mod13$aic,
      mod14$aic, mod15$aic, mod16$aic, mod17$aic, mod18$aic, mod19$aic,
      mod20$aic, mod21$aic, mod22$aic, mod23$aic, mod24$aic, mod25$aic,
      mod26$aic, mod27$aic, mod28$aic, mod29$aic, mod30$aic, mod31$aic,
      mod32$aic, mod33$aic, mod34$aic, mod35$aic, mod36$aic, mod37$aic,
      mod38$aic, mod39$aic, mod40$aic, mod41$aic, mod42$aic, mod43$aic,
      mod44$aic, mod45$aic, mod46$aic, mod47$aic, mod48$aic, mod49$aic,
      mod50$aic, mod51$aic, mod52$aic, mod53$aic, mod54$aic, mod55$aic,
      mod56$aic)
  
  call<-
    c(as.character(mod1$formula)[3], as.character(mod2$formula)[3],
      as.character(mod3$formula)[3], as.character(mod4$formula)[3],
      as.character(mod5$formula)[3], as.character(mod6$formula)[3],
      as.character(mod7$formula)[3], as.character(mod8$formula)[3],
      as.character(mod9$formula)[3], as.character(mod10$formula)[3],
      as.character(mod11$formula)[3], as.character(mod12$formula)[3],
      as.character(mod13$formula)[3], as.character(mod14$formula)[3],
      as.character(mod15$formula)[3], as.character(mod16$formula)[3],
      as.character(mod17$formula)[3], as.character(mod18$formula)[3],
      as.character(mod19$formula)[3], as.character(mod20$formula)[3],
      as.character(mod21$formula)[3], as.character(mod22$formula)[3],
      as.character(mod23$formula)[3], as.character(mod24$formula)[3],
      as.character(mod25$formula)[3], as.character(mod26$formula)[3],
      as.character(mod27$formula)[3], as.character(mod28$formula)[3],
      as.character(mod29$formula)[3], as.character(mod30$formula)[3],
      as.character(mod31$formula)[3], as.character(mod32$formula)[3],
      as.character(mod33$formula)[3], as.character(mod34$formula)[3],
      as.character(mod35$formula)[3], as.character(mod36$formula)[3],
      as.character(mod37$formula)[3], as.character(mod38$formula)[3],
      as.character(mod39$formula)[3], as.character(mod40$formula)[3],
      as.character(mod41$formula)[3], as.character(mod42$formula)[3],
      as.character(mod43$formula)[3], as.character(mod44$formula)[3],
      as.character(mod45$formula)[3], as.character(mod46$formula)[3],
      as.character(mod47$formula)[3], as.character(mod48$formula)[3],
      as.character(mod49$formula)[3], as.character(mod50$formula)[3],
      as.character(mod51$formula)[3], as.character(mod52$formula)[3],
      as.character(mod53$formula)[3], as.character(mod54$formula)[3],
      as.character(mod55$formula)[3], as.character(mod56$formula)[3])
  
  selection_df<-data.frame(model,call,aic) %>% 
    filter(aic == min(aic))
  
  summ<-summary(eval(parse(text = selection_df[1])))
  
  return(eval(parse(text = selection_df[1])))
}

mod_sel_rancho<- #linear model
  function(dataf, index,loc, diel, seas, diversity_ind){
    
    df <-
      dataf %>%
      filter(season == {{seas}},
             day_night == {{diel}},
             locality == {{loc}})%>% 
      mutate(last_fire = as.numeric(last_fire)) %>% 
      mutate(recorder_id = as.character(recorder_id)) %>% 
      left_join(diversity_ind)
    
    
    mod1 <- gam(
      reformulate(c("last_fire", "vegetation", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod2 <- gam(
      reformulate(c("last_fire", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod3 <- gam(
      reformulate(c("last_fire", "elevation", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod4 <- gam(
      reformulate(c("last_fire", "elevation", "ndvi"),
                  response = deparse(substitute(index))), data = df)
    
    mod5 <- gam(
      reformulate(c("last_fire", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod6 <- gam(
      reformulate(c("last_fire"),
                  response = deparse(substitute(index))), data = df)
    
    mod7 <- gam(
      reformulate(c("last_fire", "ndvi", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod8 <- gam(
      reformulate(c("last_fire", "ndvi", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod9 <- gam(
      reformulate(c("last_fire", "ndvi"),
                  response = deparse(substitute(index))), data = df)
    
    
    model <-
      c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9")
    
    aic<-
      c(mod1$aic, mod2$aic, mod3$aic, mod4$aic, mod5$aic, mod6$aic, mod7$aic,
        mod8$aic, mod9$aic)
    
    call<-
      c(as.character(mod1$formula)[3], as.character(mod2$formula)[3],
        as.character(mod3$formula)[3], as.character(mod4$formula)[3],
        as.character(mod5$formula)[3], as.character(mod6$formula)[3],
        as.character(mod7$formula)[3], as.character(mod8$formula)[3],
        as.character(mod9$formula)[3])
    
    selection_df<-data.frame(model,call,aic) %>% 
      filter(aic == min(aic))
    
    summ<-summary(eval(parse(text = selection_df[1])))
    
    return(eval(parse(text = selection_df[1])))
  }

mod_sel_ndvi<- #model with smoothing splines
  function(dataf, index,loc, diel, seas, diversity_ind){
    
    df <-
      dataf %>%
      filter(season == {{seas}},
             day_night == {{diel}},
             locality == {{loc}}) %>% 
      mutate(last_fire = as.numeric(last_fire)) %>% 
      mutate(recorder_id = as.character(recorder_id)) %>% 
      left_join(diversity_ind)
    
    
    mod1 <- gam(
      reformulate(c("s(last_fire)", "vegetation", "s(elevation)"),
                  response = deparse(substitute(index))), data = df)
    
    mod2 <- gam(
      reformulate(c("s(last_fire)", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod3 <- gam(
      reformulate(c("s(last_fire)", "s(elevation)"),
                  response = deparse(substitute(index))), data = df)
    
    mod4 <- gam(
      reformulate(c("s(last_fire)"),
                  response = deparse(substitute(index))), data = df)
    
    mod5 <- gam(
      reformulate(c("s(last_fire)", "vegetation", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod6 <- gam(
      reformulate(c("s(last_fire)", "elevation", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod7 <- gam(
      reformulate(c("s(last_fire)", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod8 <- gam(
      reformulate(c("s(last_fire)", "s(elevation)"),
                  response = deparse(substitute(index))), data = df)
    
    mod9 <- gam(
      reformulate(c("s(last_fire)", "vegetation", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod10 <- gam(
      reformulate(c("s(last_fire)", "elevation", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod11 <- gam(
      reformulate(c("last_fire", "vegetation", "s(elevation)"),
                  response = deparse(substitute(index))), data = df)
    
    mod12 <- gam(
      reformulate(c("last_fire", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod13 <- gam(
      reformulate(c("last_fire", "s(elevation)", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod14 <- gam(
      reformulate(c("last_fire", "s(elevation)"),
                  response = deparse(substitute(index))), data = df)
    
    mod15 <- gam(
      reformulate(c("last_fire"),
                  response = deparse(substitute(index))), data = df)
    
    mod16 <- gam(
      reformulate(c("last_fire", "vegetation", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod17 <- gam(
      reformulate(c("last_fire", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod18 <- gam(
      reformulate(c("last_fire", "elevation", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    model <-
      c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9",
        "mod10", "mod11", "mod12", "mod13", "mod14", "mod15", "mod16", "mod17",
        "mod18")
    
    aic<-
      c(mod1$aic, mod2$aic, mod3$aic, mod4$aic, mod5$aic, mod6$aic, mod7$aic,
        mod8$aic, mod9$aic, mod10$aic, mod11$aic, mod12$aic, mod13$aic,
        mod14$aic, mod15$aic, mod16$aic, mod17$aic, mod18$aic)
    
    call<-
      c(as.character(mod1$formula)[3], as.character(mod2$formula)[3],
        as.character(mod3$formula)[3], as.character(mod4$formula)[3],
        as.character(mod5$formula)[3], as.character(mod6$formula)[3],
        as.character(mod7$formula)[3], as.character(mod8$formula)[3],
        as.character(mod9$formula)[3], as.character(mod10$formula)[3],
        as.character(mod11$formula)[3], as.character(mod12$formula)[3],
        as.character(mod13$formula)[3], as.character(mod14$formula)[3],
        as.character(mod15$formula)[3], as.character(mod16$formula)[3],
        as.character(mod17$formula)[3], as.character(mod18$formula)[3])
    
    selection_df<-data.frame(model,call,aic) %>% 
      filter(aic == min(aic))
    
    summ<-summary(eval(parse(text = selection_df[1])))
    
    return(eval(parse(text = selection_df[1])))
  }

mod_sel_ndvi_rancho<- #model with smoothing splines
  function(dataf, index,loc, diel, seas, diversity_ind){
    
    df <-
      dataf %>%
      filter(season == {{seas}},
             day_night == {{diel}},
             locality == {{loc}}) %>% 
      mutate(last_fire = as.numeric(last_fire)) %>% 
      mutate(recorder_id = as.character(recorder_id)) %>% 
      left_join(diversity_ind)
    
    mod1 <- gam(
      reformulate(c("last_fire"),
                  response = deparse(substitute(index))), data = df)
    
    mod2 <- gam(
      reformulate(c("last_fire", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    mod3 <- gam(
      reformulate(c("last_fire", "vegetation", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod4 <- gam(
      reformulate(c("last_fire", "elevation"),
                  response = deparse(substitute(index))), data = df)
    
    mod5 <- gam(
      reformulate(c("last_fire", "elevation", "vegetation"),
                  response = deparse(substitute(index))), data = df)
    
    model <-
      c("mod1", "mod2", "mod3", "mod4", "mod5")
    
    aic<-
      c(mod1$aic, mod2$aic, mod3$aic, mod4$aic, mod5$aic)
    
    call<-
      c(as.character(mod1$formula)[3], as.character(mod2$formula)[3],
        as.character(mod3$formula)[3], as.character(mod4$formula)[3],
        as.character(mod5$formula)[3])
    
    selection_df<-data.frame(model,call,aic) %>% 
      filter(aic == min(aic))
    
    summ<-summary(eval(parse(text = selection_df[1])))
    
    return(eval(parse(text = selection_df[1])))
  }


#Topanga
mod_bi_topanga<-
mod_sel(averaged, bi, "topanga", "night", "summer", diversity_indices)
summary(mod_bi_topanga)

mod_h_topanga<-
  mod_sel(reg_df, h, "topanga", "night", "summer", diversity_indices)
summary(mod_h_topanga)

mod_ndsi_topanga<-
  mod_sel(reg_df, ndsi, "topanga", "night", "summer", diversity_indices)
summary(mod_ndsi_topanga)

mod_simpson_topanga<-
  mod_sel(reg_df, simpson, "topanga", "night", "summer", diversity_indices)
summary(mod_simpson_topanga)

mod_shannon_topanga<-
  mod_sel(reg_df, shannon, "topanga", "night", "summer", diversity_indices)
summary(mod_shannon_topanga)

mod_cricket_topanga<-
  mod_sel(reg_df, cricket_avg_richness, "topanga", "night", "summer", diversity_indices)
summary(mod_cricket_topanga)

mod_katydid_topanga<-
  mod_sel(reg_df, katydid_avg_richness, "topanga", "night", "summer", diversity_indices)
summary(mod_katydid_topanga)

mod_ndvi_topanga<-
  mod_sel_ndvi(averaged, ndvi, "topanga", "night", "summer", diversity_indices)
summary(mod_ndvi_topanga)

#Eleanor
mod_bi_eleanor<-
mod_sel(averaged, bi, "eleanor", "night", "summer", diversity_indices)
summary(mod_bi_eleanor)

mod_h_eleanor<-
  mod_sel(averaged, h, "eleanor", "night", "summer", diversity_indices)
summary(mod_h_eleanor)

mod_ndsi_eleanor<-
  mod_sel(averaged, ndsi, "eleanor", "night", "summer", diversity_indices)
summary(mod_ndsi_eleanor)

mod_simpson_eleanor<-
  mod_sel(averaged, simpson, "eleanor", "night", "summer", diversity_indices)
summary(mod_simpson_eleanor)

mod_shannon_eleanor<-
  mod_sel(averaged, shannon, "eleanor", "night", "summer", diversity_indices)
summary(mod_shannon_eleanor)

mod_cricket_eleanor<-
  mod_sel(averaged, cricket_avg_richness, "eleanor", "night", "summer", diversity_indices)
summary(mod_cricket_eleanor)

mod_katydid_eleanor<-
  mod_sel(averaged, katydid_avg_richness, "eleanor", "night", "summer", diversity_indices)
summary(mod_katydid_eleanor)

mod_ndvi_eleanor<-
  mod_sel_ndvi(averaged, ndvi, "eleanor", "night", "summer", diversity_indices)
summary(mod_ndvi_eleanor)

#Rancho Sierra Vista
mod_bi_rancho<-
  mod_sel_rancho(averaged, bi, "rancho", "night", "summer", diversity_indices)
summary(mod_bi_rancho)

mod_h_rancho<-
  mod_sel_rancho(averaged, h, "rancho", "night", "summer", diversity_indices)
summary(mod_h_rancho)

mod_ndsi_rancho<-
mod_sel_rancho(averaged, ndsi, "rancho", "night", "summer", diversity_indices)
summary(mod_ndsi_rancho)

mod_simpson_rancho<-
  mod_sel_rancho(averaged, simpson, "rancho", "night", "summer", diversity_indices)
summary(mod_simpson_rancho)

mod_shannon_rancho<-
  mod_sel_rancho(averaged, shannon, "rancho", "night", "summer", diversity_indices)
summary(mod_shannon_rancho)

mod_cricket_rancho<-
  mod_sel_rancho(averaged, cricket_avg_richness, "rancho", "night", "summer",
                 diversity_indices)
summary(mod_cricket_rancho)

mod_katydid_rancho<-
  mod_sel_rancho(averaged, katydid_avg_richness, "rancho", "night", "summer",
          diversity_indices)
summary(mod_katydid_rancho)

mod_ndvi_rancho<-
  mod_sel_ndvi_rancho(averaged, ndvi, "rancho", "night", "summer",
                 diversity_indices)
summary(mod_ndvi_rancho)

#biodiversity plots

plot_df <-
  averaged %>%
  filter(season == "summer",
         day_night == "night") %>% 
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(diversity_indices) %>% 
  mutate(last_fire = as.numeric(last_fire))

#ndvi
ndvi_smooth_plot <-
  plot_df %>% 
  ggplot(aes(last_fire, ndvi, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "topanga"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#66BD6B"))+
  labs(y = "NDVI", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.22), 
                           "inches"),
        plot.background = element_rect(fill = "#e5f5e0"))

#cricket richness
cricket_smooth_plot <-
  plot_df %>% 
  ggplot(aes(last_fire, cricket_avg_richness, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality == "topanga"),
               aes(fill = locality),
               alpha = 0.15,
               method = "lm", linewidth = 1) +
  scale_fill_manual(values = "#3E71CF")+
  labs(y = "Mean cricket richness", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.22), 
                           "inches"),
        plot.background = element_rect(fill = "#fee0d2"))

#katydid richness
katydid_smooth_plot <-
  plot_df %>% 
  ggplot(aes(last_fire, katydid_avg_richness, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality == "eleanor"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3)) +
  scale_fill_manual(values = c("#DD4444"))+
  labs(y = "Mean katydid richness", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.21), 
                           "inches"),
        plot.background = element_rect(fill = "#fee0d2"))

#Simpson
simpson_smooth_plot <-
plot_df %>% 
  ggplot(aes(last_fire, simpson, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "rancho"),
    aes(fill = locality),
              alpha = 0.15,
              method = "lm", linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#3E71CF"))+
  labs(y = "Simpson", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.17), 
                           "inches"),
        plot.background = element_rect(fill = "#dadaeb"))
#Shannon
shannon_smooth_plot <-
plot_df %>% 
  ggplot(aes(last_fire, shannon, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality == "topanga"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = "#3E71CF")+
  labs(y = "Shannon", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw()+
theme(legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x=element_blank(),
      plot.margin = unit(c(0.05, 0.1, 0.05, 0.16), 
                         "inches"),
      plot.background = element_rect(fill = "#dadaeb"))

#acoustic indices plots

#BI
bi_smooth_plot <-
plot_df %>% 
  ggplot(aes(last_fire, bi, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "rancho"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#3E71CF"))+
  labs(y = "BI", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.34), 
                           "inches"),
        plot.background = element_rect(fill = "#fff7bc"))

#H
h_smooth_plot <-
plot_df %>% 
  ggplot(aes(last_fire, h, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "rancho"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#3E71CF"))+
  labs(y = "H", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.1), 
                           "inches"),
        plot.background = element_rect(fill = "#fff7bc"))

#NDSI
ndsi_smooth_plot <-
plot_df %>% 
  ggplot(aes(last_fire, ndsi, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "rancho"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#3E71CF"))+
  labs(y = "NDSI", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.05, 0.1, 0.05, 0.23), 
                           "inches"),
        plot.background = element_rect(fill = "#fff7bc"))

#extract legend for grid
grid_legend <-
  plot_df %>% 
  ggplot(aes(last_fire, ndsi, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF"),
                     labels = c("Lake Eleanor Open Space",
                                "Circle X Ranch - Rancho Sierra Vista/ Satwiwa",
                                "Topanga State Park")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "rancho"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#3E71CF"))+
  labs(y = "NDSI", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  labs(color = "Locality", shape = "Vegetation unit") +
  guides(fill = "none") +
  theme(
  legend.text=element_text(size=8))

# extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)}

grid_legend <- extract_legend(grid_legend)

x_title <- expression("Last Wildfire")

grid.arrange(
  arrangeGrob(
  cricket_smooth_plot,
  katydid_smooth_plot,
  simpson_smooth_plot,
  shannon_smooth_plot,
  bi_smooth_plot,
  h_smooth_plot,
  ndsi_smooth_plot,
  ndvi_smooth_plot,
  bottom = text_grob(expression("Last Wildfire"), size = 1),
  ncol = 2),
  grid_legend,
  ncol = 2,
  widths=c(3,1.2))

#path analysis
library(lavaan)

#general models
path_df<-
averaged %>% 
  filter(season == "summer", day_night == "night") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(diversity_indices) %>% 
  mutate(elevation = rescale(elevation), 
         temp = rescale(temp),
         slope = rescale(slope),
         aspect = aspect,  
         ndvi = rescale(ndvi),
         last_fire = rescale(as.numeric(last_fire)),
         cricket_avg_richness = rescale(cricket_avg_richness),
         katydid_avg_richness = rescale(katydid_avg_richness),
         simpson = rescale(simpson),
         shannon = rescale(shannon),
         bi = rescale(bi),
         h = rescale(h),
         ndsi = rescale(ndsi)) %>% 
  select(locality, last_fire, elevation, temp,
                    slope, vegetation, ndvi,
         aspect, cricket_avg_richness, katydid_avg_richness, simpson, shannon,
         bi, ndsi, h) 
  
#specify model

#locality
specmod <- '
#equations
locality ~ aspect + ndvi
temp ~ elevation + ndvi
ndvi ~ elevation
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
#covariates of exogenous variables
#residual variances for endogenous variables
locality~~locality
temp~~temp
ndvi~~ndvi
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#cricket and katydid richness
specmod <- '
#equations
katydid_avg_richness ~ last_fire + ndvi + vegetation
cricket_avg_richness ~ temp + vegetation
temp ~ elevation + ndvi
vegetation ~ last_fire
ndvi ~ elevation + slope + locality
#variances of exogenous variables
elevation~~elevation
last_fire~~last_fire
slope~~slope
locality~~locality
#covariates of exogenous variables
elevation~~slope
locality~~elevation
#residual variances for endogenous variables
vegetation~~vegetation
temp~~temp
ndvi~~ndvi
cricket_avg_richness~~cricket_avg_richness
katydid_avg_richness~~katydid_avg_richness
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#biodiversity indices
specmod <- '
#equations
shannon ~ last_fire + ndvi + temp + vegetation
simpson ~ last_fire + ndvi + temp + vegetation
temp ~ elevation + ndvi
vegetation ~ last_fire
ndvi ~ elevation + slope + locality
#variances of exogenous variables
elevation~~elevation
last_fire~~last_fire
slope~~slope
locality~~locality
#covariates of exogenous variables
elevation~~slope
locality~~elevation
#residual variances for endogenous variables
vegetation~~vegetation
temp~~temp
ndvi~~ndvi
shannon~~shannon
simpson~~simpson
'
fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#acoustic indices
specmod <- '
#equations
h ~ elevation + ndvi + temp
bi ~ elevation + last_fire + ndvi + temp
ndsi ~ elevation + ndvi + temp + vegetation
temp ~ elevation + ndvi
vegetation ~ last_fire
ndvi ~ elevation + slope + locality
#variances of exogenous variables
elevation~~elevation
last_fire~~last_fire
slope~~slope
locality~~locality
#covariates of exogenous variables
elevation~~slope
locality~~elevation
#residual variances for endogenous variables
vegetation~~vegetation
temp~~temp
ndvi~~ndvi
h~~h
bi~~bi
ndsi~~ndsi
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#path plot
library(lavaanPlot)
lavaanPlot(model = fitmod,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"),
           coefs = TRUE,covs= TRUE,
           stars = c("regress"))


#by locality

#topanga
path_df<-
  averaged %>% 
  filter(season == "summer", day_night == "night", locality == "topanga") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(diversity_indices) %>% 
  mutate(elevation = rescale(elevation), 
         temp = rescale(temp),
         slope = rescale(slope),
         aspect = aspect,  
         ndvi = rescale(ndvi),
         last_fire = rescale(as.numeric(last_fire)),
         cricket_avg_richness = rescale(cricket_avg_richness),
         katydid_avg_richness = rescale(katydid_avg_richness),
         simpson = rescale(simpson),
         shannon = rescale(shannon),
         bi = rescale(bi),
         h = rescale(h),
         ndsi = rescale(ndsi)) %>% 
  select(last_fire, elevation, temp,
         slope, vegetation, ndvi,
         aspect, cricket_avg_richness, katydid_avg_richness, simpson, shannon,
         bi, ndsi, h) 

#specify model

#cricket and katydid richness
specmod <- '
#equations
katydid_avg_richness ~ slope
cricket_avg_richness ~ vegetation
temp ~ elevation
vegetation ~ last_fire
ndvi ~ elevation + vegetation
#variances of exogenous variables
elevation~~elevation
last_fire~~last_fire
slope~~slope
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
temp~~temp
ndvi~~ndvi
cricket_avg_richness~~cricket_avg_richness
katydid_avg_richness~~katydid_avg_richness
'
fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#biodiversity indices ## did not show relationship
specmod <- '
#equations
simpson ~ vegetation
shannon ~ elevation + slope + aspect
temp ~ elevation
vegetation ~ last_fire
ndvi ~ elevation + vegetation
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
slope~~slope
#covariates of exogenous variables
elevation~~slope
#residual variances for endogenous variables
vegetation~~vegetation
temp~~temp
ndvi~~ndvi
simpson~~simpson
shannon~~shannon
'

#acoustic indices
specmod <- '
#equations
bi ~ vegetation + last_fire + temp
h ~ last_fire
ndsi ~ temp + vegetation + slope
temp ~ elevation
vegetation ~ last_fire
ndvi ~ elevation + vegetation
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
slope~~slope
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
temp~~temp
ndvi~~ndvi
bi~~bi
h~~h
ndsi~~ndsi
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)


#eleanor
path_df<-
  averaged %>% 
  filter(season == "summer", day_night == "night", locality == "eleanor") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(diversity_indices) %>% 
  mutate(elevation = rescale(elevation), 
         temp = rescale(temp),
         slope = rescale(slope),
         aspect = aspect,  
         ndvi = rescale(ndvi),
         last_fire = rescale(as.numeric(last_fire)),
         cricket_avg_richness = rescale(cricket_avg_richness),
         katydid_avg_richness = rescale(katydid_avg_richness),
         simpson = rescale(simpson),
         shannon = rescale(shannon),
         bi = rescale(bi),
         h = rescale(h),
         ndsi = rescale(ndsi)) %>% 
  select(last_fire, elevation, temp,
         slope, vegetation, ndvi,
         aspect, cricket_avg_richness, katydid_avg_richness, simpson, shannon,
         bi, ndsi, h) 


#specify model
#cricket and katydid richness

specmod <- '
#equations
katydid_avg_richness ~ temp + vegetation + ndvi
cricket_avg_richness ~ temp
vegetation ~ elevation + last_fire
ndvi ~ last_fire + vegetation + aspect
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
temp~~temp
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
ndvi~~ndvi
cricket_avg_richness~~cricket_avg_richness
katydid_avg_richness~~katydid_avg_richness
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)


#biodiversity indices
specmod <- '
#equations
simpson ~ ndvi + temp
shannon ~ ndvi + temp
vegetation ~ elevation + last_fire
ndvi ~ last_fire + vegetation + aspect
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
temp~~temp
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
ndvi~~ndvi
simpson~~simpson
shannon~~shannon
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#acoustic indices bi
specmod <- '
#equations
bi ~ aspect + last_fire + ndvi + temp
vegetation ~ elevation + last_fire
ndvi ~ last_fire + vegetation + aspect
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
temp~~temp
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
ndvi~~ndvi
bi~~bi
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#acoustic indices h
specmod <- '
#equations
h ~ elevation + last_fire + ndvi + temp
vegetation ~ elevation + last_fire
ndvi ~ last_fire + vegetation + aspect
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
temp~~temp
slope~~slope
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
ndvi~~ndvi
h~~h
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#acoustic indices ndsi
specmod <- '
#equations
ndsi ~ aspect + elevation + last_fire + ndvi + temp
vegetation ~ elevation + last_fire
ndvi ~ last_fire + vegetation + aspect
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
temp~~temp
slope~~slope
#covariates of exogenous variables
#residual variances for endogenous variables
vegetation~~vegetation
ndvi~~ndvi
ndsi~~ndsi
'


fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#rancho
path_df<-
  averaged %>% 
  filter(season == "summer", day_night == "night", locality == "rancho") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(diversity_indices) %>% 
  mutate(elevation = rescale(elevation), 
         temp = rescale(temp),
         slope = rescale(slope),
         aspect = aspect,  
         ndvi = rescale(ndvi),
         last_fire = rescale(as.numeric(last_fire)),
         cricket_avg_richness = rescale(cricket_avg_richness),
         katydid_avg_richness = rescale(katydid_avg_richness),
         simpson = rescale(simpson),
         shannon = rescale(shannon),
         bi = rescale(bi),
         h = rescale(h),
         ndsi = rescale(ndsi)) %>% 
  select(last_fire, elevation, temp,
         slope, vegetation, ndvi,
         aspect, cricket_avg_richness, katydid_avg_richness, simpson, shannon,
         bi, ndsi, h) 

#specify model
#cricket and katydid richness

#temp + vegetation + ndvi + last_fire + slope + elevation + aspect

specmod <- '
#equations
katydid_avg_richness ~ temp + vegetation + ndvi + last_fire + slope
cricket_avg_richness ~ vegetation + last_fire + aspect + elevation + slope
ndvi ~ temp + vegetation + elevation + aspect
temp ~ elevation
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
slope~~slope
vegetation~~vegetation
#covariates of exogenous variables
#residual variances for endogenous variables
temp~~temp
ndvi~~ndvi
cricket_avg_richness~~cricket_avg_richness
katydid_avg_richness~~katydid_avg_richness
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#biodiversity indices
specmod <- '
#equations
simpson ~ temp + vegetation + ndvi + last_fire + aspect + elevation + slope
shannon ~ temp + vegetation + ndvi + last_fire + aspect + elevation + slope
ndvi ~ temp + vegetation + elevation + aspect
temp ~ elevation
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
slope~~slope
vegetation~~vegetation
#covariates of exogenous variables
#residual variances for endogenous variables
temp~~temp
ndvi~~ndvi
simpson~~simpson
shannon~~shannon
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#acoustic indices
specmod <- '
#equations
h ~ aspect
ndvi ~ temp + vegetation + elevation + aspect
temp ~ elevation
#variances of exogenous variables
aspect~~aspect
elevation~~elevation
last_fire~~last_fire
slope~~slope
vegetation~~vegetation
#covariates of exogenous variables
#residual variances for endogenous variables
temp~~temp
ndvi~~ndvi
bi~~bi
h~~h
ndsi~~ndsi
'

fitmod <- lavaan(specmod, data = path_df)
fitmod

summary(fitmod, fit.measures = T, standardized = T, rsquare = T)

#temp + vegetation + ndvi + last_fire + aspect + elevation + slope

#bootstrap
library(boot)

bootReg <- function (formula, data, i)
{
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults<-
  reg_df %>% 
  filter(season == "summer",
         day_night == "night",
         locality == "topanga") %>% 
  boot(statistic = bootReg,
                  formula = index ~ years_last_fire,
                  data = ., R = 10000)

boot.ci(bootResults, type = "bca", index = 1)


plot_df %>% 
  ggplot(aes(last_fire, cricket_avg_richness, color = locality)) +
  geom_point(aes(shape=vegetation)) +
  scale_color_manual(values = c("#DD4444", "#66BD6B", "#3E71CF")) +
  scale_shape_manual(values=c(15,16,17,2,6,8,3,4,13,18)) +
  geom_smooth( data = plot_df %>% filter(locality != "rancho"),
               aes(fill = locality),
               alpha = 0.15,
               method = "gam", formula = y ~ s(x, k = 3), linewidth = 1) +
  scale_fill_manual(values = c("#DD4444", "#3E71CF"))+
  labs(y = "Simpson", x = "Last fire") +
  scale_x_reverse() +
  scale_x_reverse(limits = c(2022, 1900)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0.17), 
                           "inches"),
        panel.background = element_rect(colour = "gray",
                                        size = 1.5, linetype = "solid"))

#standard deviations of selcted acoustic indices

seas <- "summer"
diel <- "day"

day <-
  averaged %>%
  filter(season == seas,
         day_night == diel) %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  summarize(ndsi = mean(sd_ndsi), h = mean(sd_h), bi = mean(sd_bi), .by = locality)

night <-
  averaged %>%
  filter(season == seas,
         day_night == "night") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  summarize(ndsi = mean(sd_ndsi), h = mean(sd_h), bi = mean(sd_bi), .by = locality)


#averages acoustic indices

seas <- "fall"

day <-
  averaged %>%
  filter(season == seas,
         day_night == "day") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  summarize(ndsi = mean(ndsi), h = mean(h), bi = mean(bi), .by = locality)

night <-
  averaged %>%
  filter(season == seas,
         day_night == "night") %>% 
  mutate(last_fire = as.numeric(last_fire)) %>%
  mutate(recorder_id = as.character(recorder_id)) %>% 
  summarize(ndsi = mean(ndsi), h = mean(h), bi = mean(bi), .by = locality)

