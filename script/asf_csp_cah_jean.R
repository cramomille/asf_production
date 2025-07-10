library(tidyverse)
library(sf) # pour traiter les "spatial features"
library(FactoMineR) # pour les AGD + CAH
library(factoextra) 
library(RColorBrewer) # pour les palettes de couleurs
library(Factoshiny)
library(mapsf)
library(MetBrewer)

#Import des données

load("C:/Documents and Settings/riviere-j-1/Desktop/test typo oposom/2503/2503_donneesCASD_IRISr.RData")
setwd("C:/Documents and Settings/riviere-j-1/Desktop/test typo oposom/2503/")

#Cartographie de la part des CS

sf.irisr.d |> filter(OM_CODE=="FXX") |> 
  left_join(d.irisr.d, by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=p.CS3_37),color=NA) +
  scale_fill_viridis_c() +
  theme_void()

#ACP sur les 42 postes CS3

d.acp <- d.irisr.d |> 
  select(IRISr,starts_with("p.CS3_")) |> 
  rename_with(~ gsub("p.CS3_", "CS", .x, fixed = TRUE)) |> 
  column_to_rownames(var="IRISr")

res.acp <- PCA(d.acp, graph=F)

d.acp.res <- d.acp |> rownames_to_column(var="IRISr") |> 
  left_join(as.data.frame(res.acp$ind$coord) |> 
              rownames_to_column(var="IRISr"), by="IRISr")

#% d’inertie expliquée par chaque axe

fviz_eig(res.acp, addlabels = TRUE)

#Plan factoriel des variables :

fviz_pca_var(res.acp, col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

fviz_pca_var(res.acp, axes=c(3,4), col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

#Coordonnées sur l’Axe 1 :

sf.irisr.d |> filter(OM_CODE=="FXX") |> 
  left_join(d.acp.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=Dim.2), col=NA) +
  scale_fill_gradient2(low="darkblue",high="darkred", mid="white",
                       midpoint=0) +  theme_void()

sf.irisr.d |> filter(OM_CODE=="FXX") |> 
  left_join(d.acp.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=Dim.2), col=NA) +
  scale_fill_gradient2(low="#2874a6",high="#ec7063", mid="white",
                       midpoint=0) +
  theme_void()

sf.irisr.d |> filter(OM_CODE=="FXX") |> 
  left_join(d.acp.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=Dim.3), col=NA) +
  scale_fill_gradient2(low="blue",high="darkred", mid="white",
                       midpoint=0) +
  theme_void()


#CAH sur l’ACP

n.clust <- 15
res.cah <- HCPC(res.acp, ncp=6, consol=F, nb.clust=n.clust, graph=F)

d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var="IRISr") |> 
              select(IRISr,clust), by="IRISr")

#Dendogramme 
  
plot(res.cah, choice="tree", ind.names=F)
  
#Classes sur le plan factoriel :

plot(res.cah, choice="map", ind.names=F, draw.tree=F)

#carto des classes avec palette Aliette
sf.irisr.d |> filter(OM_CODE=="FXX") |> 
  left_join(d.acp.cah.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=clust),color=NA) +
  scale_fill_manual(values=brewer.pal(n.clust,"Paired")) +
  theme_void()

#carto des classes avec palette manuelle
sf.irisr.d |> filter(OM_CODE=="FXX") |> 
left_join(d.acp.cah.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=clust),color=NA) +
  scale_fill_manual(values=c("red4", "red2", "darkorange2", "orange", "gold", "yellow", "greenyellow","aquamarine3", "paleturquoise", "skyblue2", "darkolivegreen4", "dodgerblue3", "darkolivegreen2", "darkorchid1", "purple4"))+ theme_void()

#carto des classes avec palette MET
sf.irisr.d |> filter(OM_CODE=="FXX") |> 
  left_join(d.acp.cah.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=clust),color=NA) +
  scale_fill_manual(values=met.brewer("Troy", n=15, type="continuous")) +
  theme_void()

#zoom sur une AAV avec palette MET
sf.irisr.d |> 
  semi_join(d.irisr.app |> filter(AAV2020=="008"),by="IRISrD_CODE") |> 
  left_join(d.acp.cah.res,by=c("IRISrD_CODE"="IRISr")) |> 
  ggplot() + geom_sf(aes(fill=clust)) +
  scale_fill_manual(values=met.brewer("Troy", n=15, type="continuous")) +
  theme_void()

#Profils des classes :

moy_var_ens <- d.acp.cah.res %>% select(-clust) |> 
  pivot_longer (cols=starts_with("CS"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")
profils <- d.acp.cah.res %>%   
  pivot_longer (cols=starts_with("CS"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=15, type="continuous")) +
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")


write.table(res.cah$data.clust, file="TableTypo15.csv",
            sep=";",dec=",",row.names=F)




  