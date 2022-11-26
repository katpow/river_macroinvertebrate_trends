#Working out the annual growth rates and total percentage changes for each taxa / typology comnbo (and total)

library(tidyverse)

rm(list=ls())
setwd("C:/Users/katpow/OneDrive - UKCEH/Chapter 2")

yearty_pred<- readRDS("models/three_year_filter/predictions/year_ty1_effect.rds") %>% rename(group=taxon)
yearty_guilds<- readRDS("models/three_year_filter/predictions/year_ty1_effect_guild.rds") %>% rename(group=taxon)

yearty_pred

per_change<- function(x){
  (((last(x)-first(x))/first(x))*100)
}
agr<-function(x){
  per_change(x)/length(x)
}



agr_ty<- yearty_pred %>% group_by(group, typology) %>% 
  summarise(per_change=per_change(predicted), agr=agr(predicted))
agr_ty_g<- yearty_guilds %>% group_by(group, typology) %>% 
  summarise(per_change=per_change(predicted), agr=agr(predicted)) %>% rbind(agr_ty)
head(agr_ty_g)


#Save them
saveRDS(agr_ty_g, "models/three_year_filter/predictions/agr_ty1.rds")

