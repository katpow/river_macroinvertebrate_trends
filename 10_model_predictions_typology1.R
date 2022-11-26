#Group model predictions - JASMIN
#Getting the model predictions for each group model

#Try it out with one of the taxa I've made models for 
library(lme4)
library(tidyverse)
library(effects)
library(ggeffects)
library(tools)

#Read in the necessary data 
rm(list=ls())
setwd("C:/Users/katpow/OneDrive - UKCEH/Chapter 2")
type<-readRDS("manipulated_data/typology_final.rds") 
three_group<- readRDS("manipulated_data/3_year_filter_typology.rds") 
three_guild<- readRDS("manipulated_data/3_year_filter_guild_typology.rds")
#Join the data 
three_group<- three_group %>% left_join(type, by="site_id") %>% filter(!is.na(typology1)) 
three_guild<- three_guild %>% left_join(type, by="site_id") %>% filter(!is.na(typology1))

three_group$site_id<- as.factor(three_group$site_id)
three_group$typology1<- as.factor(three_group$typology1)
year_add<- three_group %>% ungroup()%>% distinct(year, year_scaled) %>% mutate(year=as.factor(year))
three_guild$site_id<- as.factor(three_guild$site_id)
three_guild$typology1<- as.factor(three_guild$typology1)

length(unique(three_group$typology1))
#Now we want to get a list of the model taxa to read into the loop we will make to create the trends and comparisons
#taxon<- list.files("models/three_year_filter/poisson_group") %>% 
#  as.data.frame() %>% 
#  dplyr::rename(taxon= ".") %>%
#  separate(taxon, c("taxon", "delete"), sep="_") %>% 
#  dplyr::select(taxon)
#taxon<- unique(taxon$taxon)
##i<-"coleoptera"
taxon<-c("annelid", "coleoptera", "crustacean", "diptera", "ephemeroptera", "hemiptera", "megaloptera", "mollusc", "odonata", "plecoptera", "trichoptera", "turbellaria")
guild<- c("carnivore", "herbivore", "decomposer")
year_scaled1<- unique(three_group$year_scaled)
year_scaled1

year_ty_effect<- data.frame()


for(i in taxon){
  data<- three_group %>% filter(taxon==i)
  mod<- readRDS(file.path("models/three_year_filter/typology1_glmm", paste( gsub(" ", "_", i), "mod.rds", sep="_")))
  temp_ty_effect<-as.data.frame(ggpredict(mod, terms=c("year_scaled[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]", "typology1"))) %>% arrange(desc(group)) %>% rename(typology1=group) %>% mutate(year=rep(c(2002:2019), 6), taxon=i) %>% dplyr:: select(taxon, typology1, year, predicted:conf.high)
  year_ty_effect<- rbind(year_ty_effect, temp_ty_effect) 
}
head(year_ty_effect)
saveRDS(year_ty_effect, "models/three_year_filter/predictions/year_ty1_effect.rds")

year_effect<-data.frame()
year_ty_effect<- data.frame()
for(i in guild){
  data<- three_guild %>% filter(guild==i)
  mod<- readRDS(file.path("models/three_year_filter/typology1_glmm", paste( gsub(" ", "_", i), "mod.rds", sep="_")))
  temp_ty_effect<-as.data.frame(ggpredict(mod, terms=c("year_scaled[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]", "typology1"))) %>% arrange(desc(group)) %>% rename(typology1=group) %>% mutate(year=rep(c(2002:2019), 6), taxon=i) %>% dplyr:: select(taxon, typology1, year, predicted:conf.high)
  year_ty_effect<- rbind(year_ty_effect, temp_ty_effect)
}

saveRDS(year_ty_effect, "models/three_year_filter/predictions/year_ty1_effect_guild.rds")
year_ty_effect
