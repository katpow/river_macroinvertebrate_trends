#Running year-only models to check whether adding in typology makes a difference to results. 
#Have to do this again because of 'year_scaled' has been changed. 
#JASMIN script

library(tidyverse)
library(lme4)
rm(list=ls())
setwd("/home/users/katpow/chapter2")
#setwd("C:/Users/katpow/OneDrive - UKCEH/Chapter 2/manipulated_data")
type<-readRDS("typology_final.rds") 
group_ab<- readRDS("3_year_filter_typology.rds") 
guild_ab<- readRDS("3_year_filter_guild_typology.rds")

#Join the data and then create models 

group_ab<- group_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology)) 
guild_ab<- guild_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology))


taxon_list<- c("ephemeroptera", 
  "coleoptera", 
  "trichoptera",
  "diptera", 
  "mollusc",
  "crustacean", 
  "annelid", 
  "hemiptera", 
  "megaloptera", 
  "odonata", 
  "plecoptera", 
  "turbellaria")
guild_list<- c("carnivore", "herbivore", "decomposer")

for (i in taxon_list){
  data<- group_ab %>% filter(taxon==i) %>% select(total_abundance, year_scaled, site_id, sample_id)
  mod<- glmer(total_abundance ~ year_scaled + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
  if(isTRUE(class(mod)=="try-error")) {next} else {
    saveRDS(mod, file.path("year_only_group/models", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
}

for (i in guild_list){
  data<- guild_ab %>% filter(guild==i) %>% select(total_abundance, year_scaled, site_id, sample_id)
  mod<- glmer(total_abundance ~ year_scaled + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
  if(isTRUE(class(mod)=="try-error")) {next} else {
    saveRDS(mod, file.path("year_only_group/models", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
}
