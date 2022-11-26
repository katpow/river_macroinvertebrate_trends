#In this script I will join the typology data to the abundance data and create new
#GLMM models based on site-river typology.
#JASMIN script

library(tidyverse)
library(lme4)
rm(list=ls())
setwd("/home/users/katpow/chapter2")

#setwd("C:/Users/katpow/OneDrive - UKCEH/Chapter 2/manipulated_data")
type<-readRDS("typology_final.rds") 
guild_ab<- readRDS("3_year_filter_guild_typology.rds")
guild_ab<- guild_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology))

guild_list<- c("carnivore", "herbivore", 
               "decomposer")

#for (i in guild_list){
#  data<- guild_ab %>% filter(guild==i)
#  mod<- glmer(total_abundance ~ year_scaled*typology + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
#  if(isTRUE(class(mod)=="try-error")) {next} else {
#    saveRDS(mod, file.path("ttypology_models/typology", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
#}

for (i in guild_list){
  data<- guild_ab %>% filter(guild==i) 
  mod<- glmer(total_abundance ~ year_scaled*typology1 + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
  if(isTRUE(class(mod)=="try-error")) {next} else {
    saveRDS(mod, file.path("typology_models/typology1", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
}