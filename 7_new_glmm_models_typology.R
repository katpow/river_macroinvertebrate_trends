#In this script I will join the typology data to the abundance data and create new
#GLMM models based on site-river typology.
#JASMIN script

library(tidyverse)
library(lme4)
rm(list=ls())
setwd("/home/users/katpow/chapter2")

#setwd("C:/Users/katpow/OneDrive - UKCEH/Chapter 2/manipulated_data")
type<-readRDS("typology_final.rds") 
group_ab<- readRDS("3_year_filter_typology.rds") 


#Join the data and then create models 

group_ab<- group_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology)) 



taxon_list<- c("ephemeroptera", 
               "coleoptera", 
               "trichoptera",
               "diptera", 
               "mollusc",
               "crustacean", 
  "annelid", "hemiptera", "megaloptera", "odonata", "plecoptera", "turbellaria")


#for (i in taxon_list){
#data<- group_ab %>% filter(taxon==i)
#mod<- glmer(total_abundance ~ year_scaled*typology + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
#if(isTRUE(class(mod)=="try-error")) {next} else {
#  saveRDS(mod, file.path("typology_models/typology", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
#}

for (i in taxon_list){
  data<- group_ab %>% filter(taxon==i)
  mod<- glmer(total_abundance ~ year_scaled*typology1 + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
  if(isTRUE(class(mod)=="try-error")) {next} else {
    saveRDS(mod, file.path("typology_models/typology1", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
}






