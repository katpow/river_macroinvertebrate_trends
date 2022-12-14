#In this script I will join the typology data to the abundance data and create new
#GLMM models based on site-river typology for broader taxonomic groups


library(tidyverse)
library(lme4)
rm(list=ls())

type<-readRDS("gcb_typology.rds") 
group_ab<- readRDS("gcb_fwmacro_typology.rds") 


#Join the data and then create models 

group_ab<- group_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology)) 


taxon_list<- c("ephemeroptera", 
               "coleoptera", 
               "trichoptera",
               "diptera", 
               "mollusc",
               "crustacean", 
  "annelid", "hemiptera", "megaloptera", "odonata", "plecoptera", "turbellaria")


for (i in taxon_list){
  data<- group_ab %>% filter(taxon==i)
  mod<- glmer(total_abundance ~ year_scaled*typology1 + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
  if(isTRUE(class(mod)=="try-error")) {next} else {
    saveRDS(mod, file.path("paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
}






