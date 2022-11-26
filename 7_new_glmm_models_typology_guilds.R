#In this script I will join the typology data to the abundance data and create new
#GLMM models based on site-river typology.

library(tidyverse)
library(lme4)
rm(list=ls())

type<-readRDS("gcb_typology.rds") 
guild_ab<- readRDS("gcb_guilds_typology.rds")
guild_ab<- guild_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology))

guild_list<- c("carnivore", "herbivore", 
               "decomposer")


for (i in guild_list){
  data<- guild_ab %>% filter(guild==i) 
  mod<- glmer(total_abundance ~ year_scaled*typology1 + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0)
  if(isTRUE(class(mod)=="try-error")) {next} else {
    saveRDS(mod, file.path("paste( gsub(" ", "_", i), "mod.rds", sep="_")))}
}
