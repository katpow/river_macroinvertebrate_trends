
library(tidyverse)
library(lme4)
rm(list=ls())
setwd("/home/users/katpow/chapter2")


fam_ab<- readRDS("3_year_filter_long_fam_group.rds")
type<-readRDS("typology_final.rds") 
fam_ab<- fam_ab %>% left_join(type, by="site_id") %>% filter(!is.na(typology)) 
taxon_list<- unique(fam_ab$family_taxon)

i=taxon_list[as.numeric(commandArgs(trailingOnly=TRUE))[1]]
print(i)

data<- fam_ab %>% filter(family_taxon==i) %>% select(total_abundance, year_scaled, geology, dfromsrc_cat, mean_alt_cat, site_id, sample_id)

mod<- try(glmer(total_abundance ~ year_scaled + (1|year_scaled) + (year_scaled|site_id) + (1|sample_id), data=data, family=poisson, nAGQ=0))
if(isTRUE(class(mod)=="try-error")) {next} else {
  saveRDS(mod, file.path("year_only_family/models", paste( gsub(" ", "_", i), "mod.rds", sep="_")))}




