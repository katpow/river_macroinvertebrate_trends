#This script is for using emmeans package and emtrends function to test for significant differences between typology
#For the different group models

library(lme4)
library(tidyverse)
library(emmeans)
library(tools)

#Read in the necessary data 
rm(list=ls())
setwd("/home/users/katpow")
setwd("C:/Users/katpow/OneDrive - UKCEH/Chapter 2")
three_group<- readRDS("manipulated_data/3_year_filter_typology.rds")
three_guild<- readRDS("manipulated_data/3_year_filter_guild_typology.rds")
##Abandon the below chunk for now - want to narrow down the taxa 
##Now we want to get a list of the model taxa to read into the loop we will make to create the trends and comparisons
#taxon<- list.files("models/three_year_filter/poisson_group") %>% 
#  as.data.frame() %>% 
#  dplyr::rename(taxon= ".") %>%
#  separate(taxon, c("taxon", "delete"), sep="_") %>% 
#  dplyr::select(taxon)
#taxon<- unique(taxon$taxon)
##i<-"coleoptera"

#Make a taxon list
taxon<-c("annelid", "coleoptera", "crustacean", "diptera", "ephemeroptera", "hemiptera", "megaloptera", "mollusc", "odonata", "plecoptera", "trichoptera", "turbellaria")

#set up empty dataframes
trends_results<- data.frame()
compare_results<- data.frame()

#Create loop which reads in the model, carries out the emtrends() to calculate trends and contrasts for typologys, 
#creates a dataframe of the trends for each typology
#And binds it to a dataframe, labelling it with the taxon name 
#And then creates a dataframe of the pairwise contrasts between typology trends, and then
#binds it to a dataframe whilst labelling it with the taxon name 
i<-"annelid"
for(i in taxon){
  mod<- readRDS(file.path("models/three_year_filter/typology1_glmm", paste( gsub(" ", "_", i), "mod.rds", sep="_")))
  data<- three_group %>% filter(taxon==i)
  trends_compare<-emtrends(mod, pairwise~"typology1", var="year_scaled", infer=TRUE)
  temp_trends<-as.data.frame(trends_compare$emtrends)%>% mutate(taxon=i)
  trends_results<- rbind(trends_results, temp_trends)
  temp_compare<- as.data.frame(trends_compare$contrasts) %>% mutate(taxon=i)
  compare_results<- rbind(compare_results, temp_compare)
}

guild<- c("carnivore", "herbivore", "decomposer")
#And now for the guild models 
for(i in guild){
  mod<- readRDS(file.path("models/three_year_filter/typology1_glmm", paste( gsub(" ", "_", i), "mod.rds", sep="_")))
  data<- three_guild %>% filter(guild==i)
  trends_compare<-emtrends(mod, pairwise~"typology1", var="year_scaled", infer=TRUE)
  temp_trends<-as.data.frame(trends_compare$emtrends)%>% mutate(taxon=i)
  trends_results<- rbind(trends_results, temp_trends)
  temp_compare<- as.data.frame(trends_compare$contrasts) %>% mutate(taxon=i)
  compare_results<- rbind(compare_results, temp_compare)
}
head(trends_results)

trends_results[,2:8]<- round(trends_results[,2:8], digits=3)
trends_results<- trends_results%>%
  mutate(p=p.value) %>%
  mutate(p=replace(p, p<=0.001, "<0.001"))%>%
  mutate(p=replace(p, p>0.001 & p<=0.01, "<0.01"))%>%
  mutate(p=replace(p, p>0.01 & p<=0.05, "<0.05"))%>%
  dplyr::rename(typology=typology1, group=9, slope=2, slope_se=SE, slope_p=p)%>%
  dplyr::select(group, typology, slope, slope_se, slope_p) %>%
  separate(typology, c("Dominant_Geology", "Mean_Altitude"))%>%
  mutate(group=toTitleCase(group), Dominant_Geology=toTitleCase(Dominant_Geology),Mean_Altitude=toTitleCase(Mean_Altitude))
names(trends_results)<-toTitleCase(names(trends_results))
head(trends_results)
tail(trends_results)


head(compare_results)
compare_results[,2:8]<- round(compare_results[,2:8], digits=3)
compare_results<- compare_results%>%mutate(p=p.value) %>%
  mutate(p=replace(p, p<=0.001, "<0.001"))%>%
  mutate(p=replace(p, p>0.001 & p<=0.01, "<0.01"))%>%
  mutate(p=replace(p, p>0.01 & p<=0.05, "<0.05"))%>%
  dplyr::rename(group=9)%>%
  dplyr::select(9, 1:3, 10)%>%
  mutate(contrast= str_replace_all(contrast, "_", " ")) %>%
  mutate(contrast=toTitleCase(contrast), group=toTitleCase(group))
names(compare_results)<-toTitleCase(names(compare_results))
head(compare_results)


#Save my results
head(trends_results)
saveRDS(compare_results, "models/three_year_filter/typology1_emtrends/typology1_contrasts.rds")
saveRDS(trends_results, "models/three_year_filter/typology1_emtrends/typology1_trends.rds")

head(compare_results)
tail(compare_results)
head(trends_results)
tail(trends_results)

#i<- "annelid"


###############
####Getting the values for the intercepts to fill out my table now

intercept_results<- data.frame()
for(i in taxon){
  mod<- readRDS(file.path("models/three_year_filter/typology1_glmm", paste( gsub(" ", "_", i), "mod.rds", sep="_")))
  data<- three_group %>% filter(taxon==i)
  temp_intercept<-as.data.frame(emmeans(mod, pairwise~"typology1", cov.reduce=function(x) 0, infer=TRUE)$emmeans)%>% mutate(group=i)%>%
    select(9,1,2,3)%>%
    dplyr:: rename(intercept=emmean, intercept_se=SE, typology=typology1)
  intercept_results<- rbind(intercept_results, temp_intercept)
}
intercept_results
guild<- c("carnivore", "herbivore", "decomposer")
#And now for the guild models 
for(i in guild){
  mod<- readRDS(file.path("models/three_year_filter/typology1_glmm", paste( gsub(" ", "_", i), "mod.rds", sep="_")))
  data<- three_guild %>% filter(guild==i)
  temp_intercept<-as.data.frame(emmeans(mod, pairwise~"typology1", cov.reduce=function(x) 0, infer=TRUE)$emmeans)%>% mutate(group=i)%>%
    select(9,1,2,3)%>%
    dplyr:: rename(intercept=emmean, intercept_se=SE, typology=typology1)
  intercept_results<- rbind(intercept_results, temp_intercept)
}


intercept_results[,3:4]<- round(intercept_results[,3:4], digits=3)
names(intercept_results)<-toTitleCase(names(intercept_results))
intercept_results<- intercept_results %>%
  separate(Typology, c("Dominant_Geology", "Mean_Altitude"))%>%
  mutate(Group=toTitleCase(Group), Dominant_Geology=toTitleCase(Dominant_Geology),Mean_Altitude=toTitleCase(Mean_Altitude))


head(intercept_results)
tail(intercept_results)

#Can I left join by two columns?
table2<- intercept_results %>% left_join(trends_results, by=c("Group", "Dominant_Geology", "Mean_Altitude"))
head(table2)

saveRDS(table2, "models/three_year_filter/typology1_emtrends/typology1_intercept_and_trends.rds")
