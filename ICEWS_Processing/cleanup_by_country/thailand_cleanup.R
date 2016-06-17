# This file cleans the ICEWS data to isolate events between Palestinian dissidents during months that occur during a conflict (or within the chosen window prior to the conflict). It outputs two files to the network_creation directory: one with lists of dates where the nodes are active, and one with the events used to compute edges.

#Import packages and data ----------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(zoo)
library(ggplot2)
theme_set(theme_bw())

#import icews w/ group names added for individuals
icews <- read.csv("ICEWS_Processing/icews_with_groups.csv", stringsAsFactors = FALSE)

#fix dates
icews$date <- ymd(icews$date)
icews$ym <- floor_date(icews$date,"month")

#subset to actors likely to be syrian dissidents
thai <- subset(icews, location=="Thailand")

thai <- subset(thai, (cow.src==800 & agent.src!="GOV"))

#thai <- subset(thai, (cow.src==800 & agent.src!="GOV") | (cow.src!=800 & (agent.src=="REB" | agent.src=="GOV" | agent.src=="OPP")))

#Attach Src Individuals to Groups ---------------------------------------

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
thai <- thai %>%
  mutate(src.groups=strsplit(src.groups,"; ")) %>%
  unnest(src.groups)

#use group name instead of individual name when available
thai$alt.src <- ifelse(is.na(thai$src.groups)==T, thai$alt.src, thai$src.groups)

#if it is a foreign government, collapse to single actor
thai$alt.src <- ifelse(thai$cow.src!=800 & thai$agent.src=="GOV", paste("Government of ",thai$country.src, sep=""), thai$alt.src)

#in some cases the group names from dictionary aren't helpful, so go back to individual name
thai$alt.src <- ifelse(thai$alt.src=="Opposition Major Party (Out Of Government) (Thailand)", thai$name.src, thai$alt.src)
thai$alt.src <- ifelse(thai$alt.src=="Elite (Thailand)", thai$name.src, thai$alt.src)
thai$alt.src <- ifelse(thai$alt.src=="Protestors / Popular Opposition / Mobs (Thailand)", thai$name.src, thai$alt.src)

#remove invalid and non-dissident groups
thai <- subset(thai, alt.src!="Unattributed" & alt.tgt!="Unattributed")
thai <- subset(thai, alt.src!="Chavalit Yongchaiyudh") #PM & various gov positions
thai <- subset(thai, alt.src!="Thammasat University")
thai <- subset(thai, alt.src!="Thai Loves Thai" | date < "2006-09-19") #ruling party
thai <- subset(thai, alt.src!="Minister Kachornprasart")
thai <- subset(thai, alt.src!="Anand Panyarachun") #PM in 90s
thai <- subset(thai, alt.src!="Chuan Leekpai") #PM in 90s
thai <- subset(thai, alt.src!="Surin Pitsuwan") #foreign min & UN official
thai <- subset(thai, alt.src!="Surapong Suebwonglee") #finance min
thai <- subset(thai, (alt.src=="Snoh Thienthong" & date < "1997-11-08")==F)

#standardize group names
thai$alt.src[thai$alt.src=="Media (National Coalition for thaiian Revolutionary and Opposition Forces)"] <- "National Coalition for thaiian Revolutionary and Opposition Forces"

#code individuals that the dictionary didn't get
thai$alt.src[thai$alt.src=="Banyat Bantadtan"] <- "Thai Democrat Party"
thai$alt.src[thai$alt.src=="Suriyasai Katasila"] <- "New Politics Party"
thai$alt.src[thai$alt.src=="Samak Sundaravej"] <- "People's Power Party"
thai$alt.src[thai$alt.src=="Yongyuth Tiyapairat"] <- "People's Power Party"
thai$alt.src[thai$alt.src=="Consumer Services Business (Thailand)" & thai$date < "2005-01-01"] <- "Rak Thailand Party"
thai$alt.src[thai$alt.src=="Consumer Services Business (Thailand)" & thai$date > "2000-01-01"] <- "First Thai Nation"
thai$alt.src[thai$alt.src=="Banharn Silpa-archa"] <- "First Thai Nation"
thai$alt.src[thai$alt.src=="Snoh Thienthong" & date > "2001-01-01" & date < "2006-02-25"] <- "Thai Loves Thai"
thai$alt.src[thai$alt.src=="Snoh Thienthong" & date > "2006-02-24" & date < "2011-05-13"] <- "Pracharaj Party"
thai$alt.src[thai$alt.src=="Snoh Thienthong" & date > "2011-05-12"] <- "Pheu Thai Party"

#split again
thai <- thai %>%
  mutate(alt.src=strsplit(alt.src,"; ")) %>%
  unnest(alt.src)

# get min and max goldstein scores to exclude actors with no material events ------------------------------------------------------
thai <- thai %>% 
  group_by(alt.src) %>%
  mutate(min.gold=min(goldstein),max.gold=max(goldstein))

thai <- subset(thai, cow.src==652 | min.gold < -5 | max.gold > 5)

thai$gov.gold <- ifelse(thai$cow.tgt==652 & thai$agent.tgt=="GOV", thai$goldstein, NA)

sum <- thai %>% 
  group_by(alt.src) %>% 
  summarize(mean=mean(gov.gold,na.rm=T),median=median(gov.gold,na.rm=T),min=min(gov.gold,na.rm=T),max=max(gov.gold,na.rm=T))

#Get active dates for nodes ---------------------------------------------

nodes <- thai

#get goldstein scores for events targeting israeli gov/all israel
nodes$gov.gold <- ifelse(nodes$cow.tgt==666 & nodes$agent.tgt=="GOV", nodes$goldstein, NA)
nodes$isr.gold <- ifelse(nodes$cow.tgt==666, nodes$goldstein, NA)

#weight the material conflict events
nodes$gov.gold <- ifelse(nodes$gov.gold < -5, nodes$gov.gold*3, nodes$gov.gold)
nodes$isr.gold <- ifelse(nodes$isr.gold < -5, nodes$isr.gold*3, nodes$isr.gold)

#summaries for entire group existence
sum <- nodes %>% 
  group_by(alt.src) %>% 
  summarize(mean.gov=mean(gov.gold,na.rm=T),min.gov=min(gov.gold,na.rm=T),med.gov=median(gov.gold,na.rm=T),max.gov=max(gov.gold,na.rm=T), mean.isr=mean(isr.gold,na.rm=T),min.isr=min(isr.gold,na.rm=T),med.isr=median(isr.gold,na.rm=T),max.isr=max(isr.gold,na.rm=T))

sum <- nodes %>% 
  group_by(alt.src) %>% 
  summarize(gov.mat.con=sum(gov.gold < -5, na.rm=T),gov.verb.con=sum(gov.gold > -5 & gov.gold < 0, na.rm=T))


#Attach TGT invididuals to Groups ---------------------------------------

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
thai <- thai %>%
  mutate(tgt.groups = strsplit(tgt.groups,"; ")) %>%
  unnest(tgt.groups)

#use group instead of individual name when available
thai$alt.tgt <- ifelse(is.na(thai$tgt.groups) == T, thai$alt.tgt, thai$tgt.groups)

#switch back to individual names when the dictionary groups aren't useful
thai$alt.tgt <- ifelse(thai$alt.tgt=="Executive Office (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)
thai$alt.tgt <- ifelse(thai$alt.tgt=="Cabinet (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)
thai$alt.tgt <- ifelse(thai$alt.tgt=="Information / Communication / Transparency Ministry (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)
thai$alt.tgt <- ifelse(thai$alt.tgt=="Government (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)
thai$alt.tgt <- ifelse(thai$alt.tgt=="Elite (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)
thai$alt.tgt <- ifelse(thai$alt.tgt=="Foreign Ministry (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)
thai$alt.tgt <- ifelse(thai$alt.tgt=="Human Rights NGOs (Palestinian Territory, Occupied)", thai$name.tgt, thai$alt.tgt)

#standardize group names
thai$alt.tgt[thai$alt.tgt=="Palestinian Territory, Occupied"] <- "Unattributed"
thai$alt.tgt[thai$alt.tgt=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
thai$alt.tgt[thai$alt.tgt=="Popular Front for the Liberation of Palestine – General Command"] <- "Popular Front for the Liberation of Palestine"

#code a few that the dictionary doesn't take care of
thai$alt.tgt[thai$alt.tgt=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
thai$alt.tgt[thai$name.tgt=="Mahmoud Abbas"] <- "Palestine Liberation Organization; Fatah"
thai$alt.tgt[thai$name.tgt=="Ahmed Qureia"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Ismail Haniya"] <- "Hamas"
thai$alt.tgt[thai$name.tgt=="Salam Fayyad" & thai$date < "2005-12-01"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Salam Fayyad" & thai$date > "2005-12-01"] <- "Third Way"
thai$alt.tgt[thai$name.tgt=="Tayyib Abd-al-Rahim"] <- "Fatah"
thai$alt.tgt[thai$alt.tgt=="Majid Faraj"] <- "Palestinian Preventive Security Agency"
thai$alt.tgt[thai$alt.tgt=="Mohammed Deif"] <- "Izz ad-Din al-Qassam Brigades"
thai$alt.tgt[thai$name.tgt=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
thai$alt.tgt[thai$name.tgt=="Rami Hamdallah"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Nabil Abu-Rudaynah"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Riyad al-Malki"] <- "Fatah"
thai$alt.tgt[thai$alt.tgt=="Abdel Razak Al-Yahya"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Yasser Abed Rabbo"] <- "Palestine Liberation Organization; Palestine Democratic Union"
thai$alt.tgt[thai$alt.tgt=="Faisal Husseini"] <- "Palestine Liberation Organization"
thai$alt.tgt[thai$name.tgt=="Nabil Shaath"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Hanan Ashrawi" & thai$date > "2005-12-01"] <- "Third Way"
thai$alt.tgt[thai$name.tgt=="Hanan Ashrawi" & thai$date < "2005-12-02"] <- "Palestinian Legislative Council"
thai$alt.tgt[thai$name.tgt=="Riyad al-Malki"] <- "Fatah"
thai$alt.tgt[thai$name.tgt=="Nabil Abu-Rudaynah"] <- "Fatah"

#unnest again
thai <- thai %>%
  mutate(tgt.groups=strsplit(tgt.groups,"; ")) %>%
  unnest(tgt.groups)

## remove events that where target isn't dissident
thai <- subset(thai, alt.tgt %in% alt.src)

#remove events w/ self
thai <- subset(thai, alt.src!=alt.tgt)

#remove events w/ unattributed
thai <- subset(thai, alt.src!="Unattributed" & alt.tgt!="Unattributed")

#remove duplicate events
thai <- subset(thai, duplicated(subset(thai,select=c(alt.tgt,alt.src,date,cameo)))==F)

#write
write_csv(thai,"../Network_Creation/edges/palestine_edge_events.csv")

rm(thai)
